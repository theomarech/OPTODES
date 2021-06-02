#####################################################################################################
###########
###########                    SCRIPT RELATION KEDDY OXYGENE
###########
#####################################################################################################
### besoin de df_oxy_keddy dans le script "script_optodes_analyses_descriptives"
#1 -  faire les plots des différents graphes filtrer >15 ou >20
#2 remplacer par des NA les zones vides
#3 couper en groupes continue les séries par rapport aux zone NA ==> elles seront à traiter indépendamment les unes des autres
#4 pour chaque groupes faire ccf ==> noter les lag max pour chacun + lag intermédiaires
#5 essayer de trouver un modèle qui colle au donner =+> regarder autocorrélation dans les résidus si besoin AR1 ou 2
#6 diagnostique des résidus
#7 garder le meilleur modèle
#8 comparer les modèles (ANOVA)
#9 graphiques de régression si besoin
fun_aic_arima <- function(df_train, xreg_train) {
        df_var <-  xreg_train
        vec_var <- colnames(df_var)
        df_aic <- data.frame(mod = rep(NA, length(vec_var)), AIC = NA)
        for (i in 1:(length(vec_var) + 1)) {
                df_aic[i, 1] <- paste(vec_var, collapse = ' ')
                df_aic[i, 2] <-
                        auto.arima(df_train,
                                   xreg = df_var %>% as.matrix,
                                   allowdrift = TRUE, 
                                   allowmean = TRUE, 
                                   seasonal=TRUE,
                                   approximation = TRUE)$aic
                vec_var <- vec_var[-length(df_var)]
                df_var <- dplyr::select_at(df_var %>% as_tibble(), vec_var)
        }
        best_AIC <- df_aic %>% slice(-1) %>% .[which.min(.$AIC), "AIC"]
        xreg_pred <- df_aic %>%
                slice(-1) %>%
                .[which.min(.$AIC), "mod"] %>%
                str_split(" ") %>% 
                .[[1]]
        
        model_choose <- auto.arima(df_train,
                                   xreg = xreg_train %>% as_tibble() %>% dplyr::select_if(colnames(.) %in% xreg_pred) %>% as.matrix,
                                   allowdrift = TRUE, 
                                   allowmean = TRUE, 
                                   seasonal=TRUE,
                                   approximation = FALSE)
        
        return(list(model_choose,best_AIC, xreg_pred))
}


fun_arima <-function(df){
        df1 <- df %>% 
                mutate(data_log = map(data, ~mutate_at(.,vars(matches("keddy")),~log(.+1))),
                       vec_lag_keddy = map(data_log, ~fun_ccf(.)),
                       df_lag_keddy = map(data_log, ~fun_ccf_lag(.)),
                       df_lag = map2(data_log,vec_lag_keddy, ~fun_df_lag(.x,lagged="keddy",vec_lag=.y) %>% na.omit()),
                       df_cor_lag = map2(df_lag_keddy,vec_lag_keddy,~filter(.x,lag %in% .y) %>% arrange(desc(cor))),
                       indice_test = map(data_log, ~1:round(0.85*nrow(.))),
                       data_train =map2(df_lag,indice_test,~slice(.x,.y)),
                       data_test = map2(.x = df_lag,.y = indice_test,~slice(.x,-.y)),
                       xreg_train = map(data_train, ~dplyr::select_at(.,vars(matches("keddy"))) %>% as.matrix()),
                       xreg_test = map(data_test, ~dplyr::select_at(.,vars(matches("keddy"))) %>% as.matrix())) %>% 
                dplyr::select(-c(indice,data,data_log)) %>% 
                mutate(arima_base = map(data_train,~pull(.,conc_od) %>% auto.arima(allowdrift = TRUE, 
                                                                                   allowmean = TRUE, 
                                                                                   seasonal=TRUE,
                                                                                   approximation = FALSE)),
                       arima_keddy = map2(data_train,xreg_train, ~fun_aic_arima(df_train = pull(.x,conc_od),
                                                                                xreg_train =.y)),
                       arima_base_pred =  map2(arima_base,data_test,
                                               ~forecast::forecast(.x,h=nrow(.y)) %>%  as_tibble() %>% pull(1)),
                       arima_keddy_pred = map2(arima_keddy,xreg_test,
                                               ~forecast::forecast(.x[[1]],
                                                                   h = nrow(.y),
                                                                   xreg =.y %>%
                                                                           as_tibble() %>%
                                                                           dplyr::select_if(colnames(.) %in% .x[[3]]) %>% 
                                                                           as.matrix) %>%
                                                       as_tibble() %>%
                                                       pull(1)),
                       rmse_mod_keddy = map2_dbl(arima_keddy_pred,data_test,~RMSE(.x,pull(.y,1))),
                       rmse_mod_base = map2_dbl(arima_base_pred,data_test,~RMSE(.x,pull(.y,1))),
                       mae_mod_keddy = map2_dbl(arima_keddy_pred,data_test,~MAE(.x,pull(.y,1))),
                       mae_mod_base = map2_dbl(arima_base_pred,data_test,~MAE(.x,pull(.y,1))),
                       aic_base = map_dbl(arima_base,~.$aic),
                       aic_keddy = map_dbl(arima_keddy, ~.[[1]]$aic),
                       bic_base = map_dbl(arima_base,~.$bic),
                       bic_keddy = map_dbl(arima_keddy, ~.[[1]]$bic),
                       mod_complet = map_chr(arima_keddy, ~paste(.[[3]],collapse=" "))
                ) %>% return()
}


fun_split_time_series_NA <- function(df, var) {
        facteur <- expand.grid(letters[1:26], seq(1:3000))
        facteur <- str_c(facteur$Var1, facteur$Var2)
        indice_fact <- 1
        vec <- c()
        for (i in 1:nrow(df)) {
                if (!is.na(df[i, var])) {
                        vec[i] <- facteur[indice_fact]
                        indice_fact <- indice_fact
                } else{
                        indice_fact <- indice_fact + 1
                        vec[i] <- indice_fact 
                }
        }
        df_bind_vec <- bind_cols(indice = vec, df) %>%
                mutate(indice = as.factor(indice)) %>%
                group_by(indice) %>%
                nest() %>%
                mutate(nombre = map_dbl(data,  ~ nrow(.))) %>%
                filter(nombre > 200) %>%
                mutate(nombre = as.factor(nombre)) %>%
                unnest()
        df_bind_vec <- df_bind_vec %>% group_by(site,indice) %>% nest() %>% mutate(na = map_lgl(data,~pull(.,conc_od) %>% anyNA()))
        return(df_bind_vec)
}
fun_split_time_series <- function(df, var, seuil = 15) {
        facteur <- expand.grid(letters[1:26], seq(1:3000))
        facteur <- str_c(facteur$Var1, facteur$Var2)
        indice_fact <- 1
        vec <- c()
        for (i in 1:nrow(df)) {
                if (df[i, var] %>% as.numeric() >= seuil) {
                        vec[i] <- facteur[indice_fact]
                        indice_fact <- indice_fact
                } else{
                        indice_fact <- indice_fact + 1
                        vec[i] <- indice_fact
                }
        }
        df_bind_vec <- bind_cols(indice = vec, df) %>%
                filter(temp >= seuil) %>%
                mutate(indice = as.factor(indice)) %>%
                group_by(indice) %>%
                nest() %>%
                mutate(nombre = map_dbl(data,  ~ nrow(.))) %>%
                filter(nombre > 200) %>%
                mutate(nombre = as.factor(nombre)) %>%
                unnest()
        
        return(df_bind_vec)
}

fun_select_lag <- function(df, n.lag = 6) {
        # df <- df %>% filter(lag <= 0)
        lag_choose <- list()
        for (i in 1:n.lag) {
                lag_choose[[i]] <- df[which.max(abs(df$cor)),]
                df <- df[-which.max(abs(df$cor)),]
        }
        lag_choose = bind_rows(lag_choose)
        return(lag_choose)
}

fun_df_lag <- function(df, lagged = "keddy",vec_lag, var = c(1,2)) {
        if (var > 0) {
                df_lag <- matrix(ncol = length(vec_lag) + 1+ length(var),
                                 nrow = nrow(df))
                df_lag[, 1] <- df$conc_od
                vec_names <-
                        c("conc_od",
                          paste0(rep("conc_od_lag_t",length(var)),1:length(var)),
                          paste0(rep(
                                  lagged, length(vec_lag)
                          ), abs(vec_lag)))
                for(i in 2:(1+length(var))){
                        df_lag[, i] <- dplyr::lag(df$conc_od, abs(var[i - 1]))
                }
                for (i in (2+length(var)):(length(vec_lag) + 1+length(var))) {
                        df_lag[, i] <- dplyr::lag(df %>% pull(lagged), abs(vec_lag[i - (1+length(var))]))
                }
        } else{
                df_lag <- matrix(ncol = length(vec_lag) + 1,
                                 nrow = nrow(df))
                df_lag[, 1] <- df$conc_od
                vec_names <-
                        c("conc_od", paste0(rep(
                                lagged, length(vec_lag)
                        ), abs(vec_lag)))
                for (i in 2:(length(vec_lag) + 1)) {
                        df_lag[, i] <- dplyr::lag(df %>% pull(lagged), abs(vec_lag[i - 1]))
                }
        }
        colnames(df_lag) <- vec_names
        df_lag <- as_tibble(df_lag) 
        return(df_lag)
}


fun_ccf_vec_lag <- function(df){
        ccf(log(df$keddy+1), df$conc_od,lag.max = 30)[[1]] %>% 
                as.data.frame()%>% 
                bind_cols(ccf(log(df$keddy+1), df$conc_od,lag.max = 30)[[4]] %>% as.data.frame()) %>% 
                setnames(c("V1","V11"),c("cor","lag")) %>% 
                fun_select_lag( n.lag = 6) %>% pull(lag)
}

fun_ccf_lag <- function(df){
        ccf(log(df$keddy+1), df$conc_od,lag.max = 30)[[1]] %>% 
                as.data.frame()%>% 
                bind_cols(ccf(log(df$keddy+1), df$conc_od,lag.max = 30)[[4]] %>% as.data.frame()) %>% 
                setnames(c("V1","V11"),c("cor","lag")) 
}


###################################### Fin fonction

df_parentis$keddy <- na_kalman(df_parentis$keddy)
df_parentis <- df_parentis %>% mutate(lac = "lacanau") %>% dplyr::select(lac,site,date,conc_od,keddy,temp)
df_lacanau <- df_oxy_keddy %>%  mutate(lac = "parentis")%>% dplyr::select(lac,site,date,conc_od,keddy,temp)
df_parentis_lacanau <- bind_rows(df_lacanau,df_parentis)

data_splite <- df_parentis_lacanau %>%
        na.omit() %>%
        group_by(site,lac) %>%
        nest() %>%
        mutate(
                data_20 = map(data, ~ fun_split_time_series(., "temp", seuil = 20)),
                data_15 = map(data, ~ fun_split_time_series(., "temp", seuil = 15))
        )
df_final_filtre <- data_splite %>%
        unnest(data_20) %>% 
        group_by(lac,site,indice) %>% 
        nest() %>% 
        mutate(seuil = 20) %>% bind_rows(data_splite %>%
        unnest(data_15) %>% 
        group_by(lac,site,indice) %>% 
        nest() %>% 
        mutate(seuil = 15)) %>% 
        mutate(taille_serie = map_dbl(data, ~nrow(.)),
               date_range = map_chr(data, ~paste(min(.$date)%>% lubridate::date() %>%  format("%A %d-%b. %Y"),
                                                 "au",
                                                 max(.$date)%>% lubridate::date() %>%  format("%A %d-%b. %Y")
               ))) %>%
        ungroup() 


df_final_filtre %>% filter(lac=="lacanau") %>% arrange(taille_serie)

df_final_grand <- fun_split_time_series_NA(df_oxy_keddy,"conc_od") %>% 
        ungroup() %>% 
        mutate(lac = "lacanau") %>% 
        bind_rows(fun_split_time_series_NA(df_parentis,"conc_od") %>%
                          ungroup() %>% 
                          mutate(lac = "parentis")
                  )%>%
        mutate(taille_serie = map_dbl(data, ~nrow(.)),
               date_range = map_chr(data, ~paste(min(.$date)%>% lubridate::date() %>%  format("%A %d-%b. %Y"),
                                                 "au",
                                                 max(.$date)%>% lubridate::date() %>%  format("%A %d-%b. %Y")
               ))) %>%
        ungroup() %>% mutate(seuil=0) %>% dplyr::select(-na)
                        

arima_grand <- fun_arima(bind_rows(df_final_grand,df_final_filtre))


tableau_arima <-arima_grand %>%
        mutate(arima_base_order = map_chr(arima_base, ~arimaorder(.) %>% 
                                                  as.character() %>% 
                                                  paste0(c("p=","d=","q="),.,collapse = "",sep=" ")),
               arima_keddy_order = map_chr(arima_keddy, ~arimaorder(.[[1]]) %>% 
                                                   as.character() %>%
                                                   paste0(c("p=","d=","q="),.,collapse = "",sep=" ")),
               comparaison_mod_critere = map2_chr(arima_base_order,arima_keddy_order,
                                              ~ifelse(str_extract(.x,"d=[:digit:]")==str_extract(.y,"d=[:digit:]"),
                                                      "oui","non"),
                                              )
               )%>% 
        dplyr::select(lac,site,seuil,date_range,taille_serie,
                      arima_base_order,arima_keddy_order,comparaison_mod_critere,
                               rmse_mod_base,rmse_mod_keddy,
                               mae_mod_base,mae_mod_keddy,
                               aic_base,aic_keddy,
                               bic_base,bic_keddy)%>%
        mutate(rapport_rmse = rmse_mod_base/rmse_mod_keddy,
               rapport_mae = mae_mod_base/mae_mod_keddy,
               rapport_aic = aic_base/aic_keddy,
               rapport_bic = bic_base/bic_keddy,
               conditions = case_when(site %in% c("AbNu1","AbNu2")~"nu_ab",
                                      site %in% c("AbVe1","AbVe2")~"ve_ab",
                                      site %in% c("ExNu1","ExNu2")~"nu_ex",
                                      site %in% c("ExVe1","ExVe2")~"ve_ex",
                                      TRUE ~ site),
               aic_base = ifelse(comparaison_mod_critere=="oui",aic_base,NA),
               aic_keddy = ifelse(comparaison_mod_critere=="oui",aic_keddy,NA)) 
tableau_arima20 <- tableau_arima %>% filter(seuil == 20) %>% 
        dplyr::select(lac,site,date_range,taille_serie,
                      arima_base_order,arima_keddy_order,
                      comparaison_mod_critere,rapport_rmse,rapport_mae) %>%
        arrange(desc(rapport_rmse)) %>% 
        mutate_at(vars(matches("rapport")),round,digits=2)%>% group_by(lac,site) %>% nest()%>%
        mutate(ponder_rmse = map(data, ~.$rapport_rmse*.$taille_serie),
               ponder_mae = map(data, ~.$rapport_mae*.$taille_serie),
               rapport_rmse=map2_dbl(data,ponder_rmse,~sum(.y)/sum(.x$taille_serie)),
               rapport_mae=map2_dbl(data,ponder_mae,~sum(.y)/sum(.x$taille_serie))) %>% 
        dplyr::select(lac,site,rapport_rmse,rapport_mae) %>% 
        mutate(seuil="20")
tableau_arima15 <- tableau_arima %>% filter(seuil == 15) %>% 
        dplyr::select(lac,site,date_range,taille_serie,
                      arima_base_order,arima_keddy_order,
                      comparaison_mod_critere,rapport_rmse,rapport_mae) %>%
        arrange(desc(rapport_rmse)) %>% 
        mutate_at(vars(matches("rapport")),round,digits=2)%>% group_by(lac,site) %>% nest()%>%
        mutate(ponder_rmse = map(data, ~.$rapport_rmse*.$taille_serie),
               ponder_mae = map(data, ~.$rapport_mae*.$taille_serie),
               rapport_rmse=map2_dbl(data,ponder_rmse,~sum(.y)/sum(.x$taille_serie)),
               rapport_mae=map2_dbl(data,ponder_mae,~sum(.y)/sum(.x$taille_serie))) %>% 
        dplyr::select(lac,site,rapport_rmse,rapport_mae) %>% mutate(seuil = 15)
tableau_arima0 <- tableau_arima %>% filter(seuil == 0) %>% 
        dplyr::select(lac,site,date_range,taille_serie,
                      arima_base_order,arima_keddy_order,
                      comparaison_mod_critere,rapport_rmse,rapport_mae) %>%
        arrange(desc(rapport_rmse)) %>% 
 group_by(lac,site) %>% nest()%>%
        mutate(ponder_rmse = map(data, ~.$rapport_rmse*.$taille_serie),
               ponder_mae = map(data, ~.$rapport_mae*.$taille_serie),
               rapport_rmse=map2_dbl(data,ponder_rmse,~sum(.y)/sum(.x$taille_serie)),
               rapport_mae=map2_dbl(data,ponder_mae,~sum(.y)/sum(.x$taille_serie))) %>% 
        dplyr::select(lac,site,rapport_rmse,rapport_mae) %>% mutate(seuil=0)     %>%    mutate_at(vars(matches("rapport")),round,digits=3)
tableau_arima20 %>% View()
tableau_arima20 %>%ungroup() %>% arrange(lac,site)%>%  write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_arima_20degres.csv",row.names=FALSE)
tableau_arima15 %>%ungroup() %>% arrange(lac,site)%>%  write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_arima_15degres.csv",row.names=FALSE)
tableau_arima0 %>%ungroup() %>% arrange(lac,site)%>%  write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_arima_0degres.csv",row.names=FALSE)
a <- tableau_arima20 %>% group_by(lac,site) %>% nest()%>%
        mutate(ponder_rmse = map(data, ~.$rapport_rmse*.$taille_serie),
               ponder_mae = map(data, ~.$rapport_mae*.$taille_serie),
               rapport_rmse=map2_dbl(data,ponder_rmse,~sum(.y)/sum(.x$taille_serie)),
               rapport_mae=map2_dbl(data,ponder_mae,~sum(.y)/sum(.x$taille_serie)))
##### CCF

tab_lag <- arima_grand %>%  mutate( tableau_lag = 
                                  pmap(list(lac,site,df_cor_lag,taille_serie),
                                       ~ ..3 %>% mutate(lag = as.character(lag),
                                                        cor = round(cor,digits= 2 )) %>% 
                                               dplyr::select(lag,cor) %>% 
                                               t() %>%
                                               as.data.frame %>% 
                                               setnames(colnames(.),paste("dec",1:6,sep="_")) %>% 
                                               mutate(lac = ..1, 
                                                      site = ..2,
                                                      taille_serie=..4,
                                                      var= c("dec","cor")))
                          )
tabl_lag_20 <- tab_lag%>% filter(seuil ==20) %>% pull(tableau_lag) %>% bind_rows() %>% arrange(lac, site, desc(taille_serie)) %>%  
        write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_ccf_20degres.csv",row.names=FALSE)
tabl_lag_15 <- tab_lag%>% filter(seuil ==15) %>% pull(tableau_lag) %>% bind_rows() %>% 
        write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_ccf_15degres.csv",row.names=FALSE)
tabl_lag_0 <- tab_lag%>% filter(seuil ==0) %>% pull(tableau_lag) %>% bind_rows() %>% 
        write.csv(file="/home/theo/Bureau/Vincent/figures/tableau_final_ccf_0degres.csv",row.names=FALSE)




