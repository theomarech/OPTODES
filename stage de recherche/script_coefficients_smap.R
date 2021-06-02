#####################################################################################################
####                                                                                            #####
####                Interaction entre variables causales :   COEFFICIENTS S-MAP                 #####
####                                                                                            #####      
#####################################################################################################
source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")

## Période avril- octobre => data_2

  
##################### EXVE1                            ##############################################
#####################################################################################################


exve1 <- data_2[[3]]
time_exve1 <-
  liste_conditions_période_croissance[[3]] %>% select(date)
## ox : E = 4
## temp : E = 6
## Keddy : E = 8

##### variables causales : température/oxygène
exve1_ox <- smap_coef(
  df = data_2[[3]],
  time_exve1 = liste_conditions_période_croissance[[3]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 4,
  variable_causale = c("mean_temp", "expo"),
  nom_condition = "ExVe1"
)


exve2_ox <- smap_coef(
  df = data_2[[7]],
  time_exve1 = liste_conditions_période_croissance[[7]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 4,
  variable_causale = c("mean_temp", "expo"),
  nom_condition = "ExVe2"
)

exnu1_ox <- smap_coef(
  df = data_2[[4]],
  time_exve1 = liste_conditions_période_croissance[[4]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 4,
  variable_causale = c("mean_temp", "expo"),
  nom_condition = "ExNu1"
)

exnu2_ox <- smap_coef(
  df = data_2[[8]],
  time_exve1 = liste_conditions_période_croissance[[8]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 3,
  variable_causale = c("mean_temp", "expo"),
  nom_condition = "ExNu2"
)

abnu2_ox <- smap_coef(
  df = data_2[[6]],
  time_exve1 = liste_conditions_période_croissance[[6]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 4,
  variable_causale = c("mean_temp", "expo"),
  nom_condition = "AbNu2"
)

abnu1_ox <- smap_coef(
  df = data_2[[2]],
  time_exve1 = liste_conditions_période_croissance[[2]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 4,
  variable_causale = c("mean_temp"),
  nom_condition = "AbNu1"
)

abve1_ox <- smap_coef( ## à faire
  df = data_2[[1]],
  time_exve1 = liste_conditions_période_croissance[[1]] %>% select(date),
  variable_cause = "mean_conc_ox",
  E = 3,
  variable_causale = c("mean_temp"),
  nom_condition = "AbVe1"
)


##
abve1_temp <- smap_coef( # theta = 3
  df = data_2[[1]],
  time_exve1 = liste_conditions_période_croissance[[1]] %>% select(date),
  variable_cause = "mean_temp",
  E = 2,
  variable_causale = c("expo"),
  nom_condition = "AbVe1_temp"
)

abve2_temp <- smap_coef( # theta = 2
  df = data_2[[5]],
  time_exve1 = liste_conditions_période_croissance[[5]] %>% select(date),
  variable_cause = "mean_temp",
  E = 2,
  variable_causale = c("expo"),
  nom_condition = "AbVe1_temp"
)

abnu2_temp <- smap_coef( #theta = 3 
  df = data_2[[6]],
  time_exve1 = liste_conditions_période_croissance[[6]] %>% select(date),
  variable_cause = "mean_temp",
  E = 2,
  variable_causale = c("expo"),
  nom_condition = "AbNu2_temp"
)

exve1_temp <- smap_coef( #theta = 0
  df = data_2[[3]],
  time_exve1 = liste_conditions_période_croissance[[3]] %>% select(date),
  variable_cause = "mean_temp",
  E = 6,
  variable_causale = c("expo"),
  nom_condition = "ExVe1_temp"
)



liste_smap_tot <- list(exve1_ox,
                     exve2_ox,
                     exnu1_ox,
                     exnu2_ox,
                     abnu1_ox,
                     abnu2_ox,
                     abve1_ox,
                     abve1_temp,
                     abve2_temp,
                     abnu2_temp,
                     exve1_temp)
df_smap_tot <- bind_rows(liste_smap_tot)
write.csv(df_smap_tot, file = "C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/0 - Donnees/df_smap_1.csv" )

liste_smap_ox <- list(exve1_ox,
                       exve2_ox,
                       exnu1_ox,
                       exnu2_ox,
                       abnu1_ox,
                       abnu2_ox,
                       abve1_ox)
df_smap_ox <- bind_rows(liste_smap_ox)
write.csv(df_smap_ox, file = "C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/0 - Donnees/smap.csv" )



df_smap_ox <- read.table("smap.csv",sep=",", header= TRUE)
summary(smap_tot)




smap_coef <- function(df = data_2[[3]],
                      time_exve1 = liste_conditions_période_croissance[[3]] %>% select(date),
                      variable_cause = "mean_conc_ox",
                      E = 4,
                      variable_causale = c("mean_temp", "expo"),
                      nom_condition = "ExNu1") {
  indice_1 <-
    (E - length(variable_causale)) + 1  # sélection des coefficients
  indice_2 <-
    (E - length(variable_causale)) + length(variable_causale)
  
  
  if (E > length(variable_causale)) {
    if (length(variable_causale) == 2) {
      data_smap <-
        make_block(df[variable_cause], max_lag = E - length(variable_causale)) %>% # création de la variable décalée seule
        select(-time) %>%
        bind_cols(cbind.data.frame(df[variable_causale[1]], df[variable_causale[2]])) %>%
        scale() %>%
        as.data.frame()
      
      smap_choix <- block_lnlp(
        data_smap,
        lib = c(1, NROW(data_smap)),
        pred = c(1, NROW(data_smap)),
        method = "s-map",
        save_smap_coefficients = TRUE,
        tp = 1,
        theta = 1:10,
        target_column = 1
      )
      
      
      plot(
        smap_choix$theta,
        smap_choix$rho,
        xlab = "Nonlinearity (theta)",
        ylab = "Forecast Skill (rho)",
        type = "l"
      )
      
      cat("Choisir theta ")
      
      selection_E <- readline(prompt = "")
      
      theta_max <- as.numeric(selection_E)
      
      
      
      smap <- block_lnlp(
        data_smap,
        lib = c(1, NROW(data_smap)),
        pred = c(1, NROW(data_smap)),
        method = "s-map",
        save_smap_coefficients = TRUE,
        tp = 1,
        theta = theta_max,
        target_column = 1
      )
      
      
      smapcoef <- smap$smap_coefficients
      smapcoef2 <- smapcoef[[1]][, indice_1:indice_2]
      smapcoef3 <- bind_cols(time_exve1 , smapcoef2) %>%
        filter(date < "2017-04-01 00:00:00") %>%
        mutate(temperature = .[, 2],
               keddy = .[, 3]) %>% select(date, temperature, keddy)
      
      
      
      dataggplot <-
        gather(smapcoef3, key = "Variable", value = "valeur", 2:3) %>%
        mutate(condition = rep(nom_condition, nrow(.)))
      
      plot_smap <- ggplot() +
        geom_rect(
          data = data.frame(
            mois_deb = seq(
              as_datetime('2016-06-01'),
              as_datetime('2016-10-01'),
              by = '2 month'
            ),
            mois_fin = seq(
              as_datetime('2016-07-01'),
              as_datetime('2016-11-01'),
              by = '2 month'
            )
          ),
          aes(
            xmin = mois_deb,
            xmax = mois_fin,
            ymin = -Inf,
            ymax = +Inf
          ),
          alpha = 0.15
        ) +
        geom_line(data = dataggplot, aes(
          x = date,
          y = valeur,
          colour = Variable
        )) +
        facet_grid(Variable ~ .) +
        geom_smooth(data = dataggplot,
                    aes(
                      x = date,
                      y = valeur,
                      colour = Variable
                    ),
                    se = FALSE) +
        ylab("coefficients s-map") +
        xlab(NULL) +
        geom_hline(yintercept = 0,
                   linetype = "longdash",
                   col = "grey30") +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = rel(0.9),
            color = "grey30"
          ),
          strip.background = element_blank(),
          panel.background = element_rect(fill = NA),
          strip.placement = 'outside',
          axis.text = element_text(size = rel(1), color = "grey30"),
          axis.text.x = element_text(vjust = 1, hjust = 1),
          axis.ticks = element_line(size = rel(1), color = "grey30"),
          axis.title = element_text(
            size = rel(1),
            hjust = 0.5,
            vjust = 4,
            colour = "grey30"
          )
        )
      } else {
      data_smap <-
        make_block(df[variable_cause], max_lag = E - length(variable_causale)) %>% # création de la variable décalée seule
        select(-time) %>%
        bind_cols(df[variable_causale[1]]) %>%
        scale() %>%
        as.data.frame()
      
      smap_choix <- block_lnlp(
        data_smap,
        lib = c(1, NROW(data_smap)),
        pred = c(1, NROW(data_smap)),
        method = "s-map",
        save_smap_coefficients = TRUE,
        tp = 1,
        theta = 1:10,
        target_column = 1
      )
      
      
      plot(
        smap_choix$theta,
        smap_choix$rho,
        xlab = "Nonlinearity (theta)",
        ylab = "Forecast Skill (rho)",
        type = "l"
      )
      
      cat("Choisir theta ")
      
      selection_E <- readline(prompt = "")
      
      theta_max <- as.numeric(selection_E)
      
      
      
      smap <- block_lnlp(
        data_smap,
        lib = c(1, NROW(data_smap)),
        pred = c(1, NROW(data_smap)),
        method = "s-map",
        save_smap_coefficients = TRUE,
        tp = 1,
        theta = theta_max,
        target_column = 1
      )
      
      
      smapcoef <- smap$smap_coefficients
      smapcoef2 <- smapcoef[[1]][indice_1:indice_2]
      smapcoef3 <- bind_cols(time_exve1 , smapcoef2) %>%
        filter(date < "2017-04-01 00:00:00") %>%
        mutate(keddy = .[, 2]) %>% select(date, keddy)
      
      
      
      dataggplot <-
        gather(smapcoef3, key = "Variable", value = "valeur", 2) %>%
        mutate(condition = rep(nom_condition, nrow(.)))
      
      plot_smap <- ggplot() +
        geom_rect(
          data = data.frame(
            mois_deb = seq(
              as_datetime('2016-06-01'),
              as_datetime('2016-10-01'),
              by = '2 month'
            ),
            mois_fin = seq(
              as_datetime('2016-07-01'),
              as_datetime('2016-11-01'),
              by = '2 month'
            )
          ),
          aes(
            xmin = mois_deb,
            xmax = mois_fin,
            ymin = -Inf,
            ymax = +Inf
          ),
          alpha = 0.15
        ) +
        geom_line(data = dataggplot, aes(x = date, y = valeur)) +
        geom_smooth(data = dataggplot,
                    aes(x = date, y = valeur),
                    se = FALSE) +
        ylab("coefficients s-map") +
        xlab(NULL) +
        geom_hline(yintercept = 0,
                   linetype = "longdash",
                   col = "grey30") +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = rel(0.9),
            color = "grey30"
          ),
          strip.background = element_blank(),
          panel.background = element_rect(fill = NA),
          strip.placement = 'outside',
          axis.text = element_text(size = rel(1), color = "grey30"),
          axis.text.x = element_text(vjust = 1, hjust = 1),
          axis.ticks = element_line(size = rel(1), color = "grey30"),
          axis.title = element_text(
            size = rel(1),
            hjust = 0.5,
            vjust = 4,
            colour = "grey30"
          )
        )
      
      
    }
    
  } else{
    warning(
      "le nombre de dimension de la variable d'origine est inférieur au nombre de variables causales"
    )
  }
  
  print(plot_smap)
  return(dataggplot)
}
