rm(list = ls())
source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")
map_dbl(data_2,nrow)

CCM (
  liste = data_2,
  scale_var = TRUE,
  surrogate = TRUE,
  delay_ccm = -45:45,
  num_surr = 100,
  taille_lib = 5000,
  name = "CCM_test_surr_avril_octobre_ebisu",
  var_ccm = c("mean_temp", "mean_conc_ox", "expo"),
  librairies_ccm = c(10, 100, 500, 1500, 5000),
  tableau_final = data.frame(
    variables = c("mean_temp", "mean_conc_ox", "expo"),
    AbNu1 = c(, 5, 6),
    AbNu2 = c(2, 4, 7),
    AbVe1 = c(1, 4, 10),
    AbVe2 = c(2, 3, 7),
    ExNu1 = c(2, 3, 6),
    ExNu2 = c(2, 3, 5),
    ExVe1 = c(2, 2, 5),
    ExVe2 = c(2, 4, 7)
  )
)

CCM (
  liste = data_2,
  scale_var = TRUE,
  delay_ccm = -45:45,
  surrogate = TRUE,
  num_surr = 100,
  taille_lib = 5000,
  name = "CCM_avril_octobre_tp_surro_ebi_cor",
  var_ccm = c("mean_temp", "mean_conc_ox", "expo"),
  librairies_ccm = c(10, 100, 500, 1500, 5000),
  tableau_final = data.frame(
    variables = c("mean_temp", "mean_conc_ox", "expo"),
    AbNu1 = c(3, 5, 10),
    AbNu2 = c(2, 4, 7),
    AbVe1 = c(2, 4, 10),
    AbVe2 = c(2, 3, 7),
    ExNu1 = c(2, 3, 6),
    ExNu2 = c(2, 5, 3),
    ExVe1 = c(8, 2, 5),
    ExVe2 = c(2, 4, 7)
  )
)

CCM(
  liste = data_test,
  scale_var = TRUE,
  delay_ccm = -3:3,
  num_surr = 10,
  surrogate = TRUE,
  taille_lib = 50,
  name = "CCM_test_surr3",
  var_ccm = c("mean_temp", "mean_conc_ox", "expo"),
  librairies_ccm = seq(10, 100, by = 10),
  tableau_final = data.frame(
    variables = c("mean_temp", "mean_conc_ox", "expo"),
    AbNu1 = c(2, 8, 4),
    AbNu2 = c(4, 7, 3),
    AbVe1 = c(2, 4, 3),
    AbVe2 = c(2, 7, 3),
    ExNu1 = c(2, 6, 11),
    ExNu2 = c(2, 6, 9),
    ExVe1 = c(4, 4, 18),
    ExVe2 = c(2, 3, 9)
  )
)

i = 1

liste = data_2
scale_var = TRUE
delay_ccm = -3:3
num_surr = 10
surrogate = TRUE
nb_sample = 50
taille_lib = 50
name = "CCM_test_surr10"
var_ccm = c("mean_temp", "mean_conc_ox", "expo")
librairies_ccm = seq(10, 100, by = 10)
tableau_final = data.frame(
  variables = c("mean_temp", "mean_conc_ox", "expo"),
  AbNu1 = c(2, 8, 4),
  AbNu2 = c(4, 7, 3),
  AbVe1 = c(2, 4, 3),
  AbVe2 = c(2, 7, 3),
  ExNu1 = c(2, 6, 11),
  ExNu2 = c(2, 6, 9),
  ExVe1 = c(4, 4, 18),
  ExVe2 = c(2, 3, 9)
)







CCM <- function(liste,
                scale_var = TRUE,
                delay_ccm,
                name = "CCM",
                surrogate = TRUE,
                var_ccm,
                nb_sample = 50,
                num_surr,
                taille_lib,
                librairies_ccm = seq(10, 80, by = 10),
                tableau_final = data.frame(
                  variables = c("mean_temp", "mean_conc_ox", "expo"),
                  AbNu1 = c(1, 2, 6),
                  AbNu2 = c(1, 3, 7),
                  AbVe1 = c(1, 4, 5),
                  AbVe2 = c(2, 4, 2),
                  Exnu1 = c(1, 1, 7),
                  ExNu2 = c(1, 3, 7),
                  ExVe1 = c(2, 4, 7),
                  ExVe2 = c(2, 3, 5)
                )) {
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/", name))
  if (scale_var == TRUE) {
    for (h in 1:length(liste)) {
      liste[[h]][, 2:length(liste[[h]])] <-
        as.data.frame(scale(liste[[h]][, 2:length(liste[[h]])]))
    }
  } else {
    liste <- liste
  }
  
  nombre_var <- length(liste[[1]]) - 1
  
  
  list_final <- list()
  
  
  indice <- 1:nombre_var
  
  
  
  
  ##### fonction qui créée les combinaisons de variables à tester dans le CCM
  combinaison <- function(vec) {
    n <- vec %>%
      expand.grid(vec) %>%
      filter(Var1 != Var2)
    
    dft <-
      data.frame(Var1 = rep(0, length(vec) ^ 2 - length(vec)),
                 Var2 = rep(0, length(vec) ^ 2 - length(vec)))
    
    for (i in 1:nrow(n)) {
      ordre <- as.data.frame(t(n[i,]))
      
      dft[i,] <- sort(ordre[, 1])
    }
    
    combinaison <- unique(dft)
    
    return(combinaison)
  }
  
  combinaisons <-
    combinaison(var_ccm) # combinaisons contient les combinaisons de variables à teste"r
  
  
  tableau_final <- tableau_final
  
  for (i in 1:length(liste)) {
    nom_condition <-
      liste[[i]][i, 1] # nom de la condition analysée i.e un tableau de la liste (e.g AbVe)
    
    dir.create(
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/",
        name,
        "/",
        nom_condition,
        "/"
      )
    )
    
    
    for (k in 1:nrow(combinaisons)) {
      nombre_moyennes <- length(librairies_ccm)
      
      nom_var_un <- combinaisons[k, 1]
      nom_var_deux <- combinaisons[k, 2]
      
      E_ccm_1 <-
        as.numeric(tableau_final %>% filter(variables == combinaisons[k, 1]) %>% select(nom_condition))
      E_ccm_2 <-
        as.numeric(tableau_final %>% filter(variables == combinaisons[k, 2]) %>% select(nom_condition))
      
      
      delay_cor <-
        data.frame(
          delay = delay_ccm,
          ccm_1_rho = rep(0, length(delay_ccm)),
          ccm_1_mae = rep(0, length(delay_ccm)),
          ccm_1_rmse = rep(0, length(delay_ccm)),
          ccm_1_lambda = rep(0, length(delay_ccm)),
          ccm_2_rho = rep(0, length(delay_ccm)),
          ccm_2_mae = rep(0, length(delay_ccm)),
          ccm_2_rmse = rep(0, length(delay_ccm)),
          ccm_2_lambda = rep(0, length(delay_ccm))
        )
      
      for (l in 1:length(delay_ccm)) {
        # fonction sur les tp
        ccm_1 <-
          ccm(
            liste[[i]],
            E = E_ccm_1,
            lib_column = combinaisons[k, 1],
            target_column = combinaisons[k, 2],
            lib_sizes = NROW(liste[[i]]),
            num_samples = nb_sample,
            random_libs = TRUE,
            replace = TRUE,
            tp = delay_ccm[l]
          )
        
        ccm_2 <-
          ccm(
            liste[[i]],
            E = E_ccm_2,
            lib_column = combinaisons[k, 2],
            target_column = combinaisons[k, 1],
            lib_sizes = NROW(liste[[i]]),
            num_samples = nb_sample,
            random_libs = TRUE,
            replace = TRUE,
            tp = delay_ccm[l]
          )
        ## Tableau calcule lambda et rho
        delay_cor[l, 2] <- mean(ccm_1$rho)
        delay_cor[l, 3] <- mean(ccm_1$mae)
        delay_cor[l, 4] <- mean(ccm_1$rmse)
        delay_cor[l, 5] <-
          (mean(ccm_1$rmse) + abs(mean(ccm_1$mae))) / abs(mean(ccm_1$rho))
        delay_cor[l, 6] <- mean(ccm_2$rho)
        delay_cor[l, 7] <- mean(ccm_2$mae)
        delay_cor[l, 8] <- mean(ccm_2$rmse)
        delay_cor[l, 9] <-
          (mean(ccm_2$rmse) + abs(mean(ccm_2$mae))) / abs(mean(ccm_2$rho))
      }
      
      
      ccm_1 <-
        ccm(
          liste[[i]],
          E = E_ccm_1,
          lib_column = combinaisons[k, 1],
          target_column = combinaisons[k, 2],
          lib_sizes = librairies_ccm,
          num_samples = nb_sample,
          random_libs = TRUE,
          replace = TRUE,
          tp = delay_cor[which.min(delay_cor[, 5]), 1]
        )
      
      ccm_2 <-
        ccm(
          liste[[i]],
          E = E_ccm_2,
          lib_column = combinaisons[k, 2],
          target_column = combinaisons[k, 1],
          lib_sizes = librairies_ccm,
          num_samples = nb_sample,
          random_libs = TRUE,
          replace = TRUE,
          tp = delay_cor[which.min(delay_cor[, 9]), 1]
        )
      
      
      ############### SURROGATE SEASONAL ##############
      if (surrogate == TRUE) {
        surr_var1 <-
          make_surrogate_seasonal(na.omit(liste[[i]][, combinaisons[k, 1]]),
                                  T_period = 24,
                                  num_surr = num_surr)
        surr_var2 <-
          make_surrogate_seasonal(na.omit(liste[[i]][, combinaisons[k, 2]]),
                                  T_period = 24,
                                  num_surr = num_surr)
        ccm_rho_surr <-
          data.frame(var1 = numeric(num_surr),
                     var2 = numeric(num_surr))

        for (m in 1:num_surr) {
          ccm_rho_surr$var1[m] <-
            ccm(
              cbind(liste[[i]][, combinaisons[k, 1]], surr_var2[, m]),
              E = E_ccm_1,
              lib_column = 1,
              target_column = 2,
              lib_sizes = taille_lib,
              replace = TRUE
            )$rho
          
          ccm_rho_surr$var2[m] <-
            ccm(
              cbind(liste[[i]][, combinaisons[k, 2]], surr_var1[, m]),
              E = E_ccm_2,
              lib_column = 1,
              target_column = 2,
              lib_sizes = taille_lib,
              replace = TRUE
            )$rho

        }
        
        sur_ccm1 <-
          mean(ccm_rho_surr$var1) + 2 * sd(ccm_rho_surr$var1)
        sur_ccm2 <-
          mean(ccm_rho_surr$var2) + 2 * sd(ccm_rho_surr$var2)

      }
      ############### FIN  SURROGATE ##############
      
      
      
      
      
      a_xmap_t_means <-
        ccm_1[, c("lib_size", "rho")] %>% mutate(variable = rep(combinaisons[k, 1], nrow(ccm_1)))
      
      t_xmap_a_means <-
        ccm_2[, c("lib_size", "rho")] %>% mutate(variable = rep(combinaisons[k, 2], nrow(ccm_2)))
      
      tableau_ccm <- bind_rows(a_xmap_t_means, t_xmap_a_means)
      
      tableau_delay <- delay_cor %>%
        select(1, 2, 6) %>%
        gather("ccm", "cor", 2:3)
      tableau_lambda <- delay_cor %>%
        select(1, 5, 9) %>%
        gather("ccm", "lambda", 2:3)
      
      
      
      
      ############### PLOT ##############
      
      plot_delay <-
        ggplot(tableau_delay, aes(delay, cor, colour = ccm)) +
        geom_line(size = rel(1)) +
        geom_point(size = rel(2)) +
        ylab("Corrélation") +
        xlab("Lambda") +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = rel(0.9),
            color = "grey30"
          ),
          panel.background = element_rect(fill = NA),
          axis.text = element_text(size = rel(1), color = "grey30"),
          axis.text.x = element_text(vjust = 1, hjust = 1),
          axis.ticks = element_line(size = rel(1), color = "grey30"),
          axis.title = element_text(
            size = rel(1),
            hjust = 0.5,
            vjust = 4,
            colour = "grey30"
          )
        ) +
        scale_colour_discrete(labels = c(
          paste(combinaisons[k, 2], "cause", combinaisons[k, 1]),
          paste(combinaisons[k, 1], "cause", combinaisons[k, 2])
        )) +
        scale_colour_discrete(labels = c(
          paste(combinaisons[k, 2], "cause", combinaisons[k, 1]),
          paste(combinaisons[k, 1], "cause", combinaisons[k, 2])
        )) +
        geom_vline(
          xintercept = delay_cor[which.max(delay_cor[, 2]), 1],
          #############
          linetype = "longdash",
          col = "#F8766D",
          alpha = 0.5
        ) +
        geom_vline(
          xintercept = c(delay_cor[which.max(delay_cor[, 6]), 1]),
          #############
          linetype = "longdash",
          col = "#00BFC4",
          alpha = 0.5
        )
      
      plot_lambda <-
        ggplot(tableau_lambda, aes(delay, lambda, colour = ccm)) +
        geom_line(size = rel(1)) +
        geom_point(size = rel(2)) +
        ylab("f(lambda) \n (i.e. rmse + mae / rho") +
        xlab("Lambda") +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = rel(0.9),
            color = "grey30"
          ),
          panel.background = element_rect(fill = NA),
          axis.text = element_text(size = rel(1), color = "grey30"),
          axis.text.x = element_text(vjust = 1, hjust = 1),
          axis.ticks = element_line(size = rel(1), color = "grey30"),
          axis.title = element_text(
            size = rel(1),
            hjust = 0.5,
            vjust = 4,
            colour = "grey30"
          )
        ) +
        scale_colour_discrete(labels = c(
          paste(combinaisons[k, 2], "cause", combinaisons[k, 1]),
          paste(combinaisons[k, 1], "cause", combinaisons[k, 2])
        )) +
        geom_vline(
          xintercept = delay_cor[which.min(delay_cor[, 5]), 1],
          #############
          linetype = "longdash",
          col = "#F8766D",
          alpha = 0.5
        ) +
        geom_vline(
          xintercept = delay_cor[which.min(delay_cor[, 9]), 1],
          #############
          linetype = "longdash",
          col = "#00BFC4",
          alpha = 0.5
        )
      
      if (surrogate == TRUE) {
        plot_ccm <-
          ggplot(data = tableau_ccm,
                 aes(
                   x = lib_size,
                   y = rho,
                   colour = variable,
                   fill = variable
                 )) +
          stat_summary(
            geom = "ribbon",
            fun.data = mean_cl_normal,
            alpha = 0.05,
            fun.args = list(conf.int = 0.99)
          ) +
          stat_summary(geom = "line",
                       fun.y = mean,
                       linetype = "dashed") +
          stat_summary(geom = "point", fun.y = mean) +
          ylab("Corrélation") +
          xlab("Taille de la librairie") +
          theme(
            axis.line = element_line(
              linetype = 1,
              size = rel(0.9),
              color = "grey30"
            ),
            panel.background = element_rect(fill = NA),
            axis.text = element_text(size = rel(1), color = "grey30"),
            axis.text.x = element_text(vjust = 1, hjust = 1),
            axis.ticks = element_line(size = rel(1), color = "grey30"),
            axis.title = element_text(
              size = rel(1),
              hjust = 0.5,
              vjust = 4,
              colour = "grey30"
            )
          ) +
          scale_colour_discrete(labels = c(
            paste(
              combinaisons[k, 2],
              "cause",
              combinaisons[k, 1],
              "|décalage = ",
              delay_cor[which.min(delay_cor[, 5]), 1]
            ),
            paste(
              combinaisons[k, 1],
              "cause",
              combinaisons[k, 2],
              "|décalage =",
              delay_cor[which.min(delay_cor[, 9]), 1]
            )
          )) + geom_hline(
            yintercept = c(sur_ccm1, sur_ccm2),
            col = c("#F8766D", "#00BFC4"),
            linetype = "longdash"
          )
        
      } else {
        plot_ccm <-
          ggplot(data = tableau_ccm,
                 aes(
                   x = lib_size,
                   y = rho,
                   colour = variable,
                   fill = variable
                 )) +
          stat_summary(
            geom = "ribbon",
            fun.data = mean_cl_normal,
            alpha = 0.05,
            fun.args = list(conf.int = 0.99)
          ) +
          stat_summary(geom = "line",
                       fun.y = mean,
                       linetype = "dashed") +
          stat_summary(geom = "point", fun.y = mean) +
          ylab("Corrélation") +
          xlab("Taille de la librairie") +
          theme(
            axis.line = element_line(
              linetype = 1,
              size = rel(0.9),
              color = "grey30"
            ),
            panel.background = element_rect(fill = NA),
            axis.text = element_text(size = rel(1), color = "grey30"),
            axis.text.x = element_text(vjust = 1, hjust = 1),
            axis.ticks = element_line(size = rel(1), color = "grey30"),
            axis.title = element_text(
              size = rel(1),
              hjust = 0.5,
              vjust = 4,
              colour = "grey30"
            )
          ) +
          scale_colour_discrete(labels = c(
            paste(
              combinaisons[k, 2],
              "cause",
              combinaisons[k, 1],
              "|décalage = ",
              delay_cor[which.min(delay_cor[, 5]), 1]
            ),
            paste(
              combinaisons[k, 1],
              "cause",
              combinaisons[k, 2],
              "|décalage =",
              delay_cor[which.min(delay_cor[, 9]), 1]
            )
          ))
      }
      ############### FIN PLOT ##############
      
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/",
          name,
          "/",
          nom_condition,
          "/CCM_",
          paste0(combinaisons[k, 1], "_", combinaisons[k, 2]),
          ".pdf"
        )
      
      pdf(filename)
      grid.arrange(plot_delay, plot_lambda, plot_ccm)
      dev.off()
    } ### fermeture de la boucle sur les ccm
    print(i)
  } ### fermeture de la boucle sur les conditions
  
  return(list_final)
} ### fermeture de la fonction
