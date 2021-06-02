setwd("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/0 - Donnees")
########## PACKAGES ######
library(scales)
library(forecast)
library(stR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tictoc)
library(stringr)
library(WaveletComp)
library(doParallel)
library(rEDM)
library(purrr)
library(gridExtra)
library(astsa)
library(reshape2)
library(ranger)
library(pROC)
library(caret)
library(randomForest)
library(formattable)
library(datasets)
library(profvis) ##### profileur de performance
library(microbenchmark) #### estimation du temps d'execution
library(benchmarkme) #### estimation des performances du PC

########## Variables ############

### Conditions : variables qualitatives
# abris/exposé
# herbier/sans herbier

### Répétitions : x 2 par conditions

### Variables mesurées
# DXY : Direction vent quotidien maxi moyenne sur 10 minute (rose des sable 0 à 360)
# FXY : Vitesse vent quotidien maxi moyenne sur 10 minute (m.s-1)
# HXY : Heure vent quotidien maxi moyenne sur 10 minute (m.s-1)
# expo = exposition aux vagues : dépend de la vitesse de la direction du vent et de l'exposition au vent (fetch)
# Oxygène : min, mean, max
# T°C : min, mean, max


########## Ouverture et description du jeux de données ############
rm(list = ls())


data <-
  read.table(
    "donnees_fusionnees_conc.csv",
    h = T,
    sep = ",",
    dec = "."
  )
data$date <- ymd_hms(data$date)
data <- data %>% mutate(mois = month(data$date))

soleil <- read.table("donnees_soleil.txt", h = T, sep = "\t")


liste_conditions <- list(
  AbVe1 <-
    data %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep1"),
  AbNu1 <-
    data %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep1"),
  ExVe1 <-
    data %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep1"),
  ExNu1 <-
    data %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep1"),
  AbVe2 <-
    data %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep2"),
  AbNu2 <-
    data %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep2"),
  ExVe2 <-
    data %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep2"),
  ExNu2 <-
    data %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep2")
)


file <-
  data %>% filter(date < "2016-10-31 23:59:59" |
                    date > "2017-04-01 00:00:00")
liste_conditions_période_croissance <- list(
  PC_AbVe1 <-
    file %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep1"),
  PC_AbNu1 <-
    file %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep1"),
  PC_ExVe1 <-
    file %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep1"),
  PC_ExNu1 <-
    file %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep1"),
  PC_AbVe2 <-
    file %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep2"),
  PC_AbNu2 <-
    file %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep2"),
  PC_ExVe2 <-
    file %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep2"),
  PC_ExNu2 <-
    file %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep2")
)

file_juin_octobre <-
  data %>% filter(date < "2016-10-31 23:59:59")
liste_conditions_période_hypoxie <- list(
  PC_AbVe1 <-
    file_juin_octobre %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep1"),
  PC_AbNu1 <-
    file_juin_octobre %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep1"),
  PC_ExVe1 <-
    file_juin_octobre %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep1"),
  PC_ExNu1 <-
    file_juin_octobre %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep1"),
  PC_AbVe2 <-
    file_juin_octobre %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep2"),
  PC_AbNu2 <-
    file_juin_octobre %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep2"),
  PC_ExVe2 <-
    file_juin_octobre %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep2"),
  PC_ExNu2 <-
    file_juin_octobre %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep2")
)
file_test <-
  data %>% filter(date > "2016-07-15 00:00:00" &
                    date < "2016-07-17 00:00:00")
liste_test <- list(
  PC_AbVe1 <-
    file_test %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep1"),
  PC_AbNu1 <-
    file_test %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep1"),
  PC_ExVe1 <-
    file_test %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep1"),
  PC_ExNu1 <-
    file_test %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep1"),
  PC_AbVe2 <-
    file_test %>% filter(expo_vent == "Ab", veg == "Ve", rep == "Rep2"),
  PC_AbNu2 <-
    file_test %>% filter(expo_vent == "Ab", veg == "Nu", rep == "Rep2"),
  PC_ExVe2 <-
    file_test %>% filter(expo_vent == "Ex", veg == "Ve", rep == "Rep2"),
  PC_ExNu2 <-
    file_test %>% filter(expo_vent == "Ex", veg == "Nu", rep == "Rep2")
)


data_1 <-
  map(liste_conditions, ~ select(., c(3, 5, 8, 13))) # ,14,15,16,18,19
data_2 <-
  map(liste_conditions_période_croissance, ~ select(., c(3, 5, 8, 13))) # ,14,15,16,18,19
data_3 <-
  map(liste_conditions_période_hypoxie, ~ select(., c(3, 5, 8, 13)))
data_test <-
  map(liste_test, ~ select(., c(3, 5, 8, 13))) # ,14,15,16,18,19

data_moy <-
  data %>% 
  group_by(date, expo_vent, veg) %>% 
  summarize_all(mean) %>%
  mutate(site = str_c(expo_vent, veg)) %>% 
  select("date", "site", "mean_temp", "mean_conc_ox", "expo",-"expo_vent")

ggplot() +
  geom_line(data = data_moy, aes(date, scale(expo)), color = "chartreuse2") +
  geom_line(data = data_moy, aes(date, scale(mean_conc_ox)), color = "darkblue") +
  geom_line(data = data_moy, aes(date, scale(mean_temp)), color = "darkred") +
  facet_wrap( ~ site)

data_ML <-
  map(liste_conditions, ~ select(., c(2, 3, 5, 8, 13, 21, 22, 23)))

dataPLOT <-
  data %>% mutate(
    veg = ifelse(data$veg == "Ve", "Avec végétation", "Sans végétation"),
    expo_vent = ifelse(data$expo_vent == "Ex", "Exposé au vent", "Abrité du vent"),
    répétition = rep
  )

data_freq1 <- data %>%
  mutate(mois  = month(data$date, label = TRUE, abbr = FALSE),
         annees = year(data$date),
         date_heure = data$date,
         date = as_date(data$date),
         semaine = as.factor(week(data$date)),
         hypoxie = ifelse(data$mean_conc_ox >= 2 & data$mean_conc_ox <=5,1,0),
         anoxie = ifelse(data$mean_conc_ox < 2, 1, 0)) %>% 
  select(date,date_heure,annees,mois,semaine,site,hypoxie,anoxie)

data_freq <-  soleil %>%
  mutate(
    date = ymd(soleil$date),
    lever_soleil = ymd_hms(str_c(soleil$date,soleil$Lever_soleil,sep=" ")),
    coucher_soleil = ymd_hms(str_c(soleil$date,soleil$coucher_soleil,sep=" "))
  ) %>% 
  select(date,lever_soleil,coucher_soleil) %>% 
  inner_join(data_freq1,., by = "date")  %>% 
  mutate(
    jour_nuit = ifelse(
      .$date_heure < .$lever_soleil | .$date_heure > .$coucher_soleil, "nuit", "jour"
    )
  ) %>% 
  mutate(date = data_freq1$date_heure) %>% 
  select(date,annees,mois,semaine,site,hypoxie,anoxie,jour_nuit)


########################################################################################################################
#####                                                                                                             ######
#####             >>>>>>>>>>>>>            F O N C T I O N     S I M P L E X              <<<<<<<<<<<<<<<         ######
#####                                                                                                             ######
########################################################################################################################


Simplex <- function(liste,
                    name = "simplex_desaisonnalisees_toutes_series",
                    scale_var = TRUE,
                    dimension,
                    librairie,
                    prediction,
                    temps_prediction,
                    tp = 1:10) {
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
  
  
  tableau_final <- data.frame(
    variables = c(1:nombre_var),
    Esimplex = c(1:nombre_var),
    Epnn = c(1:nombre_var),
    theta = c(1:nombre_var),
    cor_theta_lin = c(1:nombre_var),
    cor_theta_max = c(1:nombre_var)
  )
  list_final <- list()
  
  tp <- tp
  
  lib <- librairie
  
  pred <- prediction
  
  indice <- 1:nombre_var
  
  
  
  for (i in 1:length(liste)) {
    tic()
    nom_condition <-
      as.character(liste[[i]][i, 1]) # nom de la condition analysée i.e un tableau de la liste (e.g AbVe)
    
    dir.create(
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/",
        name,
        "/",
        nom_condition,
        "/"
      )
    )
    
    for (j in 2:length(liste[[i]])) {
      variable <- liste[[i]][[j]]
      
      nom_var <- names(liste[[i]][j]) # nom de la variable
      
      simplex_output <- simplex(variable, lib, pred, E = dimension)
      
      choix <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        geom_vline(
          xintercept = seq(1, max(dimension), by = 1),
          linetype = "longdash",
          col = "grey40",
          alpha = 0.5
        ) +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      
      print(choix)
      
      cat(
        "Choisir le nombre de dimension de la ",
        paste0(
          "condition : ",
          as.character(nom_condition),
          " -- ",
          nom_var
        ),
        ":"
      )
      
      selection_E <- readline(prompt = "")
      
      E_max <- as.numeric(selection_E)
      
      simplex_pred <-
        simplex(variable, lib, pred, E = E_max, tp = tp)
      
      smap_output <- s_map(variable, lib, pred, E = E_max)
      
      cor_theta_lineaire <- smap_output$rho[smap_output$theta == 0]
      
      cor_theta_max <- max(smap_output$rho)
      
      theta_select <- smap_output$theta[which.max(smap_output$rho)]
      
      tableau_final[indice[j - 1], 1] <- nom_var
      tableau_final[indice[j - 1], 2] <- E_max
      tableau_final[indice[j - 1], 3] <- theta_select
      tableau_final[indice[j - 1], 4] <- cor_theta_lineaire
      tableau_final[indice[j - 1], 5] <- cor_theta_max
      
      
      ###### Les plots
      a <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      
      b <-
        ggplot(simplex_pred, aes(tp, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Période de prédiction") +
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
        scale_x_continuous(breaks = c(seq(1, max(tp), by = 1)), limits =
                             c(1, max(tp)))
      
      c <-
        ggplot(smap_output, aes(theta, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Non linéarité (theta)") +
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
        )
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/",
          name,
          "/",
          nom_condition,
          "/variable_",
          nom_var,
          ".pdf"
        )
      
      pdf(filename)
      grid.arrange(a, b, c)
      dev.off()
    }
    
    list_final[[i]] <- tableau_final
    
    names(list_final)[i] <- paste(nom_condition)
    
    filename <-
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/",
        name,
        "/",
        nom_condition,
        "/tableau_final.pdf"
      )
    
    pdf(filename)
    grid.table(tableau_final)
    dev.off()
  } ### fermeture de la boucle sur les conditions
  filename <-
    paste0(
      "C:/users/theo.marechal/Work Folders/Desktop/",name,"tableau_final.pdf"
    )
  
  pdf(filename)
  grid.table(bind_rows(list_final))
  dev.off()
} ### fermeture de la fonction


########################################################################################################################
#####                                                                                                             ######
#####             >>>>>>>>>>>>>            F O N C T I O N     E D M                      <<<<<<<<<<<<<<<         ######
#####                                                                                                             ######
########################################################################################################################







EDM <- function(liste,
                scale_var = TRUE,
                dimension,
                librairie,
                prediction,
                temps_prediction,
                tp = 1:10,
                var_ccm,
                librairies_ccm = seq(10, 80, by = 10)) {
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM"))
  
  if (scale_var == TRUE) {
    for (h in 1:length(liste)) {
      liste[[h]][, 2:length(liste[[h]])] <-
        as.data.frame(scale(liste[[h]][, 2:length(liste[[h]])]))
    }
  } else {
    liste <- liste
  }
  
  nombre_var <- length(liste[[1]]) - 1
  
  
  tableau_final <- data.frame(
    variables = c(1:nombre_var),
    E = c(1:nombre_var),
    theta = c(1:nombre_var),
    cor_theta_lin = c(1:nombre_var),
    cor_theta_max = c(1:nombre_var)
  )
  list_final <- list()
  
  tp <- tp
  
  lib <- librairie
  
  pred <- prediction
  
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
      ordre <- as.data.frame(t(n[i, ]))
      
      dft[i, ] <- sort(ordre[, 1])
    }
    
    combinaison <- unique(dft)
    
    return(combinaison)
  }
  
  combinaisons <-
    combinaison(var_ccm) # combinaisons contient les combinaisons de variables à teste"r
  
  
  
  for (i in 1:length(liste)) {
    nom_condition <-
      levels(liste[[i]][i, 1])[i] # nom de la condition analysée i.e un tableau de la liste (e.g AbVe)
    
    dir.create(
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/EDM/",
        nom_condition,
        "/"
      )
    )
    
    for (j in 2:length(liste[[i]])) {
      variable <- liste[[i]][[j]]
      
      nom_var <- names(liste[[i]][j]) # nom de la variable
      
      simplex_output <- simplex(variable, lib, pred, E = dimension)
      
      choix <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      
      print(choix)
      
      cat("Choisir le nombre de dimension de la variable", nom_var, ":")
      
      selection_E <- readline(prompt = "")
      
      E_max <- as.numeric(selection_E)
      
      simplex_pred <-
        simplex(variable, lib, pred, E = E_max, tp = tp)
      
      smap_output <- s_map(variable, lib, pred, E = E_max)
      
      cor_theta_lineaire <- smap_output$rho[smap_output$theta == 0]
      
      cor_theta_max <- max(smap_output$rho)
      
      theta_select <- smap_output$theta[which.max(smap_output$rho)]
      
      tableau_final[indice[j - 1], 1] <- nom_var
      tableau_final[indice[j - 1], 2] <- E_max
      tableau_final[indice[j - 1], 3] <- theta_select
      tableau_final[indice[j - 1], 4] <- cor_theta_lineaire
      tableau_final[indice[j - 1], 5] <- cor_theta_max
      
      
      ###### Les plots
      a <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      
      b <-
        ggplot(simplex_pred, aes(tp, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Période de prédiction") +
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
        scale_x_continuous(breaks = c(seq(1, max(tp), by = 1)), limits =
                             c(1, max(tp)))
      
      c <-
        ggplot(smap_output, aes(theta, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Non linéarité (theta)") +
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
        )
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/EDM/",
          nom_condition,
          "/variable_",
          nom_var,
          ".pdf"
        )
      
      pdf(filename)
      grid.arrange(a, b, c)
      dev.off()
    }
    
    list_final[[i]] <- tableau_final
    
    names(list_final)[i] <- paste(nom_condition)
    
    filename <-
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/EDM/",
        nom_condition,
        "/tableau_final.pdf"
      )
    
    pdf(filename)
    grid.table(tableau_final)
    dev.off()
    
    ######## boucle qui fait les CCM ######
    
    for (k in 1:nrow(combinaisons)) {
      nombre_moyennes <- length(librairies_ccm)
      
      nom_var_un <- combinaisons[k, 1]
      nom_var_deux <- combinaisons[k, 2]
      
      E_ccm_1 <-
        tableau_final %>%
        filter(variables == combinaisons[k, 1]) %>%
        select(E)
      E_ccm_2 <-
        tableau_final %>%
        filter(variables == combinaisons[k, 2]) %>%
        select(E)
      
      ccm_1 <-
        ccm(
          liste[[i]],
          E = E_ccm_1,
          lib_column = combinaisons[k, 1],
          target_column = combinaisons[k, 2],
          lib_sizes = librairies_ccm,
          num_samples = 100,
          random_libs = TRUE,
          replace = TRUE
        )
      
      ccm_2 <-
        ccm(
          liste[[i]],
          E = E_ccm_2,
          lib_column = combinaisons[k, 2],
          target_column = combinaisons[k, 1],
          lib_sizes = librairies_ccm,
          num_samples = 100,
          random_libs = TRUE,
          replace = TRUE
        )
      
      a_xmap_t_means <-
        ccm_means(ccm_1) %>% mutate(variable = rep(combinaisons[k, 1], nombre_moyennes))
      
      t_xmap_a_means <-
        ccm_means(ccm_2) %>% mutate(variable = rep(combinaisons[k, 2], nombre_moyennes))
      
      tableau_ccm <- bind_rows(a_xmap_t_means, t_xmap_a_means)
      
      
      plot_ccm <-
        ggplot(tableau_ccm, aes(lib_size, rho, colour = variable)) +
        geom_line(size = rel(1)) +
        geom_point(size = rel(2)) +
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
          paste(combinaisons[k, 2], "cause", combinaisons[k, 1]),
          paste(combinaisons[k, 1], "cause", combinaisons[k, 2])
        ))
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/EDM/",
          nom_condition,
          "/CCM_",
          paste0(combinaisons[k, 1], "_", combinaisons[k, 2]),
          ".pdf"
        )
      
      pdf(filename)
      print(plot_ccm)
      dev.off()
    } ### fermeture de la boucle sur les ccm
  } ### fermeture de la boucle sur les conditions
  return(list_final)
} ### fermeture de la fonction










########################################################################################################################
#####                                                                                                             ######
#####        >>>>>>>>>>>>>            F O N C T I O N     C C M     D E L A I S           <<<<<<<<<<<<<<<         ######
#####                                                                                                             ######
########################################################################################################################



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
  
  
  ############################### Boucle sur les conditions ##################################
  for (i in 1:length(liste)) {
    nom_condition <-
      as.character(liste[[i]][i, 1]) # nom de la condition analysée i.e un tableau de la liste (e.g AbVe)
    
    dir.create(
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/",
        name,
        "/",
        nom_condition,
        "/"
      )
    )
    
    ############################### Boucle sur les variables ##################################  
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
          ccm_2_lambda = rep(0, length(delay_ccm)),
          ccm_1_ic_sup = rep(0, length(delay_ccm)),
          ccm_2_ic_sup = rep(0, length(delay_ccm)),
          ccm_1_ic_inf = rep(0, length(delay_ccm)),
          ccm_2_ic_inf = rep(0, length(delay_ccm)),
          ccm_1_ic_sup_rho = rep(0, length(delay_ccm)),
          ccm_2_ic_sup_rho = rep(0, length(delay_ccm)),
          ccm_1_ic_inf_rho = rep(0, length(delay_ccm)),
          ccm_2_ic_inf_rho = rep(0, length(delay_ccm))
        )
      
      ############################### Boucle sur les TP ##################################
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
          mean((ccm_1$rmse + ccm_1$mae) / ccm_1$rho)
        delay_cor[l, 6] <- mean(ccm_2$rho)
        delay_cor[l, 7] <- mean(ccm_2$mae)
        delay_cor[l, 8] <- mean(ccm_2$rmse)
        delay_cor[l, 9] <-
          mean((ccm_2$rmse + ccm_2$mae) / ccm_2$rho)
        delay_cor[l,10] <- mean((ccm_1$rmse + ccm_1$mae) / ccm_1$rho) + 1.96*(sd((ccm_1$rmse + ccm_1$mae) / ccm_1$rho)/sqrt(length(ccm_1$rmse)))
        
        delay_cor[l,11] <- mean((ccm_2$rmse + ccm_2$mae) / ccm_2$rho) + 1.96*(sd((ccm_2$rmse + ccm_2$mae) / ccm_2$rho)/sqrt(length(ccm_2$rmse)))
        
        delay_cor[l,12] <- mean((ccm_1$rmse + ccm_1$mae) / ccm_1$rho) - 1.96*(sd((ccm_1$rmse + ccm_1$mae) / ccm_1$rho)/sqrt(length(ccm_1$rmse)))
        delay_cor[l,13] <- mean((ccm_2$rmse + ccm_2$mae) / ccm_2$rho) - 1.96*(sd((ccm_2$rmse + ccm_2$mae) / ccm_2$rho)/sqrt(length(ccm_2$rmse)))
        delay_cor[l,14] <- mean(ccm_1$rho) + 1.96*(sd(ccm_1$rho)/sqrt(length(ccm_1$rho)))
        delay_cor[l,15] <- mean(ccm_2$rho) + 1.96*(sd(ccm_2$rho)/sqrt(length(ccm_2$rho)))
        delay_cor[l,16] <- mean(ccm_1$rho) - 1.96*(sd(ccm_1$rho)/sqrt(length(ccm_1$rho)))
        delay_cor[l,17] <- mean(ccm_2$rho) - 1.96*(sd(ccm_2$rho)/sqrt(length(ccm_2$rho)))
        
        
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
          tp = delay_cor[which.max(delay_cor[, 2]), 1]
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
          tp = delay_cor[which.max(delay_cor[, 6]), 1]
        )
      
      ############################### Boucle sur les surrogates ##################################
      ############### SURROGATE SEASONAL ##############
      if (surrogate == TRUE) {
        var_1 <- as.numeric(na.omit(liste[[i]][, combinaisons[k, 1]]))
        var_2 <- as.numeric(na.omit(liste[[i]][, combinaisons[k, 2]]))
        surr_var1 <- make_surrogate_ebisuzaki(var_1, num_surr = num_surr)
        surr_var2 <- make_surrogate_ebisuzaki(var_2, num_surr = num_surr)
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
              tp = delay_cor[which.max(delay_cor[, 2]), 1],
              lib_sizes = taille_lib,
              replace = T
            )$rho
          
          ccm_rho_surr$var2[m] <-
            ccm(
              cbind(liste[[i]][, combinaisons[k, 2]], surr_var1[, m]),
              E = E_ccm_2,
              lib_column = 1,
              target_column = 2,
              tp = delay_cor[which.max(delay_cor[, 6]), 1],
              lib_sizes = taille_lib,
              replace = T
            )$rho
          
          
        }
        
        sur_ccm1 <-
          mean(ccm_rho_surr$var1) + 2 * sd(ccm_rho_surr$var1)
        sur_ccm2 <-
          mean(ccm_rho_surr$var2) + 2 * sd(ccm_rho_surr$var2)
        
        print(c(sur_ccm1,sur_ccm2))
      }
      ############### FIN  SURROGATE ##############
      
      
      
      
      
      a_xmap_t_means <-
        ccm_1[, c("lib_size", "rho")] %>% mutate(variable = rep(combinaisons[k, 1], nrow(ccm_1)))
      
      t_xmap_a_means <-
        ccm_2[, c("lib_size", "rho")] %>% mutate(variable = rep(combinaisons[k, 2], nrow(ccm_2)))
      
      tableau_ccm <- bind_rows(a_xmap_t_means, t_xmap_a_means)
      
      
      tableau_delay <- delay_cor %>%
        select(1, 2, 6, 14, 15, 16, 17) %>%
        gather("ccm", "cor", c(2,3)) %>% 
        split(f = .$ccm) 
      
      tableau_delay <- bind_rows(
        tableau_delay$ccm_1_rho[,c(1,2,4,6,7)] %>% 
          mutate(upper = ccm_1_ic_sup_rho,lower = ccm_1_ic_inf_rho) %>% 
          select(-(2:3)), tableau_delay$ccm_2_rho[,c(1,3,5,6,7)] %>% 
          mutate(upper = ccm_2_ic_sup_rho,lower = ccm_2_ic_inf_rho) %>% 
          select(-(2:3)))
      
      tableau_lambda <- delay_cor %>%
        select(1, 5, 9,10,11,12,13) %>%
        gather("ccm", "lambda", c(2,3)) %>% 
        split(f = .$ccm) 
      
      tableau_lambda <-bind_rows(
        tableau_lambda$ccm_1_lambda[,c(1,2,4,6,7)] %>% 
          mutate(upper = ccm_1_ic_sup,lower = ccm_1_ic_inf) %>% 
          select(-(2:3)), tableau_lambda$ccm_2_lambda[,c(1,3,5,6,7)] %>% 
          mutate(upper = ccm_2_ic_sup,lower = ccm_2_ic_inf) %>% 
          select(-(2:3))
      )
      
      
      
      ############################### PLOT ##################################      
      plot_delay <-
        ggplot(tableau_delay, aes(x = delay, y = cor, color = ccm)) +
        geom_line(aes(group = ccm))+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
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
        ggplot(tableau_lambda, aes(x = delay, y = lambda, color = ccm)) +
        geom_line(aes(group = ccm)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
        ylab("f(lambda) \n (i.e. rmse + mae / rho)") +
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
              delay_cor[which.max(delay_cor[, 2]), 1]
            ),
            paste(
              combinaisons[k, 1],
              "cause",
              combinaisons[k, 2],
              "|décalage =",
              delay_cor[which.max(delay_cor[, 6]), 1]
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
              delay_cor[which.max(delay_cor[, 2]), 1]
            ),
            paste(
              combinaisons[k, 1],
              "cause",
              combinaisons[k, 2],
              "|décalage =",
              delay_cor[which.max(delay_cor[, 6]), 1]
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






########################################################################################################################
#####                                                                                                             ######
#####             >>>>>>>>>>>>>            F O N C T I O N     O N D E L E T T E          <<<<<<<<<<<<<<<         ######
#####                                                                                                             ######
########################################################################################################################




Ondelettes <- function(liste) {
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes"))
  for (i in 1:length(liste)) {
    nom_condition <- as.character(liste[[i]][2, 2])
    dir.create(
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/",
        nom_condition,
        "/"
      )
    )
    
    for (j in 3:length(liste[[i]])) {
      nom_variable <- colnames(liste[[i]][j])
      variable <- as.data.frame(na.omit(liste[[i]][c(1, j)]))
      ondelette <- analyze.wavelet(variable,
                                   nom_variable,
                                   dt = 1 / 24,
                                   upperPeriod = 360)
      
      
      spectre_puissance <-
        data.frame(
          Puissance = ondelette$Power.avg,
          Periode = ondelette$Period,
          pval = ondelette$Power.avg.pval
        )
      spectre_puissance <-
        spectre_puissance %>% filter(pval <= 0.05)
      spectre_puissance <-
        ggplot(spectre_puissance, aes(Periode, Puissance)) +
        geom_line(col = "red3", size = rel(1)) +
        geom_point(col = "darkblue") +
        xlab("Période (j)") +
        geom_vline(
          xintercept = seq(0, 360, by = 20),
          linetype = "longdash",
          col = "grey40",
          alpha = 0.5
        ) +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = rel(2),
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
        )
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/",
          nom_condition,
          "/Ondelette_",
          nom_variable,
          ".pdf"
        )
      pdf(filename)
      plot_ondelette <-
        wt.image(
          ondelette,
          color.key = "quantile",
          n.levels = 250,
          show.date = TRUE,
          date.format = "%Y %M",
          legend.params = list(lab = "wavelet power levels", mar = 4.7)
        )
      print(spectre_puissance)
      dev.off()
    }
  }
}

########################################################################################################################
#####                                                                                                             ######
#####   >>>>>>>>>>>>>         F O N C T I O N     D E S A I S O N N A L I S A T I O N          <<<<<<<<<<<<<<<    ######
#####                                                                                                             ######
########################################################################################################################




desaisonnalisation <- function(liste, saison) {
  liste_final <- list()
  for (i in 1:length(liste)) {
    tic()
    df <-
      data.frame(
        mean_temp = rep(0, nrow(liste[[i]])),
        mean_conc_ox = rep(0, nrow(liste[[i]])),
        expo = rep(0, nrow(liste[[i]]))
      )
    nom_condition <- as.character(liste[[i]][2, 1])
    for (j in 2:length(liste[[i]])) {
      nom_variable <- colnames(liste[[i]][j])
      serie_des <- msts(liste[[i]][, j], seasonal.periods = saison)
      modele_des <- AutoSTR(serie_des, robust = FALSE)$output$random
      df[, j - 1] <- modele_des
    }
    names(df) <- colnames(liste[[i]][, -1])
    df <- df %>%
      mutate(condition = rep(nom_condition, nrow(df))) %>%
      select(c(4, 1, 2, 3))
    liste_final[[i]] <- df
    print(i)
    toc()
  }
  
  return(liste_final)
}

########################################################################################################################
#####                                                                                                             ######
#####   >>>>>>>>>>>>>         F O N C T I O N     P S E U D O  NN                              <<<<<<<<<<<<<<<    ######
#####                                                                                                             ######
########################################################################################################################


PNN <- function(vec, E) {
  vec <- na.omit(vec)
  
  longueur_matrice <- length(vec) - E
  
  df <- matrix(0, longueur_matrice, E)
  
  df[, 1] <- vec[-c((length(vec) - (E - 1)):length(vec))]
  
  Ek <- matrix(0, length(vec) - E, E - 1)
  
  matrice_rapport <-
    matrix(rep(0, dim(Ek)[1] * (dim(Ek)[2] - 1)), dim(Ek)[1], dim(Ek)[2] -
             1)
  
  Gk <- c()
  
  
  
  for (i in 2:E) {
    df[, i] <- vec[c(i:(length(vec) - (E - i + 1)))]
  }
  
  ######### boucle qui va tourner sur les différentes dimensions ########
  for (j in 1:(ncol(df) - 1)) {
    e <- j + 1
    df2 <- df[, 1:e]
    
    
    distance <- as.matrix(dist(df2))
    
    dist_c <- c()
    
    ######### boucle qui va tourner sur les individus de la matrice de distance de la dimension ########
    for (k in 1:nrow(distance)) {
      Ek[k, j] <-
        min(distance[-k, k]) + 0.0001 # on recherche la plus petite distance dans l'attracteur
    }
  }
  
  for (m in 1:(ncol(Ek) - 1)) {
    matrice_rapport[, m] <- log(Ek[, m + 1] / Ek[, m])
  }
  
  
  moy_Ek <- apply(matrice_rapport, 2, mean)
  
  
  return(moy_Ek)
}

########################################################################################################################
#####                                                                                                             ######
#####   >>>>>>>>>>>>>         F O N C T I O N     S E L E C T I O N  E                         <<<<<<<<<<<<<<<    ######
#####                                                                                                             ######
########################################################################################################################
SelectionE <- function(liste,
                       name = "simplex_desaisonnalisees_toutes_series",
                       scale_var = TRUE,
                       dimension,
                       librairie,
                       prediction,
                       temps_prediction,
                       tp = 1:10) {
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/", name))
  
  if (scale_var == TRUE) {
    for (h in 1:length(liste)) {
      liste[[h]][, 2:length(liste[[h]])] <-
        as.data.frame(scale(liste[[h]][, 2:length(liste[[h]])]))
    }
  } else {
    liste <- liste
  }
  
  
  
  
  
  
  
  ######################################################## P N N ##########################################################
  
  PNN <- function(vec, E) {
    vec <- na.omit(vec)
    
    longueur_matrice <- length(vec) - E
    
    df <- matrix(0, longueur_matrice, E)
    
    df[, 1] <- vec[-c((length(vec) - (E - 1)):length(vec))]
    
    Ek <- matrix(0, length(vec) - E, E - 1)
    
    matrice_rapport <-
      matrix(rep(0, dim(Ek)[1] * (dim(Ek)[2] - 1)), dim(Ek)[1], dim(Ek)[2] -
               1)
    
    Gk <- c()
    
    
    
    for (i in 2:E) {
      df[, i] <- vec[c(i:(length(vec) - (E - i + 1)))]
    }
    
    ######### boucle qui va tourner sur les différentes dimensions ########
    for (j in 1:(ncol(df) - 1)) {
      e <- j + 1
      df2 <- df[, 1:e]
      
      
      distance <- as.matrix(dist(df2))
      
      dist_c <- c()
      
      ######### boucle qui va tourner sur les individus de la matrice de distance de la dimension ########
      for (k in 1:nrow(distance)) {
        Ek[k, j] <-
          min(distance[-k, k]) + 0.0001 # on recherche la plus petite distance dans l'attracteur
      }
    }
    
    for (m in 1:(ncol(Ek) - 1)) {
      matrice_rapport[, m] <- log(Ek[, m + 1] / Ek[, m])
    }
    
    
    moy_Ek <- apply(matrice_rapport, 2, mean)
    
    
    return(moy_Ek)
  }
  
  ######################################################## F I N ##########################################################
  nombre_var <- length(liste[[1]]) - 1
  
  
  tableau_final <- data.frame(
    variables = c(1:nombre_var),
    Emax = c(1:nombre_var),
    theta = c(1:nombre_var),
    cor_theta_lin = c(1:nombre_var),
    cor_theta_max = c(1:nombre_var)
  )
  list_final <- list()
  
  tp <- tp
  
  lib <- librairie
  
  pred <- prediction
  
  indice <- 1:nombre_var
  
  
  
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
    
    for (j in 2:length(liste[[i]])) {
      variable <- liste[[i]][[j]]
      
      nom_var <- names(liste[[i]][j]) # nom de la variable
      
      simplex_output <- simplex(variable, lib, pred, E = dimension)
      pnn_output <- PNN(variable, max(dimension))
      pnn <-
        data.frame(dim = seq(2:(length(dimension) - 1)), distance = pnn_output)
      
      
      
      choix_simplex <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        geom_vline(
          xintercept = seq(1, max(dimension), by = 1),
          linetype = "longdash",
          col = "grey40",
          alpha = 0.5
        ) +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      choix_pnn <-
        ggplot(pnn, aes(dim, distance)) + geom_line(col = "orange") +
        geom_point() +
        geom_vline(
          xintercept = seq(1, max(dimension), by = 1),
          linetype = "longdash",
          col = "grey40",
          alpha = 0.5
        ) +
        ylab("Distance") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(
          breaks = c(seq(1, max(dimension) - 2, by = 1)),
          labels = seq(2, 19, 1),
          limits =
            c(1, max(dimension) - 2)
        )
      print(grid.arrange(choix_simplex, choix_pnn))
      
      cat("Choisir le nombre de dimension de la variable", nom_var, ":")
      
      selection_E <- readline(prompt = "")
      
      E_max <- as.numeric(selection_E)
      
      simplex_pred <-
        simplex(variable, lib, pred, E = E_max, tp = tp)
      
      smap_output <- s_map(variable, lib, pred, E = E_max)
      
      cor_theta_lineaire <- smap_output$rho[smap_output$theta == 0]
      
      cor_theta_max <- max(smap_output$rho)
      
      theta_select <- smap_output$theta[which.max(smap_output$rho)]
      
      tableau_final[indice[j - 1], 1] <- nom_var
      tableau_final[indice[j - 1], 2] <- E_max
      tableau_final[indice[j - 1], 3] <- theta_select
      tableau_final[indice[j - 1], 4] <- cor_theta_lineaire
      tableau_final[indice[j - 1], 5] <- cor_theta_max
      
      
      ###### Les plots
      a <-
        ggplot(simplex_output, aes(E, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Nombre de dimensions (E)") +
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
        scale_x_continuous(breaks = c(seq(1, max(dimension), by = 1)), limits =
                             c(1, max(dimension)))
      
      b <-
        ggplot(simplex_pred, aes(tp, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Période de prédiction") +
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
        scale_x_continuous(breaks = c(seq(1, max(tp), by = 1)), limits =
                             c(1, max(tp)))
      
      c <-
        ggplot(smap_output, aes(theta, rho)) + geom_line(col = "orange") +
        geom_point() +
        ylab("Corrélation") +
        xlab("Non linéarité (theta)") +
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
        )
      
      filename <-
        paste0(
          "C:/users/theo.marechal/Work Folders/Desktop/",
          name,
          "/",
          nom_condition,
          "/variable_",
          nom_var,
          ".pdf"
        )
      
      pdf(filename)
      grid.arrange(a, choix_pnn, b, c)
      dev.off()
    }
    
    list_final[[i]] <- tableau_final
    
    names(list_final)[i] <- paste(nom_condition)
    
    filename <-
      paste0(
        "C:/users/theo.marechal/Work Folders/Desktop/",
        name,
        "/",
        nom_condition,
        "/tableau_final.pdf"
      )
    
    pdf(filename)
    grid.table(tableau_final)
    dev.off()
  } ### fermeture de la boucle sur les conditions
  return(list_final)
} ### fermeture de la fonction


########################################################################################################################
#####                                                                                                             ######
#####   >>>>>>>>>>>>>         T A B L E A U    M A C H I N E    L E A R N I N G                          <<<<<<<<<<<<<<<    ######
#####                                                                                                             ######
########################################################################################################################





tableauML <- function(liste,
                      index_col = c(1, 2),
                      max_lag = 5) {
  tabML <- list()
  
  for (i in 1:length(liste)) {
    df_decalage <-
      matrix(rep(0, nrow(liste[[i]]) * length(index_col) * max_lag),
             nrow(liste[[i]]),
             length(index_col) * max_lag)
    if (length(index_col) > 1) {
      nom_col <-
        str_c(rep(colnames(liste[[i]][, index_col + 1]), each = max_lag),
              rep(1:max_lag, times = length(index_col)),
              sep = "_dec_")
    } else {
      nom_col <-
        str_c(rep(colnames(liste[[i]][index_col + 1]), each = max_lag),
              rep(1:max_lag, times = length(index_col)),
              sep = "_dec_")
    }
    
    colnames(df_decalage) <- nom_col
    df_decalage <- as.data.frame(df_decalage)
    data <- as.data.frame(liste[[i]][, index_col + 1])
    conteur1 <- 1
    conteur2 <- max_lag
    for (j in 1:length(data)) {
      df_decalage[, c(conteur1:conteur2)] <-
        make_block(data[, j], max_lag = max_lag)[, -1]
      conteur1 <- conteur1 + max_lag
      conteur2 <- conteur2 + max_lag
    }
    tabML[[i]] <-
      bind_cols(liste[[i]][, -(index_col + 1)], df_decalage)
    tabML[[i]] <- tabML[[i]][-(1:max_lag), ]
  }
  tabML <- bind_rows(tabML)
  return(tabML)
}



