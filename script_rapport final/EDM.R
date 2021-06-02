source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")

#########################################################################
## --------------------------------------------------------------------##
######################         Séries brutes    #########################
## --------------------------------------------------------------------##
#########################################################################


######### Oxygène
ggplot(dataPLOT, aes(date, mean_conc_ox, colour = répétition)) +
  geom_line(alpha = 0.5) + facet_grid(expo_vent~veg)+
  ylab(bquote("Concentration en"*~O[2]*" dissous (mg .L" ^-1*")")) +
  xlab(bquote("Date")) +
  scale_x_datetime(breaks = date_breaks("2 month"),labels = date_format("%b %Y")
                   )+
  theme(
    axis.line = element_line(
      linetype = 1,
      size = rel(0.9),
      color = "black"
    ),
    strip.background = element_rect(colour="black", fill="white"),
    panel.background = element_rect(fill = NA,color = "black"),
    axis.text = element_text( color = "grey30"),
    axis.text.x = element_text(size = rel(1.4),angle = 45,vjust = 1,hjust = 1),
    axis.text.y = element_text(size=rel(1.4)),
    axis.ticks = element_line(size = rel(1), color = "grey30"),
    axis.title = element_text(
      size = rel(1.5),
      hjust = 0.5,
      vjust = 4,
      colour = "grey30"
    )
  ) +
  scale_colour_discrete(labels = c(
    paste("répétition 1"),
    paste("répétition 2")
  )) +
  geom_hline(
    yintercept = c(2,5), #############
    linetype = "longdash",
    col = "grey40",alpha = 0.5,size=rel(0.8)) + 
  scale_y_continuous(breaks = c(0,2.5,5,7.5,10,12.5))

######### Keddy
ggplot(dataPLOT, aes(date, expo,color=répétition)) +
  geom_line(alpha = 0.5) + facet_grid(expo_vent~veg)+
  ylab(bquote("Indice de Keddy")) +
  xlab(bquote("Date")) +
  scale_x_datetime(breaks = date_breaks("2 month"),labels = date_format("%b %Y")
  )+
  theme(
    axis.line = element_line(
      linetype = 1,
      size = rel(0.9),
      color = "black"
    ),
    strip.background = element_rect(colour="black", fill="white"),
    panel.background = element_rect(fill = NA,color = "black"),
    axis.text = element_text( color = "grey30"),
    axis.text.x = element_text(size = rel(1.4),angle = 45,vjust = 1,hjust = 1),
    axis.text.y = element_text(size=rel(1.4)),
    axis.ticks = element_line(size = rel(1), color = "grey30"),
    axis.title = element_text(
      size = rel(1.5),
      hjust = 0.5,
      vjust = 4,
      colour = "grey30"
    )
  )+
  scale_colour_discrete(labels = c(
    paste("répétition 1"),
    paste("répétition 2")
  )) 
######### Température
ggplot(dataPLOT, aes(date, mean_temp, colour = répétition)) +
  geom_line(size = rel(1),alpha = 0.4) + facet_grid(expo_vent~veg)+
  ylab(bquote("Température (°C)")) +
  xlab(bquote("Date")) +
  scale_x_datetime(breaks = date_breaks("2 month"),labels = date_format("%b %Y")
  )+
  theme(
    axis.line = element_line(
      linetype = 1,
      size = rel(0.9),
      color = "black"
    ),
    strip.background = element_rect(colour="black", fill="white"),
    panel.background = element_rect(fill = NA,color = "black"),
    axis.text = element_text( color = "grey30"),
    axis.text.x = element_text(size = rel(1.4),angle = 45,vjust = 1,hjust = 1),
    axis.text.y = element_text(size=rel(1.4)),
    axis.ticks = element_line(size = rel(1), color = "grey30"),
    axis.title = element_text(
      size = rel(1.5),
      hjust = 0.5,
      vjust = 4,
      colour = "grey30"
    )
  ) +
  scale_colour_discrete(labels = c(
    paste("répétition 1"),
    paste("répétition 2")
  )) 

####### toutes les courbes ensembles
for (h in 1:length(liste)) {
  liste[[h]][, 2:length(liste[[h]])] <-
    as.data.frame(scale(liste[[h]][, 2:length(liste[[h]])]))
}
colnames(dataPLOT)

dataPLOT[,c(5,8,13)] <- as.data.frame(scale(dataPLOT[,c(5,8,13)]))
data_series <- dataPLOT[,c(2,5,8,13,20,21,24)] %>% gather(key = "variables",value = "valeurs",2:4) 

ggplot(data_series, aes(date, valeurs, color = variables)) +
  geom_line(alpha = 0.8) + facet_grid(répétition~veg+expo_vent)+
  xlab(bquote("Date")) +
  ylab("Valeurs centrées réduites")+
  scale_x_datetime(breaks = date_breaks("2 month"),labels = date_format("%b %Y")
  )+
  theme(
    axis.line = element_line(
      linetype = 1,
      size = rel(0.9),
      color = "black"
    ),
    strip.background = element_rect(colour="black", fill="white"),
    panel.background = element_rect(fill = NA,color = "black"),
    axis.text = element_text( color = "grey30"),
    axis.text.x = element_text(size = rel(1.4),angle = 45,vjust = 1,hjust = 1),
    axis.text.y = element_text(size=rel(1.4)),
    axis.ticks = element_line(size = rel(1), color = "grey30"),
    axis.title = element_text(
      size = rel(1.5),
      hjust = 0.5,
      vjust = 4,
      colour = "grey30"
    )
  ) + scale_colour_manual(labels = c(
    paste("Keddy"),
    paste("Oxygène dissous"),
    paste("Température")
  ),values = c("#71c98b","#45669e","#e12e4f"))









ggplot() + geom_smooth(data = data, aes(date, scale(mean_temp)), col = "blue") +
  geom_smooth(data = data, aes(date, scale(mean_conc_ox)), col = "red") +
  geom_smooth(data = data, aes(date, scale(scale(expo)))) +
  facet_wrap(expo_vent ~ veg)

colnames(data)

veg <- file %>% filter(veg == "Ve")
veg <- file %>% filter(veg == "Nu")

ggplot() + geom_violin(data = file, aes(site, mean_conc_ox))
ggplot(data = file, aes(log(expo+1))) + geom_histogram() +  facet_wrap(~site) 
ggplot(data = file, aes(log(expo+1))) + geom_histogram() +  facet_wrap(~expo_vent) 

ggplot(data = veg, aes(expo, mean_conc_ox, col = site)) + geom_point(alpha = 0.1) + geom_smooth(method = "lm")
file %>% group_by(expo_vent) %>% summarise(moy = mean(expo, na.rm = TRUE),
                                      es = sd(expo, na.rm = TRUE) /sqrt(length(na.omit(expo))),
                                      med = median(expo, na.rm = TRUE))
file %>% group_by(site) %>% summarise(moy = mean(expo, na.rm = TRUE),
                                           es = sd(expo, na.rm = TRUE) /sqrt(length(na.omit(expo))),
                                           med = median(expo, na.rm = TRUE))

#########################################################################
## ---------------------------------------------------------------------##
######################         ONDELETTES       #########################
## ---------------------------------------------------------------------##
#########################################################################
data_moy <- data_moy[,-1]
liste <- split(data_moy, f = as.factor(data_moy$site))
?split
Ondelettes(liste)

#########################################################################
## ---------------------------------------------------------------------##
###################### EMPIRICAL DYNAMIC MODELS #########################
## ---------------------------------------------------------------------##
#########################################################################

## Objectif analyser les relations entre variables en lien avec l'oxygène
## chaque analyse est effectuée suivant les différentes conditions
colnames(data2[[1]])


##################### Abrité avec végétation ############################
EDM(
  liste = data_2,
  dimension = c(1:30),
  librairie = c(1, 1500),
  prediction = c(1501, 3500),
  tp = 1:24,
  var_ccm = c("mean_conc_ox", "mean_temp", "expo"),
  librairies_ccm = seq(20, 2000, by = 200)
)

Simplex(
  liste = data_2,
  scale_var = TRUE,
  name = "simplex_avril_octobre",
  dimension = c(1:30),
  librairie = c(1, 2700),
  prediction = c(2701, 5200),
  temps_prediction = 1:10,
  tp = 1:10
)

SelectionE(
  liste = data_2,
  scale_var = TRUE,
  name = "simplex_desaisonnalisees_avril_octobre",
  dimension = c(1:30),
  librairie = c(1, 2000),
  prediction = c(2001, 5000),
  temps_prediction = 1:10,
  tp = 1:10
)
###### SMAP SEQUENTIEL ###########
block <- AbVe1[, c(2, 8, 13)]
a <- block_lnlp(block,
  norm_type = c("L2 norm"),
  method = "s-map", tp = 1:10,
  columns = NULL, target_column = 1, stats_only = TRUE,
  first_column_time = TRUE, exclusion_radius = NULL, epsilon = NULL,
  theta = NULL, silent = FALSE, save_smap_coefficients = TRUE,
  short_output = FALSE
)

coeff <- a[[10]]$smap_coefficients
coeff <- coeff %>% filter_all(is.nan(coeff))
plot(scale(coeff[, 1]), type = "l")
plot(scale(coeff[, 2]), type = "l", col = "red")
plot(scale(coeff[, 3]), type = "l", col = "red")
lines(scale(coeff[, 2]), col = "red")
lines(scale(coeff[, 3]), col = "blue")
lines(scale(coeff[, 4]), col = "orange")


#########################################################################
## ---------------------------------------------------------------------##
######################     Fréquence d'hypoxie        ###################
## ---------------------------------------------------------------------##
#########################################################################


hypoxie <- data %>% mutate(hypoxie = ifelse(data$mean_conc_ox <= 5 & data$mean_conc_ox >= 2, 1, 0))
ggplot() + geom_boxplot(data = hypoxie, aes(site, hypoxie))
data_3 <- hypoxie %>%
  mutate(semaine = week(date)) %>%
  group_by(site, semaine, hypoxie) %>%
  summarise(mean())
head(data_3, n = 50)
?head



#########################################################################
## ---------------------------------------------------------------------##
######################     Désaisonnalisation         ###################
## ---------------------------------------------------------------------##
#########################################################################
source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")


a <- desaisonnalisation(data_1, saison = c(24, 365 * 24))

SelectionE(
  data_1,
  name = "series_toutes_periodes",
  scale_var = TRUE,
  dimension = c(1:30),
  librairie = c(1, 4500),
  prediction = c(4501, 9000),
  tp = 1:10
)


CCM(
  liste = a,
  scale_var = TRUE,
  delay_ccm = -45:45,
  name = "CCM_desaison_entier",
  var_ccm = c("mean_temp", "mean_conc_ox", "expo"),
  librairies_ccm = seq(20, 6000, by = 600),
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

#########################################################################
## ---------------------------------------------------------------------##
######################     M A CH I N E   L           ###################
## ---------------------------------------------------------------------##
#########################################################################

data