setwd("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/0 - Donnees")


###### PACKAGES ######
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tictoc)
library(stringr)
library(WaveletComp)
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
# ######### Variables ########### #

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


# ######### Ouverture et description du jeux de données ############ #
rm(list=ls()) 

data <- read.table("donnees_fusionnees.csv",h=T,sep=",",dec=".") 

data$date <-ymd_hms(data$date)# conversion des données en format date

data <- na.omit(data)

summary(data) 

head(data)



liste_conditions <- list(
  AbVe1 <- data %>% filter(Expo == "Ab", Veg == "Ve", Rep =="Rep1"),
  AbNu1 <- data %>% filter(Expo == "Ab", Veg == "Nu", Rep =="Rep1"),
  ExVe1 <- data %>% filter(Expo == "Ex", Veg == "Ve", Rep =="Rep1"),
  ExNu1 <- data %>% filter(Expo == "Ex", Veg == "Nu", Rep =="Rep1"),
  AbVe2 <- data %>% filter(Expo == "Ab", Veg == "Ve", Rep =="Rep2"),
  AbNu2 <- data %>% filter(Expo == "Ab", Veg == "Nu", Rep =="Rep2"),
  ExVe2 <- data %>% filter(Expo == "Ex", Veg == "Ve", Rep =="Rep2"),
  ExNu2 <- data %>% filter(Expo == "Ex", Veg == "Nu", Rep =="Rep2"))


levels(liste_conditions[[1]][1,2])[1]


######### Modélisation des phénomènes d'hypoxies ##########
file <- data %>% select(c(7:15,17))
head(data)
file$hypoxie <- as.factor(file$hypoxie)
inTraining <- createDataPartition(file$hypoxie, p = .75, list = FALSE)
training <- file[ inTraining,]
testing  <- file[-inTraining,]
tr_control <- trainControl(method="repeatedcv",
             number = 5,
             repeats = 5,
             classProbs = TRUE,
             summaryFunction = twoClassSummary)


b <- train(hypoxie~.,training,method="ranger",trControl = tr_control)
b <- randomForest(hypoxie~.,training)
pred<-predict(b,testing)
table(pred,testing$hypoxie)
varImpPlot(b)
######### Commentaire globaux sur le jeux de données ##########
# données manquante => na.omit, imputation ? knn, PCA, random forest, fourrier ?
# -> Répétition des conditions => Vincent veut garder les deux conditions et comparer 
#On compare quoi ? les séries temporelles pour chaque conditions ? un test ? connais pas 







plot(ExVe$mean_ox,ExVe$mean_tp)
plot(ExNu$mean_ox,ExNu$expo)
plot(log(AbVe$mean_ox),AbVe$expo)
plot(AbNu$mean_ox,AbNu$mean_tp)


######### Exploration des séries temporelles  ###########

### Plot simple sur les séries de données
a <- ggplot(data,aes(date, mean_ox,col=Rep))+geom_line()+facet_wrap(Expo~Veg)+theme_bw()
b <- ggplot(data,aes(date, mean_ox,col=Rep))+geom_smooth()+facet_wrap(Expo~Veg)+theme_bw()
filename<-paste("C:/users/theo.marechal/Work Folders/Desktop/Plot/serie.pdf")
pdf(filename)
grid.arrange(a,b)
dev.off()

ggplot(data,aes(date, mean_tp))+geom_line()+facet_wrap(Expo~Veg)+theme_bw()
ggplot(data,aes(date, expo))+geom_line()+facet_wrap(Expo~Veg)+theme_bw()

### Autorcorrélation des fonctions
acf2(AbVe$mean_ox,max.lag = 8000)


nrow(AbVe)/2

levels(data$Site)
head(data)
sum(na.omit(data$Pct_sat_ODPct_moy==0))
anoxie <- data%>% filter(Pct_sat_ODPct_moy==0)%>%select(DateFt)
mois <- month(anoxie$DateFt)










###############################################################
#               Ondelettes croisée et spectre de puissance    #
###############################################################




liste_conditions<- list(AbVe,AbNu,ExVe,ExNu)
summary(liste_conditions)





for(i in 1:(length(liste_conditions)-4)){
  
  Rep1 <- na.omit(liste_conditions[[i]])
  Rep2 <- na.omit(liste_conditions[[i+4]])
  tic()
  ondelette_rep1 <- analyze.wavelet(Rep1, "mean_ox",
                        loess.span = 0,
                        upperPeriod = 180,
                        dt = 1/24, dj = 1/250,
                        make.pval = TRUE, n.sim = 1)
  ondelette_rep2 <- analyze.wavelet(Rep2, "mean_ox",
                                    loess.span = 0,
                                    upperPeriod = 180,
                                    dt = 1/24, dj = 1/250,
                                    make.pval = TRUE, n.sim = 1)

  spectre_puissance_rep1 <- data.frame(Puissance = ondelette_rep1$Power.avg, Periode = ondelette_rep1$Period)
  spectre_puissance_rep2 <- data.frame(Puissance = ondelette_rep2$Power.avg, Periode = ondelette_rep2$Period)
  

  toc()
  
  filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/Ondelettes_oxygene_moyen_",Rep1$Expo[1],"__","Vegetation_",Rep1$Veg[1],".pdf") 
  pdf(filename)
       a <- ggplot(spectre_puissance_rep1,aes(Periode,Puissance),col="red")+geom_line(color="red",size=1)+theme_bw()
       b <- wt.image(ondelette_rep1, color.key = "quantile", n.levels = 250, show.date = TRUE, date.format = "%Y %M",
           legend.params = list(lab = "wavelet power levels", mar = 4.7))
       c <- ggplot(spectre_puissance_rep2,aes(Periode,Puissance),col="red")+geom_line(color="red",size=1)+theme_bw()
       d <- wt.image(ondelette_rep2, color.key = "quantile", n.levels = 250, show.date = TRUE, date.format = "%Y %M",
                     legend.params = list(lab = "wavelet power levels", mar = 4.7))
       print(a)
       print(b)
       print(c)
       print(d)
  dev.off()
}













for(i in 1:length(liste_conditions)){
  
  my_data <- na.omit(liste_conditions[[i]])
  tic()
  my.w <- analyze.wavelet(my_data, "expo",
                          loess.span = 0,
                          upperPeriod = 180,
                          dt = 1/24, dj = 1/250,
                          make.pval = TRUE, n.sim = 1)
  
  spectre_puissance <- data.frame(Puissance = my.w$Power.avg, Periode = my.w$Period)
  
  
  toc()
  
  filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/Ondelettes_exposition_",my_data$Expo[1],"__","Vegetation_",my_data$Veg[1],".pdf") 
  pdf(filename)
  a<- ggplot(spectre_puissance,aes(Periode,Puissance),col="red")+geom_line(color="red",size=1)+theme_bw()
  b<-wt.image(my.w, color.key = "quantile", n.levels = 250, show.date = TRUE, date.format = "%Y %M",
              legend.params = list(lab = "wavelet power levels", mar = 4.7))
  print(a)
  print(b)
  dev.off()
}







###############################################################
#                           EDM                               #
###############################################################



###########??? ExVe ##############

#### Oxygène 
ts <- ExVe1$mean_ox
lib <- c(1,1500)
pred <- c(1600,2500)
plot(ts,type="l")
tic()
simplex_output <- simplex(ts, lib, pred,E=c(1:30))
toc()
str(simplex_output)


plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)",
     ylab = "Forecast Skill (rho)")

abline(v=3) ##??? trois dimension pour l'oxygène
simplex_output$E[which.max(simplex_output$rho)]

simplex_output <- simplex(ts, lib, pred, E = 4, tp = 1:10)

plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)",
     ylab = "Forecast Skill (rho)")


smap_output <- s_map(ts, lib, pred, E = 3)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)",
     ylab = "Forecast Skill (rho)")

#### Température

ts <- ExVe1$mean_tp
lib <- c(1,1500)
pred <- c(1600,2500)
plot(ts,type="l")
tic()
simplex_output <- simplex(ts, lib, pred,E=c(1:30))
toc()
str(simplex_output)


plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)",
     ylab = "Forecast Skill (rho)")

abline(v=2) ##??? trois dimension pour l'oxygène


simplex_output <- simplex(ts, lib, pred, E = 4, tp = 1:10)

plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)",
     ylab = "Forecast Skill (rho)")


smap_output <- s_map(ts, lib, pred, E = 3)

plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)",
     ylab = "Forecast Skill (rho)")



#### Vagues
acf2(diff(ExVe1$expo),1000)

ts <- ExVe1$expo
lib <- c(1,1500)
pred <- c(1600,2500)
plot(ts,type="l")
tic()
simplex_output <- simplex(ts, lib, pred,E=c(1:30))
toc()
str(simplex_output)

plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)",
     ylab = "Forecast Skill (rho)")

simplex_output <- simplex(ts, lib, pred, E = 2, tp = 1:10)
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)",
     ylab = "Forecast Skill (rho)")
abline(v=2)

smap_output <- s_map(ts, lib, pred, E = 2)
plot(smap_output$theta, smap_output$rho, xlab = "Nonlinearity (theta)",
     ylab = "Forecast Skill (rho)")

plot(ExVe2$expo,type="l")
anchovy_xmap_sst <- ccm(ExVe1[1:2500,], E = 10, lib_column = "expo",
                        target_column = "mean_ox", lib_sizes = seq(2, 600, by = 50), num_samples = 100,
                        random_libs = TRUE, replace = TRUE)

str(anchovy_xmap_sst)
plot(anchovy_xmap_sst$lib_size,anchovy_xmap_sst$rho)


#### Températures


ts <- ExVe2$mean_ox
lib <- c(1,1500)
pred <- c(1600,2500)
plot(ts,type="l")
tic()
simplex_output <- simplex(ts, lib, pred,E=c(1:30))
toc()
str(simplex_output)
abline(v=2)
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)",
     ylab = "Forecast Skill (rho)")
simplex_output <- simplex(ts, lib, pred, E = 2, tp = 1:10)
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)",
     ylab = "Forecast Skill (rho)")
abline(v=3)

smap_output <- s_map(ts, lib, pred, E = 3)
plot(smap_output$theta, smap_output$rho, xlab = "Nonlinearity (theta)",
     ylab = "Forecast Skill (rho)")

plot(ExVe2$expo,type="l")
anchovy_xmap_sst <- ccm(ExVe2[1:2500,], E = 7, lib_column = "expo",
                        target_column = "mean_ox", lib_sizes = seq(2, 1000, by = 100), num_samples = 100,
                        random_libs = TRUE, replace = TRUE)

str(anchovy_xmap_sst)
plot(anchovy_xmap_sst$lib_size,anchovy_xmap_sst$rho)
cor(ExVe2$mean_tp,ExVe2$mean_ox,method="spearman")
abline(h=0.58)
