rm(list=ls()) # Reinitialistion de la console R

getwd() # Connaitre l'environnement de travail
setwd("C:/DATA/2018 - Stage CARMA/Travail sous R/Donnees/") # changer l'environnement de travail

## chargement packages necessaires
library(lubridate)


# IMPORTATION DES DIFFERENTES DONNEES
#===================================================================
## AbNu : sans plantes, a l'abri du vent (donnees ttes les 30 min)
#===================================================================

Data_AbNu1<-read.csv2("AbNu1_MP.csv", header=T)
summary(Data_AbNu1)

## creation nouvelle variable date avec le bon format date : jour/mois/annee heure:minutes 
Data_AbNu1$DateFt<-strptime(Data_AbNu1$Date, "%d/%m/%y %H:%M",tz="GMT")
summary(Data_AbNu1)

## changement du format des variables Temp et Pct_sat_ODPPct
Data_AbNu1$Temp<-as.numeric(as.character(Data_AbNu1$Temp))
Data_AbNu1$Pct_sat_ODPct<-as.numeric(as.character(Data_AbNu1$Pct_sat_ODPct))
summary(Data_AbNu1)

#-----
Data_AbNu2<-read.csv2("AbNu2_MP.csv", header=T)
summary(Data_AbNu2)

Data_AbNu2$DateFt<-strptime(Data_AbNu2$Date, "%d/%m/%y %H:%M",tz="GMT")
summary(Data_AbNu2)
Data_AbNu2$Temp<-as.numeric(as.character(Data_AbNu2$Temp))
Data_AbNu2$Pct_sat_ODPct<-as.numeric(as.character(Data_AbNu2$Pct_sat_ODPct))
summary(Data_AbNu2)


#================================================================
## AbVeg : herbier, a l'abri du vent (donnees ttes les 30 min)
#================================================================

Data_AbVeg1<-read.csv("AbVeg1_MP.csv", header=T, sep=";")
summary(Data_AbVeg1)

Data_AbVeg1$DateFt<-strptime(Data_AbVeg1$Date, "%d/%m/%y %H:%M",tz="GMT")
summary(Data_AbVeg1)
Data_AbVeg1$Temp<-as.numeric(as.character(Data_AbVeg1$Temp))
Data_AbVeg1$Pct_sat_ODPct<-as.numeric(as.character(Data_AbVeg1$Pct_sat_ODPct))
summary(Data_AbVeg1)

#-----
Data_AbVeg2<-read.csv("AbVeg2_MP.csv", header=T, sep=";")
summary(Data_AbVeg2)

Data_AbVeg2$DateFt<-strptime(as.character(Data_AbVeg2$Date), "%d/%m/%Y %H:%M",tz="GMT")
summary(Data_AbVeg2)
Data_AbVeg2$Temp<-as.numeric(as.character(Data_AbVeg2$Temp))
Data_AbVeg2$Pct_sat_ODPct<-as.numeric(as.character(Data_AbVeg2$Pct_sat_ODPct))
summary(Data_AbVeg2)


#================================================================
## ExNu : herbier, a l'abri du vent (donnees ttes les 30 min)
#================================================================

Data_ExNu1<-read.csv("ExNu1_MP.csv", header=T, sep=";")
summary(Data_ExNu1)

Data_ExNu1$DateFt<-strptime(Data_ExNu1$Date, "%d/%m/%y %H:%M",tz="GMT")
summary(Data_ExNu1)
Data_ExNu1$Temp<-as.numeric(as.character(Data_ExNu1$Temp))
Data_ExNu1$Pct_sat_ODPct<-as.numeric(as.character(Data_ExNu1$Pct_sat_ODPct))
summary(Data_ExNu1)

#-----
Data_ExNu2<-read.csv("ExNu2_MP.csv", header=T, sep=";")
summary(Data_ExNu2)

Data_ExNu2$DateFt<-strptime(as.character(Data_ExNu2$Date), "%d/%m/%y %H:%M",tz="GMT")
summary(Data_ExNu2)
Data_ExNu2$Temp<-as.numeric(as.character(Data_ExNu2$Temp))
Data_ExNu2$Pct_sat_ODPct<-as.numeric(as.character(Data_ExNu2$Pct_sat_ODPct))
summary(Data_ExNu2)


#================================================================
## ExVeg : herbier, a l'abri du vent (donnees ttes les 30 min)
#================================================================

Data_ExVeg1<-read.csv2("ExVeg1_MP.csv", header=T, sep=";")
summary(Data_ExVeg1)

Data_ExVeg1$DateFt<-strptime(Data_ExVeg1$Date, "%d/%m/%Y %H:%M",tz="GMT")
summary(Data_ExVeg1)
Data_ExVeg1$Temp<-as.numeric(as.character(Data_ExVeg1$Temp))
Data_ExVeg1$Pct_sat_ODPct<-as.numeric(as.character(Data_ExVeg1$Pct_sat_ODPct))
summary(Data_ExVeg1)

#-----
Data_ExVeg2<-read.csv2("ExVeg2_MP.csv", header=T, sep=";")
summary(Data_ExVeg2)

Data_ExVeg2$DateFt<-strptime(as.character(Data_ExVeg2$Date), "%d/%m/%Y %H:%M",tz="GMT")
summary(Data_ExVeg2)
Data_ExVeg2$Temp<-as.numeric(as.character(Data_ExVeg2$Temp))
Data_ExVeg2$Pct_sat_ODPct<-as.numeric(as.character(Data_ExVeg2$Pct_sat_ODPct))
summary(Data_ExVeg2)



#============================================================================
## Creation BDD contenant tous les sites ensemble (donnees ttes les heures)
#============================================================================

## collage des 8 jeux de donnees precedents les uns a la suite des autres (en ligne)
Data_All_Site<-rbind(Data_AbNu1,Data_AbNu2,Data_AbVeg1,Data_AbVeg2,Data_ExNu1,Data_ExNu2,Data_ExVeg1,Data_ExVeg2)

## creation d'une variable Site 
Data_All_Site$Site<-as.factor(c(rep("AbNu1",dim(Data_AbNu1)[1]),rep("AbNu2",dim(Data_AbNu2)[1]),rep("AbVeg1",dim(Data_AbVeg1)[1]),rep("AbVeg2",dim(Data_AbVeg2)[1]),
                      rep("ExNu1",dim(Data_ExNu1)[1]),rep("ExNu2",dim(Data_ExNu2)[1]),rep("ExVeg1",dim(Data_ExVeg1)[1]),rep("ExVeg2",dim(Data_ExVeg2)[1])))

## creation d'une variable date en enlevant les minutes pour creation d'une BDD contenant donnees a l'heure
## donc au format : jour/mois/annee heure
Data_All_Site$Date2<-strptime(as.character(Data_All_Site$DateFt), "%Y-%m-%d %H",tz="GMT")
Data_All_Site$Site_Date2<-factor(paste(Data_All_Site$Site,Data_All_Site$Date2))

## supression des doublons
Data_All_Site2<-unique(Data_All_Site[,c("Site","Date2","Site_Date2")])


## creation BDD contenant les calculs de la saturation par heure (moyenne, minimum, maximum)
Data_PctODP_Temp_Hour<-data.frame(Site_Date2=row.names(tapply(Data_All_Site$Pct_sat_ODPct,Data_All_Site$Site_Date2, mean)),
                             Pct_sat_ODPct_moy=tapply(Data_All_Site$Pct_sat_ODPct,Data_All_Site$Site_Date2, function(x) mean(x, na.rm=T)),
                             Pct_sat_ODPct_min=tapply(Data_All_Site$Pct_sat_ODPct,Data_All_Site$Site_Date2, function(x) min(x, na.rm=T)),
                             Pct_sat_ODPct_max=tapply(Data_All_Site$Pct_sat_ODPct,Data_All_Site$Site_Date2, function(x) max(x, na.rm=T)),
                             Temp_moy=tapply(Data_All_Site$Temp,Data_All_Site$Site_Date2, function(x) mean(x, na.rm=T)),
                             Temp_min=tapply(Data_All_Site$Temp,Data_All_Site$Site_Date2, function(x) min(x, na.rm=T)),
                             Temp_max=tapply(Data_All_Site$Temp,Data_All_Site$Site_Date2, function(x) max(x, na.rm=T)))
summary(Data_PctODP_Temp_Hour)

## remplacement des -Inf et Inf par des NAs
Data_PctODP_Temp_Hour$Pct_sat_ODPct_moy<-as.numeric(apply(Data_PctODP_Temp_Hour$Pct_sat_ODPct_moy,1,function(x) ifelse(is.infinite(x),"NA",x)))
Data_PctODP_Temp_Hour$Pct_sat_ODPct_min<-as.numeric(apply(Data_PctODP_Temp_Hour$Pct_sat_ODPct_min,1,function(x) ifelse(is.infinite(x),"NA",x)))
Data_PctODP_Temp_Hour$Pct_sat_ODPct_max<-as.numeric(apply(Data_PctODP_Temp_Hour$Pct_sat_ODPct_max,1,function(x) ifelse(is.infinite(x),"NA",x)))
Data_PctODP_Temp_Hour$Temp_moy<-as.numeric(apply(Data_PctODP_Temp_Hour$Temp_moy,1,function(x) ifelse(is.infinite(x),"NA",x)))
Data_PctODP_Temp_Hour$Temp_min<-as.numeric(apply(Data_PctODP_Temp_Hour$Temp_min,1,function(x) ifelse(is.infinite(x),"NA",x)))
Data_PctODP_Temp_Hour$Temp_max<-as.numeric(apply(Data_PctODP_Temp_Hour$Temp_max,1,function(x) ifelse(is.infinite(x),"NA",x)))


## concatenation des infos par site/date (en heure) 
Data_All_Site2<-merge(Data_All_Site2,Data_PctODP_Temp_Hour, by="Site_Date2", all.x=T)
names(Data_All_Site2)[3]<-"DateFt" # renommage de la variable
head(Data_All_Site2)
Data_All_Site2$DateFt2<-paste(Data_All_Site2$DateFt)

summary(Data_All_Site2)

#================================================================
## Donnees Vent Biscarosse (donnees ttes les heures)
#================================================================

Data_Vent_Bisca<-read.csv2("Data_vent_Bisca.data.csv", header=T, sep=";")
summary(Data_Vent_Bisca)

## creation nouvelle variable date avec le bon format date : jour/mois/annee heure:00
Data_Vent_Bisca$DateFt<-paste(Data_Vent_Bisca$Day,"/",Data_Vent_Bisca$Month,"/",Data_Vent_Bisca$Year," ",Data_Vent_Bisca$Hour,":00", sep="")
Data_Vent_Bisca$DateFt<-strptime(as.character(Data_Vent_Bisca$DateFt), "%d/%m/%Y %H:%M",tz="GMT")

## Decalage d'une heure des donnees car HXY indique qu'il s'agit des donnees recoltees une heure auparavant
## par ex a 10h alors que la date de la ligne est 11h
Data_Vent_Bisca$DateFt_corrig<-Data_Vent_Bisca$DateFt-hours(1)

head(Data_Vent_Bisca)
Data_Vent_Bisca$DateFt2<-paste(Data_Vent_Bisca$DateFt_corrig)

#===============================================================================
## Donnees Fetch (donnee pour chaque site en fonction de la direction du vent)
#===============================================================================

Data_Fetch<-read.csv2("Fetch_PAR40_Optodes_Hydrophytes.csv", header=T, sep=";")
summary(Data_Fetch)

Data_Fetch$fetch<-as.numeric(paste(Data_Fetch$fetch))

## creation d'une variable contenant le site et la direction du vent (pour calcul exposition utlerieur)
Data_Fetch$Site_Direction<-factor(paste(Data_Fetch$site,Data_Fetch$direction,sep="_"))

head(Data_Fetch)

###################################################################################################################
###################### Creation BDD Finale #############################################################
###################################################################################################################

# Concatenation donnees fetch, vent et biologiques

#===============================================================================================================
## Regroupement donnees biologiques et donnees vent (FXY : vitesse du vent ; DXY : direction du vent) par date
#===============================================================================================================

Data_All_Site_Vent_Bisca<-merge(Data_All_Site2,Data_Vent_Bisca[,c("DateFt2","FXY","DXY")], by="DateFt2")
Data_All_Site_Vent_Bisca$Site_Direction<-factor(paste(Data_All_Site_Vent_Bisca$Site, Data_All_Site_Vent_Bisca$DXY, sep="_"))

head(Data_All_Site_Vent_Bisca, 10)

#===========================================================================================
## Regroupement donnees biologiques + vent avec donnees fetch par site et direction du vent
#===========================================================================================

Data_All_Site_Fetch<-merge(Data_All_Site_Vent_Bisca,Data_Fetch[, c("Site_Direction","fetch")], by="Site_Direction", all.x=T)
head(Data_All_Site_Fetch)
summary(Data_All_Site_Fetch)

## Calcul de l'exposition : Fetch x vitesse du vent (FXY)
Data_All_Site_Fetch$Expo<-Data_All_Site_Fetch$fetch*Data_All_Site_Fetch$FXY
summary(Data_All_Site_Fetch)

Data_All_Site_Fetch[with(Data_All_Site_Fetch, order(Data_All_Site_Fetch$Expo)),]
#===========================================================================================
## Export .csv du jeu de donnees final
#===========================================================================================

write.csv2(Data_All_Site_Fetch[,c("Site","DateFt","Pct_sat_ODPct_moy","Pct_sat_ODPct_min","Pct_sat_ODPct_max",
                                  "Temp_moy","Temp_min","Temp_max","Expo")],"Data_All_Site_Fetch.csv", row.names=F)

