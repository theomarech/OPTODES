library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tictoc)
library(stringr)
library(WaveletComp)
library(rEDM)
library(purrr)
library(readxl)


########## Variables vents ############
# DXY : Direction vent quotidien maxi moyenne sur 10 minute (rose des sable 0 à 360)
# FXY : Vitesse vent quotidien maxi moyenne sur 10 minute (m.s-1)
# HXY : Heure vent quotidien maxi moyenne sur 10 minute (m.s-1)

########## transformation des données : fusion des tables et export en csv ###################



############################ Données oxygènes SATURATION #########################
#--------------------------------------------------------------------------------#
##################################################################################
dir()
data <- read.table("Data_All_Site_Fetch.csv",h=T,sep=";",dec=",") # données sauf vent
vent <- read.table("Data_vent_Bisca.data.csv",h=T,sep=";",dec=",") # données vent

names(data)[2]<- "date"
names(vent)[2]<- "date"

# Conversion en format date
vent$date <- ymd_h(vent$date)
data$date <- ymd_hms(data$date) 

vent_2<-vent %>% select(c(2,7:9))# sélection des variables d'intérêts
vent_2 <- vent_2[1:nrow(vent)-1,]
final <- right_join(data,vent_2,by="date") # fusion des tables sur date
colnames(final) <- c("site","date","mean_ox","min_ox","max_ox","mean_tp","min_tp","max_tp","expo","FXY","DXY","HXY")
final<- final %>% mutate(Expo= str_sub(site,1,2), Veg = str_sub(site,3,4), Rep = str_sub(site,-1)) %>% mutate(Rep = ifelse(Rep == 1, "Rep1", "Rep2"))


write.csv(final,file="donnees_fusionnees.csv") # export du tableau final en csv







############################ Données oxygènes dissous ############################
#--------------------------------------------------------------------------------#
##################################################################################
setwd("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/V2_Cristina")

conditions <- expand.grid(c("Ab","Ex"),c("Nu","Ve"),c(1,2))
conditions <- sort(str_c(conditions[,1],conditions[,2],conditions[,3]))

liste_conditions <- list(
AbNu1 <- read_xlsx(dir()[1]),
AbNu2 <- read_xlsx(dir()[2]),
AbVe1 <- read_xlsx(dir()[3]),
AbVe2 <- read_xlsx(dir()[4]),
ExNu1 <- read_xlsx(dir()[7]),
ExNu2 <- read_xlsx(dir()[8]),
ExVe1 <- read_xlsx(dir()[9]),
ExVe2 <- read_xlsx(dir()[10]))
names(liste_conditions) <- conditions
liste_conditions <- map(liste_conditions,~select(.,c(1,3,4,5)))

 for ( i in 1:length(conditions)){
   liste_conditions[[i]] <- liste_conditions[[i]] %>% mutate(site = rep(conditions[i],nrow(liste_conditions[[i]])))
 }

data_conc <- bind_rows(liste_conditions) %>% select(c(5,1,2,3,4))

names(data_conc) <- c("site","date","temp","sat_ox","conc_ox")

data_conc$date <- round_date(data_conc$date,unit="houre")

data_mean <- data_conc %>%  group_by(date,site) %>% summarise_all(mean) %>% rename(mean_conc_ox=conc_ox,
                                                                                  mean_temp=temp,
                                                                                  mean_sat_ox=sat_ox)
data_min <- data_conc %>%  group_by(date,site) %>% summarise_all(min) %>% rename(min_conc_ox=conc_ox,
                                                                                  min_temp=temp,
                                                                                  min_sat_ox=sat_ox)
data_max <- data_conc %>%  group_by(date,site) %>% summarise_all(max) %>% rename(max_conc_ox=conc_ox,
                                                                                 max_temp=temp,
                                                                                 max_sat_ox=sat_ox)
data <- bind_cols(data_min,data_mean,data_max)
data_conc <- data %>% select(-c(date1,date2,site1,site2)) %>% select(date,site,min_temp,mean_temp,max_temp,min_conc_ox,mean_conc_ox,max_conc_ox,min_sat_ox,mean_sat_ox,max_sat_ox)



setwd("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/0 - Donnees")

data <- read.table("Data_All_Site_Fetch.csv",h=T,sep=";",dec=",") # données sauf vent
vent <- read.table("Data_vent_Bisca.data.csv",h=T,sep=";",dec=",") # données vent
inso <- read.table("données_insolations.txt",sep="\t",dec=',',header=TRUE)
colnames(inso)

names(inso)[2] <- "date"
names(data)[2]<- "date"
names(vent)[2]<- "date"
names(data)[1]<- "site"
data$site <- str_replace(data$site,"g","")
data$site <- as.factor(data$site)
# Conversion en format date
vent$date <- ymd_h(vent$date)
data$date <- ymd_hms(data$date) 
inso$date <- ymd_h(inso$date)



vent<-vent %>% select(c(2,7:9))


jointure_expo <- left_join(data_conc,data,by=c("site","date"))
jointure_vent <- left_join(jointure_expo,vent,by="date")
jointure_inso <- left_join(jointure_vent,inso,by="date")
arrange(jointure_inso,date)
jointure_final <- jointure_inso[-which(jointure_inso$site=="ExVe1" & jointure_inso$date>"2017-04-05 10:00:00 UTC"),]


final <- jointure_final %>% select(-c("POSTE","Pct_sat_ODPct_moy","Pct_sat_ODPct_min","Pct_sat_ODPct_max","Temp_moy","Temp_min","Temp_max"))
final <- rename(final,expo = Expo,temp_air = T,duree_inso = INS,ray_glo = GLO)
final<- final %>% mutate(expo_vent= str_sub(site,1,2), veg = str_sub(site,3,4), rep = str_sub(site,-1)) %>% mutate(rep = ifelse(rep == 1, "Rep1", "Rep2"))

summary(final)
write.csv(final,file="donnees_fusionnees_conc.csv") # export du tableau final en csv


