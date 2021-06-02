source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")
i = 1
j = 3
liste <- data_1
Ondelettes(liste)
Ondelettes <- function(liste){
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes"))
  for (i in 1 :length(liste)){
    nom_condition <- as.character(liste[[i]][2,2])
    dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/",nom_condition,"/"))
    
    for(j in 3:length(liste[[i]])){
      nom_variable <- colnames(liste[[i]][j])
      variable <- as.data.frame(na.omit(liste[[i]][j]))
      ondelette <- analyze.wavelet(variable,
                                   nom_variable,
                                   dt = 1/24,
                                   upperPeriod = 360)
      plot_ondelette <-  wt.image(ondelette, color.key = "quantile", n.levels = 250, show.date = TRUE, date.format = "%Y %M",
                                  legend.params = list(lab = "wavelet power levels", mar = 4.7))
      
      spectre_puissance <- data.frame(Puissance = ondelette$Power.avg, Periode = ondelette$Period,pval=ondelette$Power.avg.pval)
      spectre_puissance <- spectre_puissance %>% filter(pval<=0.05)
      spectre_puissance <- ggplot(spectre_puissance,aes(Periode,Puissance)) + 
        geom_line(col="red3",size=rel(1)) + 
        geom_point(col="darkblue")+
        xlab("Période (j)")+
        geom_vline(xintercept = seq(0,360,by=20),linetype="longdash",col="grey40", alpha=0.5)+
        theme(axis.line  = element_line(linetype = 1,size=rel(2),color="grey30"),
              panel.background = element_rect(fill=NA),
              axis.text = element_text(size=rel(1),color= "grey30"),
              axis.text.x = element_text( vjust = 1,hjust = 1),
              axis.ticks = element_line(size=rel(1),color="grey30"),
              axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))
      
      filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/Ondelettes/",nom_condition,"/Ondelette_",nom_variable,".pdf")
      pdf(filename)
      print(plot_ondelette)
      print(spectre_puissance)
      dev.off()
    }
  }
}
