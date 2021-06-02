



liste_conditions <- list(
  AbVe1 <- data %>% filter(Expo == "Ab", Veg == "Ve", Rep =="Rep1"),
  AbNu1 <- data %>% filter(Expo == "Ab", Veg == "Nu", Rep =="Rep1"),
  ExVe1 <- data %>% filter(Expo == "Ex", Veg == "Ve", Rep =="Rep1"),
  ExNu1 <- data %>% filter(Expo == "Ex", Veg == "Nu", Rep =="Rep1"),
  AbVe2 <- data %>% filter(Expo == "Ab", Veg == "Ve", Rep =="Rep2"),
  AbNu2 <- data %>% filter(Expo == "Ab", Veg == "Nu", Rep =="Rep2"),
  ExVe2 <- data %>% filter(Expo == "Ex", Veg == "Ve", Rep =="Rep2"),
  ExNu2 <- data %>% filter(Expo == "Ex", Veg == "Nu", Rep =="Rep2"))


data2 = map(liste_conditions,~select(.,c(2,4,7,10)))










EDM <- function(liste,dimension,librairie,prediction,temps_prediction,tp = 1:10){
  
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM"))
                    
  nombre_var <- length(liste[[1]])-1
  
  tableau_final <- data.frame(variables=c(1:nombre_var),
                              E=c(1:nombre_var),
                              theta=c(1:nombre_var),
                              cor_theta_lin=c(1:nombre_var),
                              cor_theta_max=c(1:nombre_var))
  list_final <- list()
  
  tp = tp
  
  lib <- librairie
  
  pred <- prediction
  
  indice <- 1:nombre_var
  
    for(i in 1 : length(liste)){
      
      nom_condition <- levels(liste[[i]][i,1])[i]
      
      dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM/",nom_condition,"/"))
      
      for(j in 2 : length(liste[[i]])){
        
          variable <- liste[[i]][[j]]
          
          nom_var <- names(liste[[i]][j])
         
          simplex_output <- simplex(variable, lib, pred,E = dimension)
          
          E_max <- simplex_output$E[which.max(simplex_output$rho)]
          
          simplex_pred <- simplex(variable, lib, pred, E = E_max, tp = tp)
          
          smap_output <- s_map(variable, lib, pred, E = E_max)
          
          cor_theta_lineaire <- smap_output$rho[smap_output$theta == 0]
          
          cor_theta_max <- max(smap_output$rho)
          
          theta_select <- smap_output$theta[which.max(smap_output$rho)]
          
          tableau_final[indice[j-1],1] <- nom_var
          tableau_final[indice[j-1],2] <- E_max
          tableau_final[indice[j-1],3] <- theta_select
          tableau_final[indice[j-1],4] <- cor_theta_lineaire
          tableau_final[indice[j-1],5] <- cor_theta_max
          
          
          ###### Les plots
          a <- ggplot(simplex_output,aes(E,rho)) + geom_line(col="orange") +
            geom_point() +
            ylab("Corrélation")+
            xlab("Nombre de dimensions (E)")+
            theme(axis.line  = element_line(linetype = 1,size=rel(0.9),color="grey30"),
                  panel.background = element_rect(fill=NA),
                  axis.text = element_text(size=rel(1),color= "grey30"),
                  axis.text.x = element_text( vjust = 1,hjust = 1),
                  axis.ticks = element_line(size=rel(1),color="grey30"),
                  axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))+
            scale_x_continuous(breaks=c(seq(1,max(dimension),by=1)),limits=c(1,max(dimension)))
          
          b <- ggplot(simplex_pred,aes(tp,rho)) + geom_line(col="orange") +
            geom_point() +
            ylab("Corrélation")+
            xlab("Période de prédiction")+
            theme(axis.line  = element_line(linetype = 1,size=rel(0.9),color="grey30"),
                  panel.background = element_rect(fill=NA),
                  axis.text = element_text(size=rel(1),color= "grey30"),
                  axis.text.x = element_text( vjust = 1,hjust = 1),
                  axis.ticks = element_line(size=rel(1),color="grey30"),
                  axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))+
            scale_x_continuous(breaks=c(seq(1,max(tp),by=1)),limits=c(1,max(tp)))
          
          c <- ggplot(smap_output,aes(theta,rho)) + geom_line(col="orange") +
            geom_point() +
            ylab("Corrélation")+
            xlab("Non linéarité (theta)")+
            theme(axis.line  = element_line(linetype = 1,size=rel(0.9),color="grey30"),
                  panel.background = element_rect(fill=NA),
                  axis.text = element_text(size=rel(1),color= "grey30"),
                  axis.text.x = element_text( vjust = 1,hjust = 1),
                  axis.ticks = element_line(size=rel(1),color="grey30"),
                  axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))
          
          filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM/",
                             nom_condition,
                             "/variable_",
                             nom_var,
                             ".pdf")

          pdf(filename)
               grid.arrange(a,b,c)
          dev.off()
     
      }
      
  list_final[[i]] <- tableau_final
  
  names(list_final)[i] <- paste(nom_condition)
  
  filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM/",
                     nom_condition,
                     "/tableau_final.pdf")
  
  pdf(filename)
         grid.table(tableau_final)
  dev.off()
  
    }
  return(list_final)
}

EDM(liste = data2,
    dimension = c(1:30),
    librairie = c(1,1500),
    prediction = c(1600,2500),
    tp = 1:24)


