source("C:/users/theo.marechal/Work Folders/Desktop/Données STAGE/script_librairies.R")
liste = data_2
scale_var = TRUE
delay_ccm = -1:1
var_ccm = c("mean_conc_ox","mean_temp","expo")
librairies_ccm = seq(10,100,by=10)
i = 1 
j = 1
k = 1
l = 1

CCM(liste = data_2,
    delay_ccm = -1:1,
    var_ccm = c("mean_conc_ox","mean_temp","expo"),
    librairies_ccm = seq(200,2000,by=200))



rm(list=ls()) 



CCM <- function(liste, 
                scale_var = TRUE, 
                delay_ccm,
                var_ccm,
                librairies_ccm = seq(10,80,by=10)){
  
  dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/CCM"))
  
  if(scale_var == TRUE){
    for(h in 1 :length(liste)){
      liste[[h]][,2:length(liste[[h]])] <- as.data.frame(scale(liste[[h]][,2:length(liste[[h]])]))
    }
  }else{
    liste <- liste
  }
  
  nombre_var <- length(liste[[1]])-1
  

  list_final <- list()
  

  indice <- 1:nombre_var
  
  
  
  
  ##### fonction qui créée les combinaisons de variables à tester dans le CCM
  combinaison <- function(vec){
    
    n <- vec %>% expand.grid(vec) %>%  filter(Var1 != Var2)
    
    dft <- data.frame(Var1=rep(0,length(vec)^2-length(vec)),Var2=rep(0,length(vec)^2-length(vec)))
    
    for (i in 1:nrow(n)){
      
      ordre <- as.data.frame(t(n[i,]))
      
      dft[i,] <- sort(ordre[,1])
    }
    
    combinaison <- unique(dft)
    
    return(combinaison)
  }
  
  combinaisons <-  combinaison(var_ccm) # combinaisons contient les combinaisons de variables à teste"r
  
  
  tableau_final <- data.frame(variables = c("mean_temp","mean_conc_ox","expo"),
                              AbNu1 = c(1,2,6),
                              AbNu2 = c(1,3,7),
                              AbVe1 = c(1,4,5),
                              AbVe2 = c(2,4,2),
                              Exnu1 = c(1,1,7),
                              ExNu2 =c(1,3,7),
                              ExVe1 = c(2,4,7),
                              ExVe2 = c(2,3,5))
  
  for(i in 1 : length(liste)){
    
    nom_condition <- levels(liste[[i]][i,1])[i] # nom de la condition analysée i.e un tableau de la liste (e.g AbVe)
    
    dir.create(paste0("C:/users/theo.marechal/Work Folders/Desktop/CCM/",nom_condition,"/"))

    
    for (k in 1:nrow(combinaisons)){
      
      nombre_moyennes <- length(librairies_ccm)
      
      nom_var_un <- combinaisons[k,1]
      nom_var_deux <- combinaisons[k,2]
      
      E_ccm_1 <- as.numeric(tableau_final %>% filter(variables == combinaisons[k,1]) %>% select(nom_condition))
      E_ccm_2 <- as.numeric(tableau_final %>% filter(variables == combinaisons[k,2]) %>% select(nom_condition))

      delay_cor <- data.frame(delay = delay_ccm, ccm_1 = rep(0,length(delay_ccm)),ccm_2 = rep(0,length(delay_ccm)))
      
      for (l in 1:length(delay_ccm)){
        
        ccm_1 <- ccm(liste[[i]], E = E_ccm_1, lib_column = combinaisons[k,1],
                     target_column = combinaisons[k,2], lib_sizes = NROW(liste[[i]]), num_samples = 100,
                     random_libs = TRUE, replace = TRUE, tp = delay_ccm[l])
        
        ccm_2 <- ccm(liste[[i]], E = E_ccm_2, lib_column = combinaisons[k,2],
                     target_column = combinaisons[k,1], lib_sizes = NROW(liste[[i]]), num_samples = 100,
                     random_libs = TRUE, replace = TRUE, tp = delay_ccm[l])
        
        delay_cor[l,2] <- max(ccm_1$rho)
        delay_cor[l,3] <- max(ccm_2$rho)
      }
      

      ccm_1 <- ccm(liste[[i]], E = E_ccm_1, lib_column = combinaisons[k,1],
                   target_column = combinaisons[k,2], lib_sizes = librairies_ccm, num_samples = 100,
                   random_libs = TRUE, replace = TRUE, tp = delay_cor[which.max(delay_cor[,2]),1])
      
      ccm_2 <- ccm(liste[[i]], E = E_ccm_2, lib_column = combinaisons[k,2],
                   target_column = combinaisons[k,1], lib_sizes = librairies_ccm, num_samples = 100,
                   random_libs = TRUE, replace = TRUE, tp = delay_cor[which.max(delay_cor[,3]),1])
      
      a_xmap_t_means <- ccm_means(ccm_1) %>%  mutate( variable = rep(combinaisons[k,1],nombre_moyennes))
      
      t_xmap_a_means <- ccm_means(ccm_2) %>%  mutate( variable = rep(combinaisons[k,2],nombre_moyennes))
      
      tableau_ccm <- bind_rows(a_xmap_t_means,t_xmap_a_means)
      tableau_delay <- delay_cor %>% gather("ccm","cor",2:3)
      
      plot_delay <- ggplot(tableau_delay,aes(delay,cor, colour=ccm))+
        geom_line(size=rel(1))+
        geom_point(size=rel(2))+
        ylab("Corrélation")+
        xlab("Décalage")+
        theme(axis.line  = element_line(linetype = 1,size=rel(0.9),color="grey30"),
              panel.background = element_rect(fill=NA),
              axis.text = element_text(size=rel(1),color= "grey30"),
              axis.text.x = element_text( vjust = 1,hjust = 1),
              axis.ticks = element_line(size=rel(1),color="grey30"),
              axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))+
        scale_colour_discrete(labels = c(paste(combinaisons[k,2],"cause",combinaisons[k,1]),
                                         paste(combinaisons[k,1],"cause",combinaisons[k,2])))
      
      
      plot_ccm <-  ggplot(tableau_ccm,aes(lib_size,rho, colour=variable))+
        geom_line(size=rel(1))+
        geom_point(size=rel(2))+
        ylab("Corrélation")+
        xlab("Taille de la librairie")+
        theme(axis.line  = element_line(linetype = 1,size=rel(0.9),color="grey30"),
              panel.background = element_rect(fill=NA),
              axis.text = element_text(size=rel(1),color= "grey30"),
              axis.text.x = element_text( vjust = 1,hjust = 1),
              axis.ticks = element_line(size=rel(1),color="grey30"),
              axis.title = element_text( size = rel(1),hjust=0.5,vjust=4,colour="grey30"))+
        scale_colour_discrete(labels = c(paste(combinaisons[k,2],"cause",combinaisons[k,1],"|décalage = ",delay_cor[which.max(delay_cor[,2]),1]),
                                         paste(combinaisons[k,1],"cause",combinaisons[k,2], "|décalage =", delay_cor[which.max(delay_cor[,3]),1])))
      
      filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/CCM/",
                         nom_condition,
                         "/CCM_",
                         paste0(combinaisons[k,1],"_",combinaisons[k,2]),
                         ".pdf")
      
      pdf(filename)
      grid.arrange(plot_delay,plot_ccm)
      dev.off()
    } ### fermeture de la boucle sur les ccm
    
  } ### fermeture de la boucle sur les conditions
  return(list_final)
  
} ### fermeture de la fonction



