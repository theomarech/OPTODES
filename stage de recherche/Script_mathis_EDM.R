

file <- file %>% select(-c("salinite","turbidite"))


EDM <- function(df,dimension,librairie,prediction,temps_prediction,tp 
                = 1:10,var_ccm,taille_librairie_ccm=seq(10,80,by=10)){
  
  dir.create(paste0("C:/Users/Mathis/Documents/Stage M2/Test R/EDM"))
  
  nombre_var <- length(df)
  
  tableau_final <- data.frame(variables=c(1:nombre_var),
                              E=c(1:nombre_var),
                              theta=c(1:nombre_var),
                              cor_theta_lin=c(1:nombre_var),
                              cor_theta_max=c(1:nombre_var))
  tp = tp
  
  lib <- librairie
  
  pred <- prediction
  
  ##### fonction qui créée les combinaisons de variables à tester dans le CCM
  combinaison <- function(vec){
    n <- vec %>% expand.grid(vec) %>%  filter(Var1 != Var2)
    dft <- 
      data.frame(Var1=rep(0,length(vec)^2-length(vec)),Var2=rep(0,length(vec)^2-length(vec)))
    for (i in 1:nrow(n)){
      ordre <- as.data.frame(t(n[i,]))
      dft[i,] <- sort(ordre[,1])
    }
    combinaison <- unique(dft)
    return(combinaison)
  }
  
  combinaisons <-  combinaison(var_ccm) # combinaisons contient les combinaisons de variables à teste"r
  
  
  for(i in 1 : length(df)){
    
    nom_var <- names(df)[i]
    
    variable <- as.list(df)[[i]]
    
    simplex_output <- simplex(variable, lib, pred, E = dimension)
    
    E_max <- simplex_output$E[which.max(simplex_output$rho)]
    
    simplex_pred <- simplex(variable, lib, pred, E = E_max, tp = tp)
    
    smap_output <- s_map(variable, lib, pred, E = E_max)
    
    cor_theta_lineaire <- smap_output$rho[smap_output$theta == 0]
    
    cor_theta_max <- max(smap_output$rho)
    
    theta_select <- smap_output$theta[which.max(smap_output$rho)]
    
    
    tableau_final[i,1] <- nom_var
    tableau_final[i,2] <- E_max
    tableau_final[i,3] <- theta_select
    tableau_final[i,4] <- cor_theta_lineaire
    tableau_final[i,5] <- cor_theta_max
    
    
    ###### Les plots
    a <- ggplot(simplex_output,aes(E,rho)) + geom_line(col="orange") +
      geom_point() +
      ylab("Corrélation")+
      xlab("Nombre de dimensions (E)")+
      theme(axis.line  = element_line(linetype = 
                                        1,size=rel(0.9),color="grey30"),
            panel.background = element_rect(fill=NA),
            axis.text = element_text(size=rel(1),color= "grey30"),
            axis.text.x = element_text( vjust = 1,hjust = 1),
            axis.ticks = element_line(size=rel(1),color="grey30"),
            axis.title = element_text( size = 
                                         rel(1),hjust=0.5,vjust=4,colour="grey30"))+
      scale_x_continuous(breaks=c(seq(1,max(dimension),by=1)),limits=c(1,max(dimension)))
    
    b <- ggplot(simplex_pred,aes(tp,rho)) + geom_line(col="orange") +
      geom_point() +
      ylab("Corrélation")+
      xlab("Période de prédiction")+
      theme(axis.line  = element_line(linetype = 
                                        1,size=rel(0.9),color="grey30"),
            panel.background = element_rect(fill=NA),
            axis.text = element_text(size=rel(1),color= "grey30"),
            axis.text.x = element_text( vjust = 1,hjust = 1),
            axis.ticks = element_line(size=rel(1),color="grey30"),
            axis.title = element_text( size = 
                                         rel(1),hjust=0.5,vjust=4,colour="grey30"))+
      scale_x_continuous(breaks=c(seq(1,max(tp),by=1)),limits=c(1,max(tp)))
    
    c <- ggplot(smap_output,aes(theta,rho)) + geom_line(col="orange") +
      geom_point() +
      ylab("Corrélation")+
      xlab("Non linéarité (theta)")+
      theme(axis.line  = element_line(linetype = 
                                        1,size=rel(0.9),color="grey30"),
            panel.background = element_rect(fill=NA),
            axis.text = element_text(size=rel(1),color= "grey30"),
            axis.text.x = element_text( vjust = 1,hjust = 1),
            axis.ticks = element_line(size=rel(1),color="grey30"),
            axis.title = element_text( size = 
                                         rel(1),hjust=0.5,vjust=4,colour="grey30"))
    
    filename <- paste0("C:/users/theo.marechal/Work Folders/Desktop/EDM2/variable_",
                       nom_var,
                       ".pdf")
    
    pdf(filename)
    grid.arrange(a,b,c)
    dev.off()
  }
  
  
  filename <- paste0("C:/Users/Mathis/Documents/Stage M2/Test R/EDM/tableau.pdf")
  pdf(filename)
  grid.table(tableau_final)
  dev.off()
  
  
  for (k in 1:nrow(combinaisons)){
    
    nom_var_un <- combinaisons[k,1]
    nom_var_deux <- combinaisons[k,2]
    
    E_ccm_1 <- tableau_final %>% filter(variables == 
                                          combinaisons[k,1]) %>% select(E)
    E_ccm_2 <- tableau_final %>% filter(variables == 
                                          combinaisons[k,2]) %>% select(E)
    
    ccm_1 <- ccm(df, E = E_ccm_1, lib_column = combinaisons[k,1],
                 target_column = combinaisons[k,2], lib_sizes = 
                   taille_librairie_ccm, num_samples = 100,
                 random_libs = TRUE, replace = TRUE)
    
    ccm_2 <- ccm(df, E = E_ccm_2, lib_column = combinaisons[k,2],
                 target_column = combinaisons[k,1], lib_sizes = 
                   taille_librairie_ccm, num_samples = 100,
                 random_libs = TRUE, replace = TRUE)
    
    a_xmap_t_means <- ccm_means(ccm_1) %>%  mutate( variable = 
                                                      rep(combinaisons[k,1],nrow(a_xmap_t_means)))
    
    t_xmap_a_means <- ccm_means(ccm_2) %>%  mutate( variable = 
                                                      rep(combinaisons[k,2],nrow(t_xmap_a_means)))
    
    tableau_ccm <- bind_rows(a_xmap_t_means,t_xmap_a_means)
    
    
    plot_ccm <-  ggplot(tableau_ccm,aes(lib_size,rho, colour=variable))+
      geom_line(size=rel(1))+
      geom_point(size=rel(2))+
      ylab("Corrélation")+
      xlab("Taille de la librairie")+
      theme(axis.line  = element_line(linetype = 
                                        1,size=rel(0.9),color="grey30"),
            panel.background = element_rect(fill=NA),
            axis.text = element_text(size=rel(1),color= "grey30"),
            axis.text.x = element_text( vjust = 1,hjust = 1),
            axis.ticks = element_line(size=rel(1),color="grey30"),
            axis.title = element_text( size = 
                                         rel(1),hjust=0.5,vjust=4,colour="grey30"))+
      scale_colour_discrete(labels = 
                              c(paste(combinaisons[k,2],"cause",combinaisons[k,1]),
                                paste(combinaisons[k,1],"cause",combinaisons[k,2])))
    
    filename <- paste0("C:/Users/Mathis/Documents/Stage M2/Test R/EDM/CCM_",
                       paste0(combinaisons[k,1],"_",combinaisons[k,2]),
                       ".pdf")
    
    pdf(filename)
    print(plot_ccm)
    dev.off()
  }
}
EDM(df = file,
    dimension = c(1:40),
    librairie = c(1,150),
    prediction = c(151,391),
    tp = 1:24,
    var_ccm = c("LOU","GOB","MUP"))
