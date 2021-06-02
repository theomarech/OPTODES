fun_combinaisons_unique <- function(df) {
  df %>%
    dplyr::select(site) %>%
    unique() %>%
    t() %>%
    expand.grid(., .) %>%
    .[!duplicated(t(apply(., 1, sort))), ] %>%
    filter(Var1 != Var2) %>%
    t() %>%
    as_tibble() %>%
    map( ~ as.character(.))
}


fun_boxplot <- function(df, vec, lab,vec_color) {
  ggplot(data = df, aes(x = site, y = sat_od,fill=site))+
    geom_boxplot(outlier.alpha = 0.5, outlier.size = 1) +
    scale_fill_manual(values = vec_color)+
    ggtitle(lab) +
    stat_compare_means(comparisons = vec,
                       label = "p.signif",
                       p.adjust.method = "bonferroni") +
    theme(
      plot.title = element_text(hjust=0.5, size = rel(1)),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill =
                                        "white", colour = "gray40")
    ) +
    ylim(0, 190) +
    geom_hline(yintercept = 100, linetype = 'longdash') +
    xlab(NULL)
}

boxplot_finaux <-
  na.omit(df_oxy_keddy) %>% 
  group_by(mois2) %>% nest() %>% arrange(mois2) %>% mutate(
    vec_color_fill = map(data, ~c("ve_ex" = "#73cacb",
                                  "ve_ab" = "#a6d698",
                                  "nu_ab" = "#ee7e5d",
                                  "nu_ex" = "#e1a4dc")[dplyr::select(.,site) %>% unique() %>% t() %>% as.character()]),
    var_comp = map(data, ~ fun_combinaisons_unique(.)),
    boxplot = pmap(
      list(data, var_comp, mois2,vec_color_fill),
      ~ fun_boxplot(..1, ..2, ..3,..4)
    )
  )
axe_y <- ggplot(data = a$data[[1]], aes(x = site, y = sat_od),color="invisible",fill="invisible")+
  geom_boxplot(color="transparent") +
  ggtitle("lab")+
  theme(
    plot.title = element_text(hjust=0.5, size = rel(1),color="transparent"),
    axis.text.x = element_text(color="transparent",angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill =
                                      "transparent", colour = "transparent"),
    axis.line.y = element_line()
  ) +
  ylim(0, 190) +
  xlab(NULL)+
  scale_y_continuous(limits = c(0, 190), breaks = seq(0, 190, by =20))+ylab("% de saturation en oxygène dissous")


list_plot <- append(list(axe_y),boxplot_finaux$boxplot)

ggarrange(plotlist =list_plot,
          nrow = 1,
          common.legend = TRUE,
          widths = c(0.5,rep(1,14)))                       




