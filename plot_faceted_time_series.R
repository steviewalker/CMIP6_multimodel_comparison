## 5. PLOTTING NORMALIZED 100M, MLDMAX, and 1000m IN SAME PLOT -----------------

#position = c(1:6)

#output_figures <- facet_wrap(facets = position, nrow = 3, ncol = 2)

faceted_ts <- function(model.name) {
  
  df <- df.all %>% 
    dplyr::select(Year, model.name, paste0(model.name,"_expc"), paste0(model.name,"_1000")) %>%
    gather(key = "Depth_horizon", value = "POC_flux", -Year)
  
  color <- c("dodgerblue2", "mediumpurple2", "darkorange1")
  
  figure <- ggplot(data = df) +
    geom_line(aes(x = Year, y = POC_flux, color = Depth_horizon)) +
    geom_smooth(aes(x = Year, y = POC_flux, color = Depth_horizon), size = 0.7, se = FALSE) +
    theme_bw() +
    labs(title = model.name) +
    xlab(NULL) +
    ylab("Percent Change in POC Flux") +
    scale_y_continuous(n.breaks = 6) +
    #scale_color_discrete(name = "Depth Horizon", labels = c("100m", "MLDmax", "1000m")) +
    scale_color_manual(name = "Depth Horizon", labels = c("100m", "MLDmax", "1000m"), values = color) +
    theme(legend.position = "none")
    #uncomment this for legend plot (MPI)
    #theme(legend.position = c(0.87,0.82),
    #      legend.key.size = unit(1, 'cm'), 
    #      legend.key.height = unit(0.8, 'cm'), 
    #      legend.key.width = unit(1, 'cm'), 
    #      legend.title = element_text(size=14), 
    #      legend.text = element_text(size=12))
  
  
  figure 
  
  
  ggsave(filename = paste0(model.name,"_time_series_DH_comparison.png"), plot = figure, path = "~/senior_thesis/figures/time_series", width = 16, height = 12, units = "cm", dpi = 400)
  
}
