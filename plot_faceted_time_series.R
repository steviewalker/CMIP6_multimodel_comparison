## 5. PLOTTING NORMALIZED 100M AND MLDMAX IN SAME PLOT -----------------

#position = c(1:6)

#output_figures <- facet_wrap(facets = position, nrow = 3, ncol = 2)

faceted_ts <- function(model.name) {
 
  df <- df.both %>% 
    dplyr::select(Year, model.name, paste0(model.name,"_expc")) %>%
    gather(key = "Depth_horizon", value = "POC_flux", -Year)

figure <- ggplot(data = df) +
  geom_line(aes(x = Year, y = POC_flux, color = Depth_horizon)) +
  geom_smooth(aes(x = Year, y = POC_flux, color = Depth_horizon), size = 0.7, se = FALSE) +
  theme_bw() +
  labs(title = model.name) +
  xlab(NULL) +
  ylab("Percent Change in POC Flux") +
  scale_y_continuous(n.breaks = 6) +
  scale_color_discrete(name = "Depth Horizon", labels = c("100m", "MLDmax"))

figure 

ggsave(filename = paste0(model.name,"_time_series_DH_comparison.png"), plot = figure, path = "~/senior_thesis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

}
