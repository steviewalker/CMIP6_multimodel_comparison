
setwd("~/senior_thesis/plotting_dataframes/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/", pattern = "*_time_series.csv$")

time.series <- lapply(df.sep,read_csv)

df <- time.series %>% reduce(inner_join)
df <- subset(df, select = -c(...1))

df2 <- melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")

figure <- ggplot(data = df2, aes(x = Year, y = POC_flux, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 100m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6)

ggsave(filename = "time_series_100m.png", plot = figure, path = "~/senior_thesis/figures/", width = 20, height = 12, units = "cm", dpi = 400)
