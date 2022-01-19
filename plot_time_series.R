
## Time series at 100m ---------------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df <- time.series %>% reduce(inner_join, by = "Year")
#add column for model key
df2 <- melt(df,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")

#plot time series at 100m
figure <- ggplot(data = df2, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 100m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6)

#save figure
ggsave(filename = "time_series_100m.png", plot = figure, path = "~/senior_thesis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## Time-series at MLDmax -------------------


setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.expc <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_expc.csv$")

#create empty list for storing for loop output
time.series2 <- list()

for(i in df.expc) {
  
  #read in csv file
  df.expc <- read_csv(i)
  #get rid of random ...1 column
  df.expc <- subset(df.expc, select = -c(...1))
  #store into list
  time.series2[[i]] <- df.expc
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.expc <- time.series2 %>% reduce(left_join, by = "Year")
#add column for model key
df2.expc <- melt(df.expc,  id.vars = 'Year', value.name = 'POC_flux_expc', variable.name = "Model")

#plot time series at MLDmax
figure2 <- ggplot(data = df2.expc, aes(x = Year, y = POC_flux_expc, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at MLDmax (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6)

#save figure
ggsave(filename = "time_series_expc.png", plot = figure2, path = "~/senior_thesis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# COME BACK TO THIS

## 100m and MLDmax in same panel -----------------

colnames(df) <- 



df.both <- left_join(df2,df2.expc, by = c("Model","Year"))

figure3 <- ggplot(data = df.both) +
  geom_line(aes(x = Year, y = Model, color = Model)) +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at MLDmax (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6)

figure3

