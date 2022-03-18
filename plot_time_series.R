#' @title Plotting Globally Integrated Time Series Data
#' @author Stevie Walker
#' @date 1/25/22
#' @description plots 4 figures of globally integrated time series at 100m, MLDmax, normalized 100m, and normalized MLDmax
#' @description also saves tidied and combined time series data frames

#colors for time series
color = c("violet", "goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "royalblue2")

## 1. TIME SERIES AT 100m ---------------

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
df <- time.series %>% 
  reduce(left_join, by = "Year")
#delete random extra 2015 rows
df <- df[-c(166:196), ]

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_epc100_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")


#plot time series at 100m
figure <- ggplot(data = df2, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 100m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +  
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

#save figure
ggsave(filename = "time_series_100m.png", plot = figure, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


## 2. TIME SERIES AT MLDMAX -------------------


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

#add UKESM to list separately since it doesn't have the "...1" column
time.series2$UKESM_time_series.csv = as.tibble(read_csv("UKESM_time_series_expc.csv"))

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.expc <- time.series2 %>% 
  reduce(left_join, by = "Year")

#add column for model key
df2.expc <- data.table::melt(df.expc,  id.vars = 'Year', value.name = 'POC_flux_expc', variable.name = "Model")

#save POC flux at MLDmax time-series data frame for all models
write_csv(df.expc, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_expc_all.csv")

#plot time series at MLDmax
figure2 <- ggplot(data = df2.expc, aes(x = Year, y = POC_flux_expc, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at MLDmax (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure2

#save figure
ggsave(filename = "time_series_expc.png", plot = figure2, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


## 3. NORMALIZED POC FLUX AT 100M RELATIVE TO 1850-1900 MEAN ----------------


#read in df created for first figure
epc100.all <- read_csv("~/senior_thesis/plotting_dataframes/time_series/time_series_epc100_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(epc100.all, Year <= 1900) %>% 
  summarise_if(is.numeric, mean)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.epc100 <- 
  subset(epc100.all, select = -c(1))

#calculate normalized POC flux
normalized.epc100 <- mapply('/', normalized.epc100, mean1850_1900)*100

normalized.epc100 <- normalized.epc100 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.epc100, "~/senior_thesis/plotting_dataframes/time_series/normalized_time_series_epc100.csv")

#add column for model key (reformatting data specific to the below plot)
plot.normalized.epc100 <- data.table::melt(normalized.epc100,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")

#plot time series at 100m
figure3 <- ggplot(data = plot.normalized.epc100, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at 100m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_y_continuous(limits = c(75, 110), n.breaks = 6) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))
figure3

#save figure
ggsave(filename = "normalized_time_series_100m.png", plot = figure3, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


## 4. NORMALIZED POC FLUX AT MLD MAX RELATIVE TO 1850-1900 AVG --------------


#read in df created for first figure
expc.all <- read_csv("~/senior_thesis/plotting_dataframes/time_series/time_series_expc_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized POC flux
normalized.expc <- mapply('/', normalized.expc, mean1850_1900)*100

normalized.expc <- normalized.expc %>% 
  cbind(Year = c(1849:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.expc, "~/senior_thesis/plotting_dataframes/time_series/normalized_time_series_expc.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.expc <- data.table::melt(normalized.expc,  id.vars = 'Year', value.name = 'POC_flux_MLDmax', variable.name = "Model")

#plot time series at 100m
figure4 <- ggplot(data = plot.normalized.expc, aes(x = Year, y = POC_flux_MLDmax, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at MLDmax (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_y_continuous(limits = c(75, 110), n.breaks = 6) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


#save figure
ggsave(filename = "normalized_time_series_MLDmax.png", plot = figure4, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


# 5. TIME SERIES AT 1000M -------------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_1000.csv$")

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

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_1000_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_1000', variable.name = "Model")


#plot time series at 100m
figure5 <- ggplot(data = df2, aes(x = Year, y = POC_flux_1000, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 1000m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

#save figure
ggsave(filename = "time_series_1000m.png", plot = figure, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


## 6. NORMALIZED POC FLUX AT 1000M RELATIVE TO 1850-1900 AVG --------------


#read in df created for first figure
ts.1000.all <- read_csv("~/senior_thesis/plotting_dataframes/time_series/time_series_1000_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.1000.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.1000 <- 
  subset(ts.1000.all, select = -c(1))

#calculate normalized POC flux
normalized.1000 <- mapply('/', normalized.1000, mean1850_1900)*100

normalized.1000 <- normalized.1000 %>% 
  cbind(Year = c(1849:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.1000, "~/senior_thesis/plotting_dataframes/time_series/1000_time_series_normalized.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.1000 <- data.table::melt(normalized.1000,  id.vars = 'Year', value.name = 'POC_flux_1000', variable.name = "Model")

#plot time series at 1000m
figure6 <- ggplot(data = plot.normalized.1000, aes(x = Year, y = POC_flux_1000, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at 1000m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))#+
#scale_y_continuous(limits = c(60, 112), n.breaks = 6)


#save figure
ggsave(filename = "normalized_time_series_1000m.png", plot = figure6, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)



## 7. FACETED NORMALIZED TIME SERIES FIGURE -----------

combined <- grid.arrange(figure3, figure4,figure6, ncol = 1)

ggsave(filename = "normalized_time_series_faceted.png", plot = combined, path = "~/senior_thesis/figures/faceted/", width = 20, height = 36, units = "cm", dpi = 400)

combined2 <- grid.arrange(figure, figure2, figure5, ncol = 1)

ggsave(filename = "time_series_faceted.png", plot = combined2, path = "~/senior_thesis/figures/faceted/", width = 20, height = 36, units = "cm", dpi = 400)


## 8. TIME SERIES NPP -------------------


setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.npp <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_npp.csv$")

#create empty list for storing for loop output
time.series2 <- list()

for(i in df.npp) {
  
  #read in csv file
  df.npp <- read_csv(i)
  #get rid of random ...1 column
  df.npp <- subset(df.npp, select = -c(...1))
  #store into list
  time.series2[[i]] <- df.npp
}

#add UKESM to list separately since it doesn't have the "...1" column
time.series2$UKESM_time_series_npp.csv = as.tibble(read_csv("UKESM_time_series_npp.csv"))

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.npp <- time.series2 %>% 
  reduce(left_join, by = "Year")

#add column for model key
df2.npp <- data.table::melt(df.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")

#save NPP time-series data frame for all models
write_csv(df.npp, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_npp_all.csv")

#plot time series at MLDmax
figure7 <- ggplot(data = df2.npp, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  #scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure7

#save figure
ggsave(filename = "time_series_npp.png", plot = figure7, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


# 9. NORMALIZED NPP TIME SERIES ------------

#read in df created for first figure
ts.npp.all <- read_csv("~/senior_thesis/plotting_dataframes/time_series/time_series_npp_all.csv")
ts.npp.all <- subset(ts.npp.all, select = -c(4))


#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.npp.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.npp <- 
  subset(ts.npp.all, select = -c(1))

#calculate normalized POC flux
normalized.npp <- mapply('/', normalized.npp, mean1850_1900)*100

normalized.npp <- normalized.npp %>% 
  cbind(Year = c(1849:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#add column for model key
normalized.npp <- data.table::melt(normalized.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")


#save df
write_csv(normalized.npp, "~/senior_thesis/plotting_dataframes/time_series/npp_time_series_normalized.csv")


#plot time series at MLDmax
figure8 <- ggplot(data = normalized.npp, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  #scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure8

#save figure
ggsave(filename = "normalized_time_series_npp.png", plot = figure8, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


# 10. TRANSFER EFFICIENCY TIME SERIES -----------

#read in data frames
setwd("~/senior_thesis/plotting_dataframes/time_series/")
ts_100 <- read_csv("time_series_epc100_all.csv")
ts_1000 <- read_csv("time_series_1000_all.csv")

no_year_100 <- ts_100[2:7]
no_year_1000 <- ts_1000[2:7]

TE <- (no_year_1000/no_year_100)*100
TE$Year = 1850:2100

TE %>%
  relocate(Year, .before = CESM) 

#add column for model key
TE <- data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure10 <- ggplot(data = TE, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between 100m and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure10

#save figure
ggsave(filename = "time_series_TE.png", plot = figure10, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

