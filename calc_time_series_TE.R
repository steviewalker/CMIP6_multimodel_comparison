Open NC file

pull out 100m flux for one year

calculate 1000m flux for one year

1000m/100m x 100% with these matrices

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
