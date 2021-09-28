#' @title Calculate Globally Integrated POC flux

global_flux <- function(wd, nc.file, model.name) {

setwd(wd)
nc_data <- nc_open(nc.file)

#get area
area <- ncvar_get(nc_data, "areacello")

#read in global POC flux at 100m data
epc100_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name, "_epc100_avg_st.Rds", sep = ""))
epc100_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name, "_epc100_avg_lt.Rds", sep = ""))
expc_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/", model.name,"_mean_expc_st.Rds", sep = ""))
expc_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/", model.name,"_mean_expc_lt.Rds", sep = ""))

## Global Flux at 100m ----------

global_flux_st <- epc100_st*area

sum_st <- sum(global_flux_st, na.rm = TRUE)

#SHORT TERM total global POC flux in Pt C / yr = 7.12
sum_st <- sum_st*12.01/1000000000000000

global_flux_lt <- epc100_lt*area

sum_lt <- sum(global_flux_lt, na.rm = TRUE)

#LONG TERM total global POC flux in Pt C / yr = 6.81
sum_lt <- sum_lt*12.01/1000000000000000

percent_change <- (sum_st-sum_lt)/sum_st*100

print(sum_st)
print(sum_lt)
print(percent_change)

## Global Flux at MLDmax ------------
global_flux_st2 <- expc_st*area

sum_st2 <- sum(global_flux_st2, na.rm = TRUE)

#SHORT TERM total global POC flux in Pt C / yr = 7.12
sum_st2 <- sum_st2*12.01/1000000000000000

global_flux_lt2 <- expc_lt*area

sum_lt2 <- sum(global_flux_lt2, na.rm = TRUE)

#LONG TERM total global POC flux in Pt C / yr = 6.81
sum_lt2 <- sum_lt2*12.01/1000000000000000

percent_change2 <- (sum_st2-sum_lt2)/sum_st2*100

print(sum_st2)
print(sum_lt2)
print(percent_change2)

}