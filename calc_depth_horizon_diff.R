#' @title Calculate difference between changes in POC flux at 100m and MLDmax
#' @author Stevie Walker
#' @date 10/1/21
#' @input two matrices of POC flux change at both depth horizons
#' @output two matrices of POC flux difference (100m - MLDmax) and (MLDmax - 100m)

calc_depth_horizon_diff <- function(model.name) {
  
  setwd('~/senior_thesis/plotting_dataframes')
  
  epc100 <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_diff.Rds",sep=""))
  expc <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_expc_change.Rds",sep=""))
  
  POC_flux_diff_100 = epc100 - expc
  POC_flux_diff_MLD = expc - epc100
  
  saveRDS(POC_flux_diff_100, file = paste("~/senior_thesis/plotting_dataframes/",model.name,"_depth_horizon_diff_100.Rds",sep=""), ascii = TRUE)
  saveRDS(POC_flux_diff_MLD, file = paste("~/senior_thesis/plotting_dataframes/",model.name,"_depth_horizon_diff_MLD.Rds",sep=""), ascii = TRUE)
  
}