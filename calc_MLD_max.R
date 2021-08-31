#' @title Calculate maximum annual mixed layer depth
#' @author Stevie Walker
#' @date 7/20/21
#' @description finds the 20 year short-term average MLDmax, 20 year long-term average MLDmax, and the change between the 2 for ESM data
#' @note be sure to run function get_time first so you know what to make seq.start.st and seq.start.lt


calc_MLD_max <- function(wd, nc.file, seq.start.st, seq.start.lt, model.name) {

setwd(wd)
nc_data <- nc_open(nc.file)

## Short-term ------------

#short term sequence
v <- seq(from = seq.start.st, to = seq.start.st + 228, by = 12)
n = length(v)

#create list to store for loop output in
list_max <- list()

for(i in 1:n) {
  #read in a year of MLD data
  t <- v[i]
  var <- ncvar_get(nc_data,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
  #calculate the MLD max for each year at each lat and lon
  max <- apply(var, c(1,2),max,na.rm=FALSE)
  #store output into the list
  list_max[[i]] <- max
}

Y <- do.call(cbind, list_max)
Y <- array(Y, dim=c(dim(list_max[[1]]), length(list_max)))

#20 year mean for the beginning of the 21st century
mean_MLD_max_st <- apply(Y, c(1, 2), mean, na.rm = FALSE)


## Calculating average max MLD for 2079-2099 ----------

#long term sequence
v <- seq(from = seq.start.lt, to = seq.start.lt + 228, by = 12)
n = length(v)

#create list to store for loop output in
list_max <- list()

for(i in 1:n) {
  #read in a year of MLD data
  t <- v[i]
  var <- ncvar_get(nc_data,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
  #calculate the MLD max for each year at each lat and lon
  max <- apply(var, c(1,2),max,na.rm=FALSE)
  #store output into the list
  list_max[[i]] <- max
}

Y <- do.call(cbind, list_max)
Y <- array(Y, dim=c(dim(list_max[[1]]), length(list_max)))

#20 year mean for the beginning of the 21st century
mean_MLD_max_lt <- apply(Y, c(1, 2), mean, na.rm = FALSE)

MLD_change = mean_MLD_max_lt - mean_MLD_max_st

#saving rds objects
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(mean_MLD_max_st, file = paste(model.name,"_mean_MLD_max_st.Rds",sep=""), ascii = TRUE)
saveRDS(mean_MLD_max_lt, file = paste(model.name,"_mean_MLD_max_lt.Rds",sep=""), ascii = TRUE)
saveRDS(MLD_change, file = paste(model.name,"_MLD_change.Rds",sep=""), ascii = TRUE)

}
