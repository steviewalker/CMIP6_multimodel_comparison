#' @title Plot maximum annual mixed layer depth
#' @author Stevie Walker
#' @date 7/21/21
#' @description plots and saves 1) a three panel figure of short term MLDmax, long term MLDmax, and change in MLDmax for each model
#' and 2) a one panel figure of change in POC flux that can be later combined in the notebook
#' @note check the file metadata for each model to find the lat and lon standard names
#' @note model.name must match the name used in the Rds files saved from calc_epc_100_avg.R

plot_MLD_max <- function(wd, nc.file, model.name, lat.name, lon.name) {

setwd(wd)
nc_data <- nc_open(nc.file)


mean_MLD_max_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_MLD_max_lt.Rds",sep=""))
mean_MLD_max_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_MLD_max_st.Rds",sep=""))
MLD_change <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_MLD_change.Rds",sep=""))

## Beginning of 21st century -------

ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$MLD <- mean_MLD_max_st

file_info <- ret

melt_mld <- function(L) {
  dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$MLD, value.name = "mlotst")
}

melt_MLD_max_st <- melt_mld(file_info)

MLD_max_st<- ggplot(data = melt_MLD_max_st, aes(x = lon, y = lat, fill = mlotst)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,400), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("a) Short-term (2014-2034)"," MLD"[max]," (m)",sep=""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

MLD_max_st

## End of 21st century -------


ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$MLD <- mean_MLD_max_lt

file_info <- ret

melt_mld <- function(L) {
  dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$MLD, value.name = "mlotst")
}

melt_MLD_max_lt <- melt_mld(file_info)

MLD_max_lt<- ggplot(data = melt_MLD_max_lt, aes(x = lon, y = lat, fill = mlotst)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,400), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("b) Long-term (2079-2099)"," MLD"[max]," (m)",sep=""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

MLD_max_lt

## Change in MLD ----------


#difference, taken from calc_MLD_max.R
MLD_change = mean_MLD_max_lt - mean_MLD_max_st


ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$MLD <- MLD_change

file_info <- ret

melt_mld <- function(L) {
  dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$MLD, value.name = "mlotst")
}

melt_MLD_change <- melt_mld(file_info)


MLD_max_change<- ggplot(data = melt_MLD_change, aes(x = lon, y = lat, fill = mlotst)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-150,150), oob = squish, name = "balance", direction = -1) +
 # scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("c) Change in"," MLD"[max]," (m)",sep=""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

MLD_max_change

MLD_max_change2<- ggplot(data = melt_MLD_change, aes(x = lon, y = lat, fill = mlotst)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-150,150), oob = squish, name = "balance", direction = -1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = model.name,
       subtitle = expression(paste("Change in"," MLD"[max]," (m)",sep=""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

figure <- ggarrange(MLD_max_st,MLD_max_lt, MLD_max_change,
                    ncol = 1, nrow = 3)

ggsave(paste(model.name,"_MLDmax_global_map.png",sep=""), plot = figure, path = "~/senior_thesis/figures", width = 22, height = 30, units = "cm", dpi = 400)

#saving panel c
ggsave(paste(model.name,"_MLDmax_change.png",sep=""), plot = MLD_max_change2, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)

}