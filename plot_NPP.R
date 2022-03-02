#' @title Create net primary production spatial maps
#' @author Stevie Walker
#' @date 3/1/22
#' @description plots long-term (2079-2099), historical (1850-1990), and change in column integrated NPP
#' @input matrices created by calc_NPP.R
#' @output three panel long-term, historical, and change in NPP figure, one panel change in NPP figure


#model.name = "CMCC"
#wd = "~/senior_thesis/combined_CMCC_files/"
#nc.file = "pp_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
#lat.name = "latitude"
#lon.name = "longitude"

plot_NPP <- function(wd, nc.file, model.name, lat.name, lon.name) {

setwd(wd)
nc_data <- nc_open(nc.file)


mean_npp_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/NPP/",model.name,"_mean_npp_lt.Rds",sep=""))

## Beginning of 21st century -------

ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$npp <- mean_npp_lt

melt_npp <- function(L) {
  dimnames(L$npp) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$npp, value.name = "npp")
}

melt_npp_lt <- melt_npp(ret)

npp_lt <- ggplot(data = melt_npp_lt, aes(x = lon, y = lat, fill = npp)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("Long-term (2079-2099) ","NPP"," (mol ", m^-2, " ", yr^-1,")",sep = " "))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

npp_lt

ggsave(paste(model.name,"_npp_lt.png",sep=""), plot = npp_lt, path = "~/senior_thesis/figures/NPP/", width = 20, height = 10, units = "cm", dpi = 400)

}