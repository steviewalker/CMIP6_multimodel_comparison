#' @title Plot average POC flux at 100m
#' @author Stevie Walker
#' @date 7/20/21
#' @description plots and saves 1) a three panel figure of short term POC flux, long term POC flux, and change in POC flux for each model
#' and 2) a one panel figure of change in POC flux that can be later combined in the notebook
#' @description plots on the model's native grid
#' @note check the file metadata for each model to find the lat and lon standard names
#' @note model.name must match the name used in the Rds files saved from calc_epc_100_avg.R

plot_epc100_avg <- function(wd, nc_file, model.name, lat.name, lon.name) {

#open nc file
setwd(wd)
nc_data <- nc_open(nc_file)

var_year1 <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_st.Rds",sep=""))
var_year2 <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_lt.Rds",sep=""))
var_difference_yr <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_diff.Rds",sep=""))


# #basic plot of downward POC flux at the beginning of the 21st ce --------


#plot average POC flux for early 21st century
ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$epc100 <- var_year1

file_info <- ret

#melt data so I can plot it in ggplot
melt_epc100 <- function(L) {
  dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$epc100, value.name = "epc100")
}

mepc100 <- melt_epc100(file_info)

basic.plot<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,8), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("a) Short-term (2014-2034) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
        )


basic.plot

# #basic plot of downward POC flux at the end of the 21st ce --------



#plot average POC flux for early 21st century
ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$epc100 <- var_year2

file_info <- ret

#melt data so I can plot it in ggplot
melt_epc100 <- function(L) {
  dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$epc100, value.name = "epc100")
}

mepc100 <- melt_epc100(file_info)

basic.plot2<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,8), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("b) Long-term (2079-2099) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

basic.plot2


# Plot of change in POC flux in 21st c ------------------------------------


ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$epc100 <- var_difference_yr

file_info <- ret

#melt data so I can plot it in ggplot
melt_epc100 <- function(L) {
  dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$epc100, value.name = "epc100")
}

mepc100 <- melt_epc100(file_info)

basic.plot3<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("c) Change in POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())


basic.plot3

change<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = model.name,
       subtitle = expression(paste("Change in POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())


figure <- ggarrange(basic.plot, basic.plot2, basic.plot3,
                    ncol = 1, nrow = 3)
figure <- annotate_figure(figure, top = text_grob(model.name, face = "bold", size = 16))

ggsave(paste(model.name,"_epc100_global_map.png",sep=""), plot = figure, path = "~/senior_thesis/figures", width = 22, height = 30, units = "cm", dpi = 400)

#saving panel c
ggsave(paste(model.name,"_epc100_change.png",sep=""), plot = change, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)

}
