#' @title Figure 4 Plot Points Map
#' @author Stevie Walker
#' @description Plots annotated sample points map over change in POC flux figure

## annotating over epc100 change map --------------

setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')

var_difference_yr <- readRDS("~/senior_thesis/plotting_dataframes/epc100_diff.Rds")

ret <- list()
ret$lat <- ncvar_get(nc_data, "nlat")
ret$lon <- ncvar_get(nc_data, "nlon") # - 360 # we need them as negative values
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
  theme_bw() +
  labs(title = "Profile Points") +
  theme(axis.title = element_text(size = 9),
        legend.title = element_blank())+
  annotate(geom="point", x=305, y=355, color="black", size = 2) +
  annotate(geom="text", x =314, y = 365, color = "black", label = "A") +
  annotate(geom="point", x=99, y=63, color="black", size = 2) +
  annotate(geom="text", x =108, y = 73, color = "black", label = "E") +
  annotate(geom="segment", x = 10, xend = 10, y = 0, yend = 384, color = "black") +
  annotate(geom="segment", x = 205, xend = 205, y = 0, yend = 384, color = "black") +
  annotate(geom="point", x=274, y=184, color="black", size = 2) +
  annotate(geom="text", x =283, y = 194, color = "black", label = "C") +
  annotate(geom="point", x=30, y=352, color="black", size = 2) +
  annotate(geom="text", x =39, y = 362, color = "black", label = "B") +
  annotate(geom="point", x=230, y=275, color="black", size = 2) +
  annotate(geom="text", x =239, y = 285, color = "black", label = "D") +
  annotate(geom="point", x=276, y=21, color="black", size = 2) +
  annotate(geom="text", x =285, y = 31, color = "black", label = "F")


basic.plot3

ggsave("profile_points_map.png", plot = basic.plot3, path = "~/senior_thesis/figures", width = 16, height = 9, units = "cm", dpi = 500)
