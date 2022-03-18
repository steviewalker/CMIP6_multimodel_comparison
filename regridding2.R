## Regrid entire map -------------------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)
range(remap.tbl$lat)

#shows entire model native grid
library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,remap.tbl,pch=".",col="red")

library(akima)
#sample points from North Atlantic
#remap.NA <- subset(remap.tbl,lon>-170 & lon<170 & lat > -80 & lat < 80)

geo.r <- raster(extent(-178,178,-79,88))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
#not sure why but the plot function stopped working
plot(geo.r)
map("worldHires",add=TRUE)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "medium", returnclass = "sf")

#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
  labs(title = "CESM long-term (2079-2099) NPP",
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

ggsave("~/senior_thesis/figures/regridded_CESM_lt_npp_world.png", map, width = 20, height = 10, units = "cm", dpi = 500)

