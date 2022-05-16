
df = "~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds"
nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
model.name = "CESM"
lat.name = "nlat"
lon.name = "nlon"

#read in native grid matrix
matrix <- readRDS(df)

#open nc file the matrix was derived from
wd = paste0("~/senior_thesis/combined_", model.name,"_files/")
setwd(wd)
nc_file <- nc_open(nc.file)

#pulls out corresponding lat and lon values for cells in the native grid
lat.old = ncvar_get(nc_file, lat.name)
lon.old = ncvar_get(nc_file, lon.name)

#rename matrix dimensions
dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

#convert to raster
r = rasterFromXYZ(old.df)

#save as nc file
writeRaster(r, filename = "pp_lt_CESM.nc", varname = "pp", xname = "nlon", yname = "nlat", overwrite = TRUE)

#open saved nc file
output_pp <- nc_open("pp_lt_CESM.nc")
print(output_pp)
t <- raster("pp_lt_CESM.nc", varname = "pp")

#Hem's code, but with my data

#longitude
system("ncks -O --msa -d nlon,180.,360. -d nlon,0.,179.999 pp_lt_CESM.nc out.nc", wait = TRUE)

raster.nc.test <- raster("out.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test)

system("ncap2 -O -s 'where(nlon > 180) nlon=nlon-360' out.nc out180.nc", wait = TRUE)

raster.nc.test.2 <- raster("out180.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.2)

#latitude, which for some reason was on a 0.5-384.5 scale

system("ncap2 -O -s 'where(nlat > 0) nlat=nlat/2.133333' out180.nc out90lat.nc", wait = TRUE)
raster.nc.test.3 <- raster("out90lat.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.3)

system("ncap2 -O -s 'where(nlat < 89.999) nlat=nlat-89.999' out90lat.nc out90lat2.nc", wait = TRUE)
raster.nc.test.4 <- raster("out90lat2.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.4)


system("ncap2 -O -s 'where(nlat > 89.999) nlat=nlat-89.999' out90lat2.nc out90lat3.nc", wait = TRUE)
raster.nc.test.5 <- raster("out90lat3.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.5)

res(raster.nc.test.5) = c(1,1)

geo.r <- raster(extent(-179.5627, 180.5627, -89.76462, 90.2354),
                crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                res = c(1,1))

## resampling from the GFDL data may be the way to go...

test <- raster::resample(r, raster.nc.test.5 ,method = "bilinear")

plot(test)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(test, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

## Here's where I'm stuck -----------

world <- ne_coastline(scale = "medium", returnclass = "sf")
title = paste(model.name, "long-term (2079-2099) NPP", sep = " ")

#map of regridded NA subset
map <- ggplot() +
  geom_tile(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
  labs(title = title,
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

ggsave(paste0("~/senior_thesis/figures/regridded/regridded_",model.name,"_lt_npp_world_2.png"), map, width = 20, height = 10, units = "cm", dpi = 500)


## Trying to combine the methods -----------




