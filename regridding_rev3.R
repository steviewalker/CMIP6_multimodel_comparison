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

## GFDL plot -----------


df = "~/senior_thesis/plotting_dataframes/epc100/GFDL_epc100_avg_his.Rds"
nc.file = "epc100_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_185001-201412.nc"
model.name = "GFDL"
lat.name = "lat"
lon.name = "lon"
variable = "pp"

matrix <- readRDS(df)

setwd("~/senior_thesis/combined_GFDL_files/")
nc_file <- nc_open(nc.file)

lat.old = ncvar_get(nc_file, "lat")
lon.old = ncvar_get(nc_file, "lon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

old.df$lon <- ifelse(old.df$lon>180,
                     old.df$lon-360,
                     old.df$lon)

r = rasterFromXYZ(old.df)

#GFDL raster
geo.GFDL <- rasterToPoints(r, spatial = TRUE)
# Then to a 'conventional' dataframe
df.GFDL  <- data.frame(geo.GFDL)


world <- ne_coastline(scale = "medium", returnclass = "sf")
#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
  labs(title = "GFDL long-term (2079-2099) net primary production",
       fill = "mol m-2 yr-1") +
  scale_fill_cmocean(limits = c(0,6), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

## regridding ---------


test <- raster::resample(r, raster.nc.test.5 ,method = "bilinear")
plot(test)

#GFDL raster
geo.GFDL <- rasterToPoints(test, spatial = TRUE)
# Then to a 'conventional' dataframe
df.GFDL  <- data.frame(geo.GFDL)


## trying to fix lat lon coordinates ------------


#open saved nc file
output_pp <- nc_open("pp_lt_CESM.nc")
print(output_pp)
t <- raster("pp_lt_CESM.nc", varname = "pp")

#Hem's code, but with my data

#longitude
system("ncks -O --msa -d nlon,180.,360. -d nlon,0.,179.999 pp_lt_CESM.nc out.nc", wait = TRUE)

system("ncks -O -v pp --msa -d nlon,0.,180. -d nlon,-180.,-1.0 pp_lt_CESM.nc out.nc")

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


