
## regridding function ------------

df = "~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds"
nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
model.name = "CESM"
lat.name = "nlat"
lon.name = "nlon"

## Regrid CESM ----------

regrid.CESM <- function(df, nc.file, model.name, lat.name, lon.name) {

matrix <- readRDS(df)

wd = paste0("~/senior_thesis/combined_", model.name,"_files/")
setwd(wd)
nc_file <- nc_open(nc.file)

#pulls out corresponding lat and lon values for cells in the native grid
lat.old = ncvar_get(nc_file, lat.name)
lon.old = ncvar_get(nc_file, lon.name)

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

extent.study = extent(-178,178,-89,89)

r = setExtent(r,extent.study,keepres = TRUE)

#extract coordinates at each grid cell
lon <- raster(nc.file,varname="lon_bnds")
lat <- raster(nc.file,varname="lat_bnds")

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

library(akima)

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

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "medium", returnclass = "sf")
title = paste(model.name, "long-term (2079-2099) NPP", sep = " ")

#map of regridded NA subset
map <- ggplot() +
  #geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
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

ggsave(paste0("~/senior_thesis/figures/regridded/regridded_",model.name,"_lt_npp_world.png"), map, width = 20, height = 10, units = "cm", dpi = 500)

       
}

## Regrid CMCC -------------

df = "~/senior_thesis/plotting_dataframes/NPP/CMCC_mean_npp_lt.Rds"
nc.file = "pp_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
model.name = "CMCC"
lat.name = "latitude"
lon.name = "longitude"

regrid.CMCC <- function(df, nc.file, model.name, lat.name, lon.name) {
  
  matrix <- readRDS(df)
  
  wd = paste0("~/senior_thesis/combined_", model.name,"_files/")
  setwd(wd)
  nc_file <- nc_open(nc.file)
  
  #pulls out corresponding lat and lon values for cells in the native grid
  lat.old = ncvar_get(nc_file, lat.name)
  lat.old = lat.old[1,]
  lat.old = 1:292
  
  lon.old = ncvar_get(nc_file, lon.name)
  lon.old = lon.old[,1]
  lon.old = 1:362
  
  dimnames(matrix) = list(lon = lon.old, lat = lat.old)
  old.df2 = melt(matrix)
  
  r = rasterFromXYZ(old.df2)
  
  #extract coordinates at each grid cell
  lon <- raster(nc.file,varname=lon.name)
  lat <- raster(nc.file,varname=lat.name)
  
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
  
  #CMCC Native Grid
  map("world",fill=TRUE,mar=c(0,0,0,0))
  points(lat ~lon,remap.tbl,pch=".",col="red")
  
  library(akima)
  
  geo.r <- raster(extent(-178,178,-77,88))
  res(geo.r) <- c(1,1)
  
  r.coords.x <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                       xo=xFromCol(geo.r),yo=yFromRow(geo.r), duplicate = "mean")
  r.coords.y <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                       xo=xFromCol(geo.r),yo=yFromRow(geo.r), duplicate = "mean")
  r.coords <- expand.grid(lon=xFromCol(geo.r),
                          lat=yFromRow(geo.r))
  r.coords$x <- as.vector(r.coords.x$z)
  r.coords$y <- as.vector(r.coords.y$z)
  
  r.coords.sp <- r.coords
  coordinates(r.coords.sp) <- ~x +y
  #important to make sure extract is from right package
  r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
  geo.r <- setValues(geo.r,r.coords$pp)
  
  # First, to a SpatialPointsDataFrame
  geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
  # Then to a 'conventional' dataframe
  df  <- data.frame(geo.r.pts)
  
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
  
  ggsave(paste0("~/senior_thesis/figures/regridded/regridded_",model.name,"_lt_npp_world.png"), map, width = 20, height = 10, units = "cm", dpi = 500)
         
         
}

# plot GFDL -------------

#df = "~/senior_thesis/plotting_dataframes/epc100/GFDL_epc100_avg_his.Rds"
#nc.file = "epc100_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_185001-201412.nc"
#variable = "npp"
#time.period = "his"
#date.name = "Historical (1850-1900)"

GFDL.raster <- function(df, variable, time.period, date.name) {
  
  matrix <- readRDS(df)
  
  setwd("~/senior_thesis/combined_GFDL_files/")
  #nc_file <- nc_open(nc.file)
  nc_file <- nc_open("epc100_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_185001-201412.nc")
  
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

  df.GFDL = subset(df.GFDL, select = -c(4))
  
  world <- ne_coastline(scale = "medium", returnclass = "sf")
  
  if(variable == "npp") {
  
  #map of regridded NA subset
  map <- ggplot() +
    geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey70") +
    coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
    labs(title = paste("GFDL", date.name, "NPP", sep = " "),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    #POC flux
    scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw()
  
  map
  
  ggsave(paste0("~/senior_thesis/figures/regridded/npp/regridded_GFDL_",time.period,"_",variable,".png"), map, width = 22, height = 12, units = "cm", dpi = 500)

  }
  
  else if (variable == "epc100") {
    
    #map of regridded NA subset
    map <- ggplot() +
      geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "grey70") +
      coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
      labs(title = paste("GFDL", date.name, "POC flux at 100m", sep = " "),
           fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
      #scale_fill_cmocean(limits = c(0,8), oob = squish, name = "deep", direction = 1) +
      #uncomment for change figure
      scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw()
    
    map
    
    ggsave(paste0("~/senior_thesis/figures/regridded/epc100/regridded_GFDL_",time.period,"_",variable,".png"), map, width = 22, height = 12, units = "cm", dpi = 500)
    
    
  }
  
  else if(variable == "expc") {
    
    #map of regridded NA subset
    map <- ggplot() +
      geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "grey70") +
      coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
      labs(title = expression(paste("GFDL change in POC flux"," at"," MLD"[max],sep="")),
           fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
      #scale_fill_cmocean(limits = c(0,5), oob = squish, name = "deep", direction = 1) +
      #uncomment for change figure
      scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw()
    
    map
    
    ggsave(paste0("~/senior_thesis/figures/regridded/expc/regridded_GFDL_",time.period,"_",variable,".png"), map, width = 22, height = 12, units = "cm", dpi = 500)
    
  }
  
  else if(variable == "mlotst") {
    
    #map of regridded NA subset
    map <- ggplot() +
      geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "grey70") +
      coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
      labs(title = expression(paste("GFDL ", "historical (1850-1900)", " MLD"[max],sep="")),
           fill = "m") +
      scale_fill_cmocean(limits = c(0,400), oob = squish, name = "deep", direction = 1) +
      #uncomment for change figure
      #scale_fill_cmocean(limits = c(-150,150), oob = squish, name = "balance", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw()
    
    map
    
    ggsave(paste0("~/senior_thesis/figures/regridded/mlotst/regridded_GFDL_",time.period,"_",variable,".png"), map, width = 22, height = 12, units = "cm", dpi = 500)
    
  }
  
  else if(variable == "dh_diff") {
    
    map <- ggplot() +
      geom_raster(data = df.GFDL , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "grey70") +
      coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
      labs(title = expression(paste("GFDL difference in POC flux change"," (MLD"[max],"-100m)",sep="")),
           fill = "") +
      scale_fill_cmocean(limits = c(-1,1), oob = squish, name = "balance", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw()
    
    map
    
    ggsave(paste0("~/senior_thesis/figures/regridded/DH_diff/regridded_GFDL_",time.period,"_",variable,".png"), map, width = 22, height = 12, units = "cm", dpi = 500)
    
  }
  
  }

