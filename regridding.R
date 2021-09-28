#' @title Regrid Model Resolution

rGFDL <- readRDS("~/Documents/Senior_Thesis/Data/GFDL_mean_expc_st.Rds")
rMPI <- readRDS("~/Documents/Senior_Thesis/Data/MPI_mean_expc_st.Rds")

rGFDL <- raster(rGFDL, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(rMPI, crs = "+proj=longlat +datum=WGS84")

#set for all models before matching to GFDL resolution
#proj4string(rMPI) = "+proj=longlat +datum=WGS84"
#proj4string(rGFDL) = "+proj=longlat +datum=WGS84"

new_res_MPI <- matchResolution(x= rMPI,ref= rGFDL,method= "bilinear")

plot(new_res_MPI)


## Ignore all this below --------------


ggsave("MPI_res_test.png", test, path = "~/senior_thesis/figures", width = 20, height = 20, units = "cm", dpi = 400)


#set for all models before matching to GFDL resolution
proj4string(rMPI) = "+proj=longlat +datum=WGS84"
proj4string(rGFDL) = "+proj=longlat +datum=WGS84"

bb = extent(0,404,0,802)
bb = extent(0,360,0,180)
rMPI = setExtent(rMPI, bb, keepres=TRUE)
rGFDL = setExtent(rGFDL, bb, keepres=TRUE)

rCESM <- readRDS("~/senior_thesis/plotting_dataframes/CESM_mean_expc_st.Rds")
rCMCC <- readRDS("~/senior_thesis/plotting_dataframes/CMCC_mean_expc_st.Rds")
rECEarth <- readRDS("~/senior_thesis/plotting_dataframes/EC-Earth_mean_expc_st.Rds")
rGFDL <- readRDS("~/senior_thesis/plotting_dataframes/GFDL_mean_expc_st.Rds")
rMPI <- readRDS("~/senior_thesis/plotting_dataframes/MPI_mean_expc_st.Rds")
rUKESM <- readRDS("~/senior_thesis/plotting_dataframes/UKESM_mean_expc_st.Rds")

rCESM <- raster(rCESM)
rCMCC <- raster(rCMCC)
rECEarth <- raster(rECEarth)
rGFDL <- raster(rGFDL, xmn = 0, xmx = 360, ymn = 0, ymx = 180, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(x = rMPI, xmn = 0, xmx = 802, ymn = 0, ymx = 404, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(rMPI, crs = "+proj=longlat +datum=WGS84")
rUKESM <- raster(rUKESM)

