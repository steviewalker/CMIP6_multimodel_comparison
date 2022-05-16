

## extracting at the exact grid cell ---------

#model.name = "CESM"
#nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
#coord.lat = 22.75
#coord.lon = -158

extract_native_points <- function(model.name, location, nc, lon.name, lat.name, coord.lat, coord.lon) {
  
  #open nc file
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
  
  if(model.name == "GFDL") {
    
    ## GFDL edit ---------
    
    nc_file <- nc_open(nc)
    lon <- ncvar_get(nc_file, "lon")
    lat <- ncvar_get(nc_file, "lat")
    
    #convert grid cells and corresponding lat-lon to a data.frame
    remap.tbl <- data.frame(lon=as.vector(lon),lat=as.vector(lat))
    
    #correct longitude
    remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                            remap.tbl$lon-360,
                            remap.tbl$lon)
    
    x = remap.tbl[which.min(abs(coord.lon-remap.tbl$lon)),]$lon
    
    #unfix lon to get right native grid point later
    x <- ifelse(x<180,
                x+360,
                x)
    
    y = remap.tbl[which.min(abs(coord.lat-remap.tbl$lat)),]$lat
    
    #correct lat scale for native grid
    y <- ifelse(y>=0,
                y+90,
                y)
    
    y <- ifelse(y<=0,
                abs(y),
                y)
      
    print(x)
    print(y)
    
  } else {
      # --------------------
      
      #extract coordinates at each grid cell
      lon <- raster(nc,varname= lon.name)
      lat <- raster(nc,varname= lat.name)
      
      #convert grid cells and corresponding lat-lon to a data.frame
      remap.tbl <- data.frame(coordinates(lon),
                              lon=as.vector(lon),lat=as.vector(lat))
      
      #correct longitude
      remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                              remap.tbl$lon-360,
                              remap.tbl$lon)
      
      #make a list of points close to HOTS coordinates (old manual way)
      #gen.area <- subset(remap.tbl,lon> -159 & lon< -156 & lat > 21 & lat < 23)
      #gen.area[which.min(abs(coord.lat-gen.area$lat)),]
      
      #take away native grid rows to calculate distance
      remap.latlon <- subset(remap.tbl, select = -c(1,2))
      
      #calculate distance between desired coordinates and the points in remap.tbl - lowest distance = the row we want
      dist <- distm(x = c(coord.lon,coord.lat), y = remap.latlon, fun = distHaversine)
      remap.tbl$dist <- distHaversine(p1 = c(coord.lon,coord.lat), p2 = remap.latlon)
      
      #print the row we want
      row <- remap.tbl[which.min(remap.tbl$dist),]
      print(row)
      
    }
    
    #extracting depth profile data ------------
    
    nc_file = nc_open(nc)
    
    #change for monthly vs yearly pp files
    #yearly
    npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,2), count = c(-1,-1,-1,1))*31536000
    #monthly
    #npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,2), count = c(-1,-1,-1,12))*31536000
    #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    depth <- as_tibble(ncvar_get(nc_file, "olevel"))
    #depth <- as_tibble(ncvar_get(nc_file, "lev")) #/100
    
    if(model.name == "GFDL") {
      
      npp2 <- npp[x,y, ]
      
    } else {
      
      npp2 <- npp[row$x,row$y, ]
      
    }
    
    profile <- data.frame(depth, npp2) %>%
      as_tibble()
    
    colnames(profile) <- c("depth", model.name)
    
    write_csv(x = profile, file = paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/",model.name,"_npp_profile_",location,".csv"))
    
  }
  
  
  #wd = "~/senior_thesis/combined_IPSL_files/"
  #nc_file = "pp_Oyr_IPSL-CM6A-LR_ssp585_r1i1p1f1_gn_2015-2100.nc"
  #model.name = "IPSL"
  #lat.name = "nav_lat"
  #lon.name = "nav_lon"
  
  ## combine csv files and plot depth profile ----------
  
  plot_npp_profile <- function(location) {
    
    setwd("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/")
    df.profiles <- list.files("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/", pattern = paste0("*_",location,".csv$"))
    
    #create empty list for storing for loop output
    df.profiles2 <- list()
    
    for(i in df.profiles) {
      
      #read in csv file
      df.profiles <- read_csv(i)
      #store in list
      df.profiles2[[i]] <- df.profiles
    }
    
    df.profiles <- df.profiles2 %>% 
      reduce(full_join, by = "depth")
    
    #one.percent = max(df.profiles$IPSL, na.rm = TRUE)/100
    #approx(x = IPSL.HOTS$npp, y  = IPSL.HOTS$depth, xout = one.percent)
    
    IPSL <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/IPSL_npp_profile_",location,".csv"))
    
    #calc 1% of NPP max
    one.percent = max(IPSL$IPSL, na.rm = TRUE)/100
    #find euphotic zone depth
    IPSL.ez.depth <- approx(x = IPSL$IPSL, y  = IPSL$depth, xout = one.percent)
    
    CESM <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/CESM_npp_profile_",location,".csv"))
    #calc 1% of NPP max
    one.percent = max(CESM$CESM, na.rm = TRUE)/100
    #find euphotic zone depth
    CESM.ez.depth <- approx(x = CESM$CESM, y  = CESM$depth, xout = one.percent)
    
    GFDL <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/GFDL_npp_profile_",location,".csv"))
    #calc 1% of NPP max
    one.percent = max(GFDL$GFDL, na.rm = TRUE)/100
    #find euphotic zone depth
    GFDL.ez.depth <- approx(x = GFDL$GFDL, y  = GFDL$depth, xout = one.percent)
    
    CMCC <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/CMCC_npp_profile_",location,".csv"))
    #calc 1% of NPP max
    one.percent = max(CMCC$CMCC, na.rm = TRUE)/100
    #find euphotic zone depth
    CMCC.ez.depth <- approx(x = CMCC$CMCC, y = CMCC$depth, xout = one.percent)
    
    
    UKESM <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/UKESM_npp_profile_",location,".csv"))
    #calc 1% of NPP max
    one.percent = max(UKESM$UKESM, na.rm = TRUE)/100
    #find euphotic zone depth
    UKESM.ez.depth <- approx(x = UKESM$UKESM, y = UKESM$depth, xout = one.percent)
    
    MPI <- read_csv(paste0("~/senior_thesis/plotting_dataframes/ez_depth/obs_validation/MPI_npp_profile_",location,".csv"))
    #calc 1% of NPP max
    one.percent = max(MPI$MPI, na.rm = TRUE)/100
    #find euphotic zone depth
    MPI.ez.depth <- approx(x = MPI$MPI, y = MPI$depth, xout = one.percent)
    
    
    #add column for model key (reformatting data specific to the below plot)
    df2 <- data.table::melt(df.profiles,  id.vars = 'depth', value.name = 'npp', variable.name = "Model")
    
    #colors for time series
    color = c("violet", "goldenrod2", "aquamarine3","darkorchid3", "darkorange2", "royalblue2")
    
    
    depth.profile <- ggplot(data = df2, aes(x = npp, y = depth, color = Model)) +
      geom_path() +
      #geom_point() +
      scale_x_continuous(position = "top") +
      scale_y_continuous(trans = "reverse", limits = c(400,0)) +
      scale_color_manual(values = color) +
      labs(title = paste0(location, " NPP Depth Profile"),,
           subtitle = "Dots indicate 1% of the maximum NPP") +
      xlab("NPP (mmol m-3 d)") +
      ylab("Depth (m)") +
      theme_bw() +
      annotate(geom="point", x=CESM.ez.depth$x, y=CESM.ez.depth$y, color="violet", size = 2) +
      annotate(geom="point", x=CMCC.ez.depth$x, y=CMCC.ez.depth$y, color="goldenrod2", size = 2) +
      annotate(geom="point", x=GFDL.ez.depth$x, y=GFDL.ez.depth$y, color="aquamarine3", size = 2) +
      annotate(geom="point", x=IPSL.ez.depth$x, y=IPSL.ez.depth$y, color="darkorchid3", size = 2) +
      annotate(geom="point", x=MPI.ez.depth$x, y=MPI.ez.depth$y, color="darkorange2", size = 2) +
      annotate(geom="point", x=UKESM.ez.depth$x, y=UKESM.ez.depth$y, color="royalblue2", size = 2) 
    
    
    depth.profile
    
    ggsave(filename = paste0("~/senior_thesis/figures/ez_depth/obs_validation/",location,"_NPP_depth_profile.png"), depth.profile, width = 20, height = 20, units = "cm")
    
  }
  
  
  # Find location that corresponds with observational data points -----------
  
  check_native_coords <- function(model.name, nc.file, lat.name, lon.name, x, y) {
    
    setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
    nc_file <- nc_open(nc.file)
    
    #read in sample map data  
    var_difference_yr <- readRDS(paste0("~/senior_thesis/plotting_dataframes/epc100/",model.name,"_epc100_diff.Rds"))
    
    ret <- list()
    ret$lat <- ncvar_get(nc_file, lat.name)
    ret$lon <- ncvar_get(nc_file, lon.name)
    ret$epc100 <- var_difference_yr
    
    #melt data so I can plot it in ggplot
    melt_expc <- function(L) {
      dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
      ret <- melt(L$epc100, value.name = "epc100")
    }
    
    mepc100 <- melt_expc(ret)
    
    location <- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
      geom_raster(interpolate = TRUE) +
      scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1)+
      #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI +
      theme_bw() +
      labs(title = "Check native grid location") +
      theme(axis.title = element_text(size = 9),
            legend.title = element_blank())+
      annotate(geom="point", x=x, y=y, color="black", size = 1)
    
    location
    
  }