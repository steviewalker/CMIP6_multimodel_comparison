#' @title Calculate monthly and yearly ocean density
#' @author Stevie Walker
#' @date 5/12/22
#' @description calc_monthly density finds ocean density by using temp and sal arrays calculated in calc_temp_sal.R
#' calc_yearly_density combines the density arrays and averages every 12 months, starting in January 1850
#' @input temp and sal lists
#' @output yearly density array from 1850-2100

#wd = "~/senior_thesis/CMCC_data/"
#nc.so = "so_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_205501-207412.nc"
#nc.thetao = "thetao_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_205501-207412.nc"
#lon.length = 1:362
#lat.length = 1:292
#depth.out = 200

#1 meter sea water = 1.00693064 decibar
#sal units = 0.001
calc_density <- function(model.name,depth.out,depth.file) {
  
  temp_list <- list.files("~/senior_thesis/plotting_dataframes/temp_sal/", pattern = paste0(model.name,"*.*","thetao","*.*",depth.out)) 
  sal_list <- list.files("~/senior_thesis/plotting_dataframes/temp_sal/", pattern = paste0(model.name,"*.*","so","*.*", depth.out)) 
  
  #pull depth values from one of the 4 dimensional nc files (I chose npp file because it's smaller and faster to read in)
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
  nc_depth <- nc_open(depth.file)
  depth <-  ncvar_get(nc_depth, "lev") #/100
  
  density_list <- list()
  
  for(i in 1:length(sal_list)) {
    
    #use for saving the year later
    names <- sal_list[[i]] %>%
      str_split(.,"_") %>%
      unlist
    
    for(j in 1:length(temp_list)) {
      
      #open salinity and temp lists (from calc_interp_tempsal)
      so <- readRDS(paste0("~/senior_thesis/plotting_dataframes/temp_sal/",sal_list[i]))
      thetao <- readRDS(paste0("~/senior_thesis/plotting_dataframes/temp_sal/",temp_list[j]))
      
      for(k in 1:length(so)){
        for(l in 1:length(thetao)) {
          
          if(depth.out == "surface") {
            
            #calculate density (surface depth is the first depth in the nc file)
            density <- gsw_rho_t_exact(SA = so[[k]], t = thetao[[l]], p = depth[1]*1.00693064)
            #save density matrix - length will be equal to the number of time steps
            density_list[[k]] <- density
            
          } else {
            
            #calculate density
            density <- gsw_rho_t_exact(SA = so[[k]], t = thetao[[l]], p = depth.out*1.00693064)
            #save density matrix - length will be equal to the number of time steps
            density_list[[k]] <- density
          }
        }
      }
    }
    
    #save monthly  
    saveRDS(density_list, file = paste0("~/senior_thesis/plotting_dataframes/density/",model.name,"_density_",depth.out,"_array_",names[5]), ascii = TRUE)
    
    #drop month label 
    #year <- names[5] %>%
    # str_split(.,"01-") %>%
    #unlist %>%
    #str_split(.,"12") %>%
    #unlist
    
  }
  
}

#start.date = 4
#model.name = "CESM"
#depth.out = "surface"

calc_yearly_density <- function(model.name, depth.out, start.date) {
  
  setwd("~/senior_thesis/plotting_dataframes/density/")
  
  monthly_density_list <- list.files("~/senior_thesis/plotting_dataframes/density/", pattern = paste0(model.name,"_density_",depth.out,"_*"))
  
  complete_list <- list()
  
  for(i in 1:length(monthly_density_list)) {
    
    ls <- readRDS(monthly_density_list[i])
    
    array <- array(unlist(ls), dim=c(dim(ls[[1]]), length(ls)))
    
    complete_list[[i]] <- array
  }
  
  #bind the arrays into one
  density_combine <- abind(complete_list, along = 3, force.array = TRUE)
  
  #drop needed values so the list starts at Jan 1850 (or Jan 1849 for CESM)
  density_start <- density_combine[,,-c(1:start.date)]
  
  #making for loop sequence (average every 12 years)
  jan <- seq(from = 1, to = dim(density_start)[3], by = 12)
  dec <- seq(from = 12, to = dim(density_start)[3], by = 12)
  
  density_avg <- list()
  
  for(i in 1:length(dec)) {
    
    #read in 12 months of data
    year_avg <- density_start[,,c(jan[i]:dec[i])]
    
    #combines matrices into a single array
    #year_array <- array(unlist(year_avg), dim=c(dim(year_avg[[1]]), length(year_avg)))
    
    year_matrix <- apply(year_avg, c(1,2),mean)
    
    density_avg[[i]] <- year_matrix
    
  }
  
  density_year_array <- array(unlist(density_avg), dim=c(dim(density_avg[[1]]), length(density_avg)))
  
  setwd("~/senior_thesis/plotting_dataframes/density/")
  saveRDS(density_year_array, file = paste0(model.name, "_yearly_density_",depth.out,"_array_1850_2100.Rds"), ascii = TRUE)
  
}

