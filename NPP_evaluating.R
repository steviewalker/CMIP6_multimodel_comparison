#change for monthly vs yearly pp files
#yearly
npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,2), count = c(-1,-1,-1,1))

#uncomment /100 for CESM
depth <- as_tibble(ncvar_get(nc_file, "lev")) #/100

#code for one grid cell --------

npp <- npp[150,150, ]
## INITIAL CODE TO CALCULATE NPP AT ONE GRID CELL AND MAKE SAMPLE PROFILE ---------


#create data frame
profile <- data.frame(depth, npp) %>%
  as_tibble() 
#rename depth column
profile <- rename(profile, depth = value)
#add calculated column height
profile <- profile %>% 
  #filter(depth <= 150) %>%
  mutate(height = depth - lag(depth))

#replace NA value in height column
profile[1,3] = profile$depth[1]
#calculate npp in mol m-2 d-1
profile <- profile %>%
  mutate(npp_new = npp*height)

column_npp = sum(profile$npp_new, na.rm = TRUE)

## Create sample NPP depth profile ------------

npp_profile <- function(wd, nc_file, model.name) {
  
  setwd(wd)
  nc_file <- nc_open(nc_file)
  
  #change for monthly vs yearly pp files
  #yearly
  npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,2), count = c(-1,-1,-1,1))
  #monthly
  #npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,2), count = c(-1,-1,-1,12))
  #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
  
  depth <- as_tibble(ncvar_get(nc_file, "lev")) #/100
  
  npp2 <- npp[200,100, ]
  
  profile <- data.frame(depth, npp2) %>%
    as_tibble()
  
  profile <- rename(profile, depth = value)
  
  ex.profile <- ggplot(data = profile, aes(x = npp2, y = depth)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(position = "top") +
    scale_y_continuous(trans = "reverse") +
    labs(title = paste(model.name,"NPP depth profile",sep = " ")) +
    xlab("NPP (mmol m-3 d)") +
    ylab("Depth (m)") +
    theme_bw()
  
  ex.profile
  
  ggsave(filename = paste0("~/senior_thesis/figures/NPP/",model.name,"_sample_profile.png"), ex.profile, width = 14, height = 20, units = "cm")
  
}