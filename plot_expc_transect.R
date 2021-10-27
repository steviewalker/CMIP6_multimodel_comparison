#' @title Plotting POC Flux Transects
#' @author Stevie Walker
#' @description plots latitudinal transects of POC flux in short-term, long-term, and change
#' @details April 2021


plot_transect <- function(lon, transect.st, transect.lt, save.name, save.name.zoom) {
  
  ## Adding MLD column to st and lt -----------------
  
  
  # SHORT TERM
  transect_st_MLD <- extract(mean_MLD_max_2014_2033, indices = lon, dims = 1)
  
  ret <- list()
  ret$MLD <-transect_st_MLD
  
  file_info <- ret
  
  melt_mld <- function(L) {
    dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$MLD, value.name = "mlotst")
  }
  
  melt_transect_st_MLD <- melt_mld(file_info)
  melt_transect_st_MLD <- subset(melt_transect_st_MLD, select = -lon)
  
  #duplicate each MLD 537 times to be able to add to transect data frame
  MLD_repeat<- melt_transect_st_MLD[rep(seq_len(nrow(melt_transect_st_MLD)), each = 537), ]
  
  #add mlotst to transect
  transect.st<- cbind(transect.st, MLD_repeat$mlotst)
  colnames(transect.st)[4] <- "mlotst"
  
  
  # LONG TERM 
  transect_lt_MLD <- extract(mean_MLD_max_2079_2099, indices = lon, dims = 1)
  
  ret <- list()
  ret$MLD <-transect_lt_MLD
  
  file_info <- ret
  
  melt_mld <- function(L) {
    dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$MLD, value.name = "mlotst")
  }
  
  melt_transect_lt_MLD <- melt_mld(file_info)
  melt_transect_lt_MLD <- subset(melt_transect_lt_MLD, select = -lon)
  
  #duplicate each MLD 537 times to be able to add to transect data frame
  MLD_repeat<- melt_transect_lt_MLD[rep(seq_len(nrow(melt_transect_lt_MLD)), each = 537), ]
  
  #add mlotst to transect
  transect.lt<- cbind(transect.lt, MLD_repeat$mlotst)
  colnames(transect.lt)[4] <- "mlotst"
  
  ## plots -------------------
  
  expc_transect_st <- ggplot(data = transect.st, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("a) Atlantic Ocean Short-term (2014-2034) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse") +
    scale_fill_cmocean(name = "deep") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()) +
    geom_line(aes(x = lat, y = mlotst), size=0.4)
  
  expc_transect_lt <- ggplot(data = transect.lt, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("b) Atlantic Ocean Long-term (2079-2099) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse") +
    scale_fill_cmocean(name = "deep") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()) +
    geom_line(aes(x = lat, y = mlotst), size=0.4)
  
  lat <- transect.st$lat
  depth <- transect.st$depth
  expc <- transect.lt$expc - transect.st$expc
  transect_change <- data.frame(lat, depth, expc)
  
  
  expc_transect_change <- ggplot(data = transect_change, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("c) Change in Atlantic Ocean POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse") +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  
  figure <- ggarrange(expc_transect_st,expc_transect_lt, expc_transect_change,
                      ncol = 1, nrow = 3)
  
  
  ggsave(save.name, plot = figure, path = "~/senior_thesis/figures", width = 30, height = 36, units = "cm", dpi = 400)
  
  
  ## Zoomed in transect --------------------
  
  expc_transect_st_zm <- ggplot(data = transect.st, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("a) Atlantic Ocean Short-term (2014-2034) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse", limits = c(1000,0)) +
    scale_fill_cmocean(name = "deep") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()) +
    geom_line(aes(x = lat, y = mlotst), size=0.6)
  
  
  expc_transect_lt_zm <- ggplot(data = transect.lt, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("b) Atlantic Ocean Long-term (2079-2099) POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse", limits = c(1000,0)) +
    scale_fill_cmocean(name = "deep") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()) +
    geom_line(aes(x = lat, y = mlotst), size=0.6)
  
  
  expc_transect_change_zm <- ggplot(data = transect_change, aes(x = lat, y = depth, fill = expc)) + 
    geom_raster() +
    labs(title = expression(paste("c) Change in Atlantic Ocean POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme_bw() +
    scale_y_continuous(trans = "reverse", limits = c(1000,0)) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  
  figure2 <- ggarrange(expc_transect_st_zm,expc_transect_lt_zm, expc_transect_change_zm,
                       ncol = 1, nrow = 3)
  
  ggsave(save.name.zoom, plot = figure2, path = "~/senior_thesis/figures", width = 30, height = 36, units = "cm", dpi = 400)
  
}
