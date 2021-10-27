

##FUNCTION -----------

profile_create <- function(grid.cell,depth.range,expc.range,title, data.path,save.path,save.name) {

setwd(data.path)

#extract short term and long term depth profiles for a selected lat and lon point 
profile_st <- extract(avg_expc_st, indices = grid.cell, dims = c(1,2))
profile_lt <- extract(avg_expc_lt, indices = grid.cell, dims = c(1,2))

#extract average max MLD at a selected lat and lon point (be sure to run calc_MLD_max.R first to get values)
setwd("~/senior_thesis/plotting_dataframes/")
#read in MLDmax matrices
mean_MLD_max_2014_2033 <- readRDS("~/senior_thesis/plotting_dataframes/mean_MLD_max_2014_2033.Rds")
mean_MLD_max_2079_2099 <- readRDS("~/senior_thesis/plotting_dataframes/mean_MLD_max_2079_2099.Rds")

#extract MLDmax at specified grid cell
MLD_max_st <- extract(mean_MLD_max_2014_2033, indices = grid.cell, dims = c(1,2))
MLD_max_lt <- extract(mean_MLD_max_2079_2099, indices = grid.cell, dims = c(1,2))

setwd("~/senior_thesis/combined_CESM_files/")
nc_data <- nc_open('expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc')

#get depth
depth.m = ncvar_get(nc_data, "lev")/100

## plot average POC flux for short-term (2014-2034) -----------
ret <- list()
ret$depth <-  ncvar_get(nc_data, "lev")/100
ret$expc <- profile_st*31536000

file_info <- ret

#melt data so I can plot it in ggplot
melt_depth <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat, depth = L$depth)
  ret <- melt(L$expc, value.name = "expc")
}

#ready for plotting!
melt_profile_st <- melt_depth(file_info)


## plot average POC flux for long-term (2079-2099) ------------
ret <- list()
ret$depth <-  depth.m
ret$expc <- profile_lt*31536000

file_info <- ret

#melt data so I can plot it in ggplot
melt_depth <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat, depth = L$depth)
  ret <- melt(L$expc, value.name = "expc")
}

#ready for plotting!
melt_profile_lt <- melt_depth(file_info)
print(melt_profile_lt)

profile_combined <- bind_rows(melt_profile_st, melt_profile_lt, .id = "id")
plot_profile <- data.frame(profile_combined, MLD_max_st, MLD_max_lt)

write.csv(plot_profile, paste("~/senior_thesis/plotting_dataframes/",save.name,".csv", sep = ""),row.names = TRUE)

## Final Plot ----------

depth_profile <- ggplot() +
  geom_path(data = profile_combined, aes(x = expc, y = depth, color = id), size = 1) +
  #geom_path(data = melt_profile_st, aes(x = expc, y = depth), size = 1) +
  geom_hline(yintercept = MLD_max_st, linetype = 2, color = "black", size = 0.8, show.legend = TRUE) +
  geom_hline(yintercept = MLD_max_lt, linetype = 2, color = "blue", size = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = expc.range, position = "top") +
  scale_y_continuous(limits = depth.range,trans = "reverse") +
  xlab(expression(paste("POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) + 
  ylab("Depth (m)") +
  labs(title = title) +
  theme_bw() +
  scale_color_manual(values = c("black","blue"), labels = c("2014-2034", "2079-2099")) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

  
ggsave(paste(save.name,".png",sep = ""), plot = depth_profile, path = save.path, width = 14, height = 12, units = "cm", dpi = 300)


#create list for table output
table <- list()
table$name <- title
table$max_POC_flux_st <-  max(melt_profile_st$expc, na.rm = TRUE)
table$max_POC_flux_lt <- max(melt_profile_lt$expc, na.rm = TRUE)
#PCC = particle compensation depth
table$PCC_st <- melt_profile_st$depth[which.max(melt_profile_st$expc)]
table$PCC_lt <- melt_profile_lt$depth[which.max(melt_profile_lt$expc)]
table$MLD_max_st <- print(MLD_max_st)
table$MLD_max_lt <- print(MLD_max_lt)
#% POC flux at 100m
table$hundred_st <- melt_profile_st$expc[11]/max(melt_profile_st$expc, na.rm = TRUE)*100
#mag = magnitude
table$hundred_st_mag <- melt_profile_st$expc[11]
table$five_hundred_st <- melt_profile_st$expc[34]/max(melt_profile_st$expc, na.rm = TRUE)*100
table$five_hundred_st_mag <- melt_profile_st$expc[34]
table$thousand_st <- melt_profile_st$expc[40]/max(melt_profile_st$expc, na.rm = TRUE)*100
table$thousand_st_mag <- melt_profile_st$expc[40]
table$hundred_lt <- melt_profile_lt$expc[11]/max(melt_profile_lt$expc, na.rm = TRUE)*100
table$hundred_lt_mag <- melt_profile_lt$expc[11]
table$five_hundred_lt <- melt_profile_lt$expc[34]/max(melt_profile_lt$expc, na.rm = TRUE)*100
table$five_hundred_lt_mag <- melt_profile_lt$expc[34]
table$thousand_lt <- melt_profile_lt$expc[40]/max(melt_profile_lt$expc, na.rm = TRUE)*100
table$thousand_lt_mag <- melt_profile_lt$expc[40]

setwd(save.path)
#save table output
write.csv(table, file = paste(save.name,".csv", sep = ""))

}
