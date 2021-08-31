#' @title Grab Metadata
#' @author Stevie Walker
#' @description function to get and save metadata from combined nc files to assist with plotting later
#' @input list of combined netcdf files
#' @output text files with netcdf file metadata
#' @description June 2021

#data.path <- "~/senior_thesis/combined_CESM_files"
#model.files <- list.files("~/senior_thesis/combined_CESM_files/")

grab_metadata <- function(model.files, data.path) {

for(p in model.files) {
  file.name <- p
  new.name <-  strsplit(file.name, ".nc")
  save.name <- paste(new.name,".txt", sep = "")
  setwd(data.path)
  nc_data <- nc_open(p)
  #switching to metadata save directory
  setwd("~/senior_thesis/ncfile_metadata/")
  # Save the metadata to a text file
  sink(save.name)
  print(nc_data)
  sink()
  
}

}