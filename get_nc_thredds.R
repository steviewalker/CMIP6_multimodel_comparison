#' @title CMIP6 Bulk Data Download Function
#' This function downloads CMIP6 .nc files from the ESGF database
#' To create CMIP6 file index, use function init_cmip6_index from package epwshiftr
#' The file index is needed to obtain the wget url
#' @author Stevie Walker
#' @date January 2021

get_nc_thredds <- function(cmip6.index, save.path) {

#downloading example with one file
#system("wget http://esgf-data.ucar.edu/thredds/fileServer/esg_dataroot/CMIP6/ScenarioMIP/NCAR/CESM2-WACCM/ssp585/r1i1p1f1/Oyr/pp/gn/v20190815/pp_Oyr_CESM2-WACCM_ssp585_r1i1p1f1_gn_2015-2100.nc")

#subsetting the file url column
  #change for manually made csv urls
#file_url <- cmip6.index[22]
  file_url <- cmip6.index

#creating url strings to paste into system code
for (i in file_url) {
  system_code <- paste("wget", i, sep = " ")
}

#checking format
print(system_code)

setwd(save.path)

#downloading data
for (i in system_code) {
  system(i)
}

}