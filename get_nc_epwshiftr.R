# attempt to use CMIP6 downloading package

# set directory to store files
options(epwshiftr.dir = tempdir())
options(epwshiftr.verbose = TRUE)

# get CMIP6 data nodes
(nodes <- get_data_node())

# create a CMIP6 output file index
idx <- init_cmip6_index(
  # only consider ScenarioMIP activity
  activity = "ScenarioMIP",
  
  # specify variables POC, NPP, and MLD
  variable = c("expc", "epc100", "pp", "ppos", "mlotst", "mlotstmax", "mlotstmin"),
  
  # specify report frequency
  frequency = c("yr", "mon"),
  
  # specify experiment name
  experiment = c("ssp585"),
  
  # specify GCM names
  source = c("CESM2", "CESM2-WACCM", "GFDL-ESM4", "MPI-ESM1-2-HR", 
             "MPI-ESM1-2-LR", "IPSL-CM6A-LR", "UKESM1-0-L"),
  
  # specify variant,
  variant = NULL,
  
  # save to data dictionary
  save = TRUE
)

#saving CMIP6 data list
write.csv(idx,"~/walker_thesis/cmip6_index.csv", row.names = TRUE)

esgf_query(
  activity = "ScenarioMIP",
  variable = c("expc", "epc100", "pp", "ppos", "mlotst", "mlotstmax", "mlotstmin"),
  frequency = c("yr", "mon"),
  experiment = "ssp585",
  source = c("CESM2", "CESM2-WACCM", "GFDL-ESM4", "MPI-ESM1-2-HR", 
             "MPI-ESM1-2-LR", "IPSL-CM6A-LR", "UKESM1-0-L"),
  variant = NULL,
  replica = FALSE,
  latest = TRUE,
  resolution = NULL,
  type = "Dataset",
  limit = 10000L,
  data_node = NULL
)
