# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Read and tidy simulations done with SimulO.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/simulo/SimulO_RTools.R")
source("R/simulo/tidy_simulo.R")

# data <- load("~/Desktop/SimulKd_MeltP_C_150m.RData")

files <- list.files("~/Desktop/simulo/", pattern = "^Simul\\S+.txt$", full.names = TRUE, recursive = TRUE)
files <- files[file.size(files) > 10000000] ## Found 1 file that seems to be corrupted

data <- MergeSimulO(files)
df <- tidy_simulo(data)

saveRDS(data, file = "data/clean/simulo/compute-canada/simulo.rds")
write_feather(df, "data/clean/simulo/compute-canada/simulo.feather")
