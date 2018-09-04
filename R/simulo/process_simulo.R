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

simulation <- "simulations-with-15m-melt-pond-4-lambertian-sources"

files <- list.files(glue("/media/data4tb/simulo/{simulation}"), pattern = "^simul\\S+.txt$", full.names = TRUE, recursive = TRUE)
# files <- files[file.size(files) > 10000000] ## Found 1 file that seems to be corrupted

data <- MergeSimulO(files)
df <- tidy_simulo(data)

saveRDS(data, file = glue("data/clean/simulo/compute-canada/{simulation}.rds"))
write_feather(df, glue("data/clean/simulo/compute-canada/{simulation}.feather"))
