# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Read all COPS files
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(parallel) 

read_cops <- function(file) {
  
  # file <- "/media/work/projects/greenedge-icecamp-physic/data/raw/cops/2016/RES.EXCEL/GE2016.ICMP_ICEP_160504_CAST_002/d.fit.v.01.txt"
  
  df <- data.table::fread(file, na.strings = "-999") %>%
    janitor::clean_names() %>%
    select(profile_filename,
           flag_quality,
           posixct_date_utc,
           depth = contains("depth"),
           hole_type,
           contains("par"))

  return(df)  
}

files_ed <- list.files("../greenedge-icecamp-physic/data/raw/cops/", pattern = "d.fit.+txt", full.names = TRUE, recursive = TRUE)
files_eu <- list.files("../greenedge-icecamp-physic/data/raw/cops/", pattern = "u.fit.+txt", full.names = TRUE, recursive = TRUE)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(tidyverse))

df_ed <- parLapply(cl, files_ed, read_cops) %>% 
  bind_rows()

df_eu <- parLapply(cl, files_eu, read_cops) %>% 
  bind_rows()

stopCluster(cl)

df <- inner_join(df_ed, df_eu) %>% 
  mutate(posixct_date_utc = lubridate::parse_date_time(posixct_date_utc, orders = "Ymd HMS")) %>% 
  as_tibble()

write_csv(df, "data/clean/cops.csv")
