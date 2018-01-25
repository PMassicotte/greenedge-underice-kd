# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Calculate K from Ed and Lu at 5m increment.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())



cops <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>% 
  dplyr::select(-longitude_dec_degrees, -latitude_dec_degrees, -sun_zenith_angle_degrees, -hole_type) %>% 
  filter(lubridate::year(posixct_date_utc) == 2016)
# %>% 
#   filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002")

cops <- cops %>% 
  filter(between(wavelength, 395, 710)) %>% 
  group_by(profile_filename) %>% 
  filter(max(depth) >= 80) %>% ## Keep profils that go to at least 80 m
  ungroup() %>% 
  filter(between(depth, 10, 80))

cops <- cops %>% 
  gather(type, value, edz, luz) %>% 
  drop_na(value) %>% 
  group_by(profile_filename, type, wavelength) %>% 
  nest()

## Make sure all profiles kept are between 10 and 80 meters
stopifnot(all(map_dbl(cops$data, ~min(.$depth)) == 10))
stopifnot(all(map_dbl(cops$data, ~max(.$depth)) == 80))

## Do the models
## Create 2 columns that will serve to vary the depth at which k is calculated

dat <- cops %>%
  unnest() %>%
  expand(
    nesting(
      start_depth = seq(10, 75, by = 5),
      end_depth = seq(10, 75, by = 5) + 5),
    nesting(
      profile_filename,
      type,
      wavelength,
      depth,
      flag_quality,
      posixct_date_utc,
      value
    )
  ) %>%
  group_by(profile_filename, start_depth, end_depth, type, wavelength) %>%
  filter(between(depth, start_depth, end_depth)) %>% 
  nest()

dat

fit <- function(df) {
  
  mod <- minpack.lm::nlsLM(
    value ~ a0 * exp(-k * depth),
    data = df,
    start = list(a0 = 0, k = 0.02),
    control = nls.lm.control(maxiter =  200)
  )
  
  y <- df$value
  n <- length(y)
  
  r2 <- 1 - (sum(residuals(mod) ^ 2) / ((n - 1) * var(y)))
  k <-  coefficients(mod)[2]
  
  metrics <- data.frame(r2, k)
  
  return(metrics)
  
}

## Regression between Ed and Lu
## ~ 5 minutes

no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  library(tidyverse)
  library(minpack.lm)
})

res <- dat %>% 
  mutate(mod = pblapply(data, fit, cl = cl)) %>% 
  unnest(mod)

stopCluster(cl)

## Save K

res %>% 
  dplyr::select(-(data)) %>% 
  write_feather("data/clean/k_cops.feather")
