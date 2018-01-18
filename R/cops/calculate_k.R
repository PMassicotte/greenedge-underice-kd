# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Calculate K from Ed and Lu at 5m increment.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops.feather") %>% 
  dplyr::select(-longitude_dec_degrees, -latitude_dec_degrees, -sun_zenith_angle_degrees, -hole_type) %>% 
  filter(lubridate::year(posixct_date_utc) == 2016) %>% 
  dplyr::select(-euz)

cops <- cops %>% 
  filter(wavelength >= 395 & wavelength <= 710) %>% 
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

# Do the models

# cops <- cops %>% 
#   filter(wavelength == 395 & type == "edz")
# filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002" & wavelength == 395 & type == "edz")

## Create 2 columns that will serve to vary the depth at which k is calculated

dat <- cops %>%
  unnest() %>%
  expand(
    nesting(
      start_depth = c(1, seq(10, 75, by = 5)),
      end_depth = c(80, seq(10, 75, by = 5) + 5)),
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
  nest()

dat

fit <- function(df, start_depth, end_depth) {
  
  df2 <- df %>%
    filter(between(depth, start_depth, end_depth))
  
  mod <- minpack.lm::nlsLM(
    value ~ a0 * exp(-k * depth),
    data = df2,
    start = list(a0 = 0, k = 0.02),
    control = nls.lm.control(maxiter =  200)
  )
  
  return(mod)
  
}

## Compute R2 for each model
get_r2 <- function(pred, start_depth, end_depth) {
  
  df2 <- pred %>%
    filter(between(depth, start_depth, end_depth))
  
  return(cor(df2$value, df2$pred)^2)
  
}

## Regression between Ed and Lu
## ~ 5 minutes
res <- dat %>% 
  mutate(mod = pmap(list(data, start_depth, end_depth), fit)) %>% 
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>% 
  mutate(r2 = pmap(list(pred, start_depth, end_depth), get_r2)) %>% 
  unnest(r2)

k <- res %>% 
  # filter(wavelength %in% c(395, 443)) %>% 
  mutate(k = map(mod, summary)) %>% 
  mutate(k = map(k, "coefficients")) %>% 
  mutate(k = map_dbl(k, 2)) %>% 
  unite(depth_range, start_depth, end_depth, sep = "-")

k <- k %>%
  mutate(depth_range2 = factor(depth_range, levels = unique(depth_range)))

## Save K in both feather and rds format

k %>% 
  dplyr::select(-(data:pred)) %>% 
  write_feather("data/clean/k_cops.feather")

