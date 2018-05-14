# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Compare Ked and Klu with only the profiles showing subsurface light maximum.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/simulo/tidy_simulo.R")

simulo_melt_pond <- read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather")
load("data/clean/simulo/edouard/SimulKd_NoMeltP_150m.RData")
simulo_open_water <- tidy_simulo(DataS)
rm(DataS)

df <- list(simulo_melt_pond = simulo_melt_pond, simulo_open_water = simulo_open_water) %>%
  bind_rows(.id = "type") %>%
  filter(source != "radiance" | type != "simulo_open_water") %>%
  filter(pixel_distance_to_center <= 50) %>%
  mutate(id = interaction(type, source, x, y))

# Explore -----------------------------------------------------------------

df

df %>%
  filter(x == 125 & y == 120) %>%
  filter(depth >= 5) %>%
  group_by(id) %>%
  mutate(value = (value - mean(value)) / sd(value)) %>%
  # mutate(value =  value / max(value)) %>%
  ggplot(aes(x = value, y = depth, group = id, label = id, color = type, linetype = source)) +
  geom_path() +
  scale_y_reverse()

# +
#   facet_wrap(~source, scales = "free")


# Find which Ed profiles show subsurface maxima ---------------------------

res <- df %>%
  group_by(type, source, depth, pixel_distance_to_center) %>%
  summarise(value = mean(value), n = n())

res <- res %>%
  group_by(pixel_distance_to_center) %>%
  filter(any(max(value[depth == 0.5]) < max(value))) %>%
  ungroup()

res %>%
  ggplot(aes(
    x = value,
    y = depth,
    group = interaction(pixel_distance_to_center, type),
    color = type
  )) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ source, scales = "free")

# Calculate K -------------------------------------------------------------

k <- res %>%
  group_by(type, source, pixel_distance_to_center) %>%
  nest() %>%
  mutate(mod = map(data, ~ minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 10000, k = 0.02)))) %>%
  mutate(k = map_dbl(mod, ~ coef(.)[2]))

## Visualise the fits

k %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  unnest(pred) %>%
  # filter(pixel_distance_to_center == 7) %>% 
  ggplot(aes(x = value, y = depth, group = interaction(pixel_distance_to_center, type))) +
  geom_point() +
  geom_path(aes(x = pred, color = "Model")) +
  scale_y_reverse() +
  facet_grid(round(pixel_distance_to_center, digits = 2) ~ source + type, scales = "free")

## Difference in k?

k %>%
  ggplot(aes(x = type, y = k, fill = source)) +
  geom_boxplot()

k %>% 
  ggplot(aes(x = pixel_distance_to_center, y = k, color = source, pch = type)) +
  geom_line() +
  geom_point() 

# How the error on K influence PP estimates -------------------------------

## 1 - calculate mean PAR from COPS
cops <- read_feather("data/clean/cops.feather") %>% 
  filter(between(wavelength, 400, 700)) %>% 
  filter(lubridate::year(posixct_date_utc) == 2016) %>% 
  select(depth:posixct_date_utc, wavelength, edz) %>% 
  drop_na(edz) %>% 
  mutate(posixct_date_utc = as.Date(posixct_date_utc))

par <- cops %>% 
  group_by(profile_filename, posixct_date_utc) %>% 
  filter(depth == min(depth)) %>% 
  group_by(posixct_date_utc, wavelength) %>% 
  summarise(edz = mean(edz), n = n()) %>% 
  nest() %>% 
  mutate(par = map_dbl(data, ~pracma::trapz(.$wavelength, .$edz)))

par %>% 
  ggplot(aes(x = posixct_date_utc, y = par)) +
  geom_point() +
  geom_line()

mean_par <- mean(par$par)

## 2 - Photosynthetic parameters

read
