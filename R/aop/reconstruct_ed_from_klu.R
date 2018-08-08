# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Reconstruct Ed and Lu profiles from calculated KLu.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

propagate <- function(df, init = 0.001000000) {
  df <- pmap(df, ~ exp(..3 * (..2 - seq(..1, ..2, by = 1)))) %>%
    accumulate_right(~ .x[1] * .y, .init = init) %>%
    head(., -1) %>%
    set_names(df$start_depth) %>%
    as_tibble() %>%
    gather(start_depth, e, convert = TRUE) %>%
    group_by(start_depth) %>%
    mutate(depth = start_depth + 0:5) %>%
    distinct(e, depth, .keep_all = TRUE)
}

k <- read_feather("data/clean/k_cops.feather") %>%
  filter(r2 > 0.99) %>%
  filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002" & type == "edz" & wavelength == 420) %>%
  select(start_depth, end_depth, k, wavelength)

df <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>% 
  filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002") %>% 
  filter(wavelength == 420) %>% 
  select(depth, profile_filename, wavelength, edz, luz)

k %>% 
  left_join(df)

k <- k %>%
  group_by(wavelength) %>%
  nest() %>%
  mutate(prop = map(data, propagate, init = 0.0002705336)) ## Trouver le moyen

k %>%
  unnest(prop) %>%
  ggplot(aes(x = e, y = depth, color = "Reconstructed")) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~wavelength, scales = "free")



last_plot() +
  geom_line(data = df, aes(x = edz, color = "Measured"))
