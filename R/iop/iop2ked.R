# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Convert IOPs to Kd.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

# Calculate mean IOP ------------------------------------------------------

## Calculate the mean value of IOPs for each slice of 5m depth.

iop <- read_feather("data/clean/iop_interpolated.feather")

iop <- iop %>%
  expand(
    nesting(
      start_depth = seq(10, 75, by = 5),
      end_depth = seq(10, 75, by = 5) + 5),
    nesting(
      date,
      wavelength,
      depth_grid,
      mean_a_interpol,
      mean_bbp_interpol
    )
  ) %>%
  group_by(date, start_depth, end_depth, wavelength) %>% 
  filter(between(depth_grid, start_depth, end_depth)) %>% 
  summarise_at(vars(starts_with("mean")), .funs = mean, na.rm = TRUE)

iop

color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

df %>% 
  ggplot(aes(x = mean_a_interpol, group = wavelength, fill = factor(wavelength))) +
  geom_histogram() +
  facet_wrap(~start_depth + end_depth) +
  scale_fill_manual(values = color)
  
df %>% 
  ggplot(aes(x = mean_bbp_interpol, group = wavelength, fill = factor(wavelength))) +
  geom_histogram() +
  facet_wrap(~start_depth + end_depth) +
  scale_fill_manual(values = color)
