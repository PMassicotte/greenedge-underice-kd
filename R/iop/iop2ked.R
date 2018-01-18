# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Convert IOPs to Kd.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

iop <- read_feather("data/clean/iop_interpolated.feather")

df <- iop %>%
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
  nest()

