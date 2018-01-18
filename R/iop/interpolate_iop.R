# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# This script interpolates IOPs to match wavelengths of the COPS.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

## Read cops data and extract the unique wl between 395 and 710 nm.
cops <- read_feather("data/clean/cops.feather") %>% 
  filter(between(wavelength, 395, 710)) %>% 
  distinct(wavelength)

cops_wl <- unique(cops$wavelength)

## Read iop data.
iop <- read_feather("data/clean/iop.feather") %>% 
  filter(between(wavelength, 395, 710)) %>% 
  filter(depth_grid <= 80) ## Only 0-80 m to match cops depths

# iop %>% 
#   group_by(date, depth_grid) %>%
#   summarise_at(.vars = vars(contains("mean")), funs(sum(!is.na(.)))) %>% 
#   filter_at(.vars = vars(contains("mean")), any_vars(. == 0)) %>% 
#   View()
# 
# filter(iop, date == "2016-05-13" & depth_grid == 1) %>% View()

## Interpolate data

interpol <- function(df, cops_wl) {
  
  ## Here I can do the 3 interpolations (a, c, bb)
  sf <- splinefun(df$wavelength, df$mean_a)
  mean_a_interpol <- sf(cops_wl)
  
  sf <- splinefun(df$wavelength, df$mean_bbp)
  mean_bbp_interpol <- sf(cops_wl)
  
  # sf <- splinefun(df$wavelength, df$mean_c)
  # mean_c_interpol <- sf(cops_wl)
  
  res <- data.frame(wavelength = cops_wl, mean_a_interpol, mean_bbp_interpol)
  
  return(res)
  
}

# Interpolation -----------------------------------------------------------

## Use DT, much faster
setDT(iop)
res <- iop[, interpol(.SD, cops_wl), by = .(date, depth_grid)] %>% 
  as_tibble()

write_feather(res, "data/clean/iop_interpolated.feather")

# Plot --------------------------------------------------------------------

p <- iop %>% 
  drop_na(mean_bbp) %>% 
  filter(depth_grid %in% c(10, 50)) %>% 
  ggplot(aes(x = wavelength, y = mean_bbp)) +
  geom_line() +
  geom_point() +
  facet_grid(date ~ depth_grid) +
  geom_point(data = filter(res, depth_grid %in% c(10, 50)), aes(y = mean_bbp_interpol), color = "red")

ggsave("~/Desktop/test.pdf", device = cairo_pdf, width = 6, height = 20)
