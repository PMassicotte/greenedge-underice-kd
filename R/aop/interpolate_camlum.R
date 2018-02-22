# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Interpolate camclum data so it matches COPS wavelengths.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

mud <- read_feather("data/clean/mud_camlum.feather") %>% 
  filter(between(depth, 10, 20))  

mud %>% 
  ggplot(aes(x = wavelength, y = mud)) +
  geom_path() +
  geom_point() +
  facet_wrap(~depth)

# Interpolation -----------------------------------------------------------

## Read cops data and extract the unique wl between 395 and 710 nm.
cops <- read_feather("data/clean/cops.feather") %>% 
  filter(between(wavelength, 395, 710)) %>% 
  distinct(wavelength)

cops_wl <- unique(cops$wavelength)

interpol <- function(df, cops_wl) {
  
  sf <- splinefun(df$wavelength, df$mud)
  mud_interpol <- sf(cops_wl)
  
  res <- data.frame(wavelength = cops_wl, mud_interpol)
  
  return(res)
  
}

mud <- mud %>% 
  group_by(depth) %>% 
  nest() %>% 
  mutate(interpol = map(data, interpol, cops_wl = cops_wl))

res <- mud %>% 
  unnest(interpol)

last_plot() +
  geom_point(data = res, aes(x = wavelength, y = mud_interpol), color = "red")


ggsave("~/Desktop/mud_interpolation.pdf", device = cairo_pdf)
