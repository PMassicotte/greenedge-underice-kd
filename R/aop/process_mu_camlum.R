# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Process mu-d data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# 1 colonne: profondeur
# Autres colonnes, les longueurs d’ondes:
# 406','438','494','510','560','628'

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

# Read the data -----------------------------------------------------------

mud <-
  readxl::read_excel(
    "data/raw/camlum/phil_camlum_169.xlsx",
    col_names = c("depth", "406", "438", "494", "510", "560", "628"),
    na = c("NaN")
  ) %>% 
  gather(wavelength, mud, -depth, convert = TRUE) %>% 
  drop_na()

mud

# Plot --------------------------------------------------------------------

color <- lapply(unique(mud$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(mud$wavelength))

mud %>% 
  ggplot(aes(x = mud, y = depth, color = factor(wavelength))) +
  geom_path() +
  geom_point() +
  scale_y_reverse() +
  labs(color = "Wavelengths") +
  xlab(bquote(mu[d]~(m^{-1}))) +
  ylab("Depth (m)") +
  scale_color_manual(values = color)

ggsave("graphs/iop/mud.pdf", device = cairo_pdf)
