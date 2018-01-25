# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Plot COPS data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>% 
  filter(lubridate::year(posixct_date_utc) == 2016)

plot_cops <- function(df, profile_filename, date) {
  
  source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")
  
  color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
  color <- setNames(color, unique(df$wavelength))
  
  
  df <- df %>% 
    gather(type, value, edz:luz) %>% 
    drop_na(value) %>% 
    filter(value <= 10000) # prevent a bug with large numbers https://github.com/tidyverse/ggplot2/issues/1957

  df %>% 
    ggplot(aes(x = value, y = depth)) +
    geom_path(aes(color = factor(wavelength))) +
    scale_y_reverse() +
    facet_wrap(~type, scales = "free") +
    labs(title = paste(profile_filename, date),
         subtitle = sprintf("Hole type: %s", unique(df$hole_type))) +
    guides(color = guide_legend(ncol = 3)) +
    labs(color = "Wavelengths") +
    scale_color_manual(values = color)
}


# All the data ------------------------------------------------------------

res <- cops %>% 
  group_by(profile_filename, posixct_date_utc) %>% 
  nest() %>% 
  mutate(p = pmap(list(data, profile_filename, as.character(posixct_date_utc)), plot_cops))

## Plot all profils
pdf("graphs/cops/cops_wavelengths_interpolated.pdf", height = 6, width = 12)
res$p
dev.off()

embed_fonts("graphs/cops/cops_wavelengths_interpolated.pdf")