# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Plot COPS data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops.feather")

plot_cops <- function(df, profile_filename, date) {
  
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
    guides(color = guide_legend(ncol = 2)) +
    labs(color = "Wavelengths") 
}


# All the data ------------------------------------------------------------

res <- cops %>% 
  group_by(profile_filename, posixct_date_utc) %>% 
  nest() %>% 
  mutate(p = pmap(list(data, profile_filename, as.character(posixct_date_utc)), plot_cops))

## Plot all profils
pdf("graphs/cops/cops.pdf", height = 4, width = 8)
res$p
dev.off()

embed_fonts("graphs/cops/cops.pdf")