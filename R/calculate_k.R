# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# This script calculate Ked, Keu and Klu from COPS profiles.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(minpack.lm)

cops <- read_feather("data/clean/cops.feather")

cops <- cops %>% 
  # filter(wavelength %in% c(395, 412, 443, 465, 490, 510)) %>% 
  filter(wavelength >= 395 & wavelength <= 710) %>% 
  filter(depth >= 10)

## Do the models

cops <- cops %>% 
  gather(type, value, edz, euz, luz) %>% 
  drop_na(value) %>% 
  group_by(profile_filename, type, wavelength) %>% 
  nest() %>% 
  mutate(mod_k = map(data, ~minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 0, k = 0.02), control = nls.lm.control(maxiter =  200)))) %>% 
  mutate(pred = map2(data, mod_k, modelr::add_predictions)) %>% 
  mutate(k = map(mod_k, summary)) %>% 
  mutate(k = map(k, "coefficients")) %>% 
  mutate(k = map_dbl(k, 2))

write_rds(cops, "data/clean/cops_ked_keu_klu_models.rds", compress = "none")

# Plots -------------------------------------------------------------------

plots <- cops %>% 
  unnest(pred) %>% 
  group_by(profile_filename) %>% 
  nest() %>% 
  mutate(p = map2(data, profile_filename, function(df, profile_filename) {
    
    # print(head(df))
    
    p <- df %>% 
      # filter(wavelength == 683) %>% 
      ggplot(aes(x = value, y = depth)) +
      geom_point() +
      geom_path(aes(x = pred), color = "red") +
      scale_y_reverse() +
      facet_grid(wavelength~type, scales = "free") +
      labs(title = profile_filename)
    
  }))

pdf("graphs/cops_kd_ku_fits.pdf", height = 10, width = 6)
plots$p
dev.off()

embed_fonts("graphs/cops_kd_ku_fits.pdf")
