# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Compare Ked and Klu calculated from COPS data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

k <- read_feather("data/clean/k_cops.feather")

k <- k %>% 
  filter(r2 >= 0.99) %>% 
  select(profile_filename, start_depth, end_depth, type, wavelength, k) %>% 
  spread(type, k) %>% 
  rename(ked = edz, klu = luz) %>% 
  unite(depth_range, start_depth, end_depth, remove = FALSE, sep = "-") %>% 
  drop_na(klu, ked)

color <- lapply(unique(k$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(k$wavelength))


p1 <- k %>% 
  ggplot(aes(x = klu, y = ked, label = profile_filename)) +
  geom_point(aes(color = factor(wavelength))) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") + 
  facet_wrap(~depth_range, scales = "free") +
  scale_color_manual(values = color)

p2 <- k %>% 
  filter(wavelength <= 560) %>%
  ggplot(aes(x = klu, y = ked, label = profile_filename)) +
  geom_point(aes(color = factor(wavelength))) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") + 
  facet_wrap(~depth_range, scales = "free") +
  scale_color_manual(values = color)

p3 <- k %>% 
  ggplot(aes(x = klu, y = ked, label = profile_filename)) +
  geom_point(aes(color = factor(depth_range))) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") + 
  facet_wrap(~wavelength, scales = "free")

ggsave("graphs/cops/ked_vs_klu_v1.pdf", p1, width = 14, height = 9, device = cairo_pdf)
ggsave("graphs/cops/ked_vs_klu_v2.pdf", p2, width = 14, height = 9, device = cairo_pdf)
ggsave("graphs/cops/ked_vs_klu_v3.pdf", p3, width = 14, height = 9, device = cairo_pdf)
