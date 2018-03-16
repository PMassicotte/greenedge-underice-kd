# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Export mean values of a and c in correct format for use in Hydrolight.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

iop <- read_feather("data/clean/iop.feather") %>%
  select(-mean_bbp) %>%
  drop_na(mean_a, mean_c)

iop <- iop %>%
  group_by(date) %>%
  filter(max(depth_grid) >= 150) %>%
  ungroup() %>%
  filter(date <= "2016-06-17") %>% ## We do not want bloom data
  filter(depth_grid <= 80)

iop <- iop %>%
  group_by(depth_grid, wavelength) %>%
  summarise(mean_a = mean(mean_a), mean_c = mean(mean_c), n = n()) %>%
  arrange(depth_grid)

# Plot --------------------------------------------------------------------

color <- lapply(unique(iop$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(iop$wavelength))

p1 <- iop %>%
  ggplot(aes(x = mean_a, y = depth_grid, color = factor(wavelength), group = wavelength)) +
  geom_path() +
  scale_y_reverse() +
  scale_color_manual(values = color)

p2 <- iop %>%
  ggplot(aes(x = mean_c, y = depth_grid, color = factor(wavelength), group = wavelength)) +
  geom_path() +
  scale_y_reverse() +
  scale_color_manual(values = color)

p <- cowplot::plot_grid(p1, p2, ncol = 1)

ggsave("graphs/iop/averaged_a_c_for_hydrolight.pdf", device = cairo_pdf, height = 10, width = 12)

# Format ------------------------------------------------------------------

iop <- iop %>%
  mutate(a = "a", c = "c") %>%
  unite(a, a, wavelength, remove = FALSE) %>%
  unite(c, c, wavelength, remove = FALSE) %>%
  select(-wavelength)

d1 <- iop %>%
  select(depth_grid, a, mean_a) %>%
  spread(a, mean_a)

d2 <- iop %>%
  select(depth_grid, c, mean_c) %>%
  spread(c, mean_c)

iop <- inner_join(d1, d2, by = "depth_grid")

write_csv(iop, "data/clean/averaged_a_c_for_hydrolight.csv")
