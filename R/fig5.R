# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure showing a 2D cut of light regime.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/simulo/tidy_simulo.R")

simulo <- read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather")

simulo <- simulo %>%
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
  filter(pixel_distance_to_center <= 50) ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.

df <- simulo %>%
  filter(y == 125) %>%
  group_by(depth, source) %>%
  mutate(value = value / max(value))

df <- df %>%
  group_by(source) %>%
  nest() %>%
  mutate(interpolated = map(data, ~ akima::interp(.$x, .$depth, .$value, nx = 125, ny = 125))) %>%
  mutate(interpolated = map(interpolated, ~ akima::interp2xyz(., data.frame = TRUE))) %>%
  unnest(interpolated)

p <- df %>%
  ggplot(aes(x = x - 125, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c(limits = c(0.2, 1)) +
  facet_wrap(~ source) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  theme(panel.spacing = unit(1, "lines")) +
  labs(fill = str_wrap("Normalized number of photons", 10))

ggsave("graphs/fig5.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)
