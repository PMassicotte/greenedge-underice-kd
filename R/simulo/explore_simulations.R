# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Explore SimulO simulation data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

simulo <- read_feather("data/clean/simulo/compute-canada/simulo.feather")

simulo <- simulo %>%
  mutate(source = ifelse(source == "radiance", "Lu", "Ed")) %>% 
  filter(pixel_distance_to_center <= 50) ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.

# Raster by depth ---------------------------------------------------------

p <- simulo %>%
  # filter(depth == 0.5) %>%
  group_by(depth, source) %>%
  mutate(value2 = (value - mean(value)) / sd(value)) %>%
  ggplot(aes(x = x, y = y, fill = value2)) +
  geom_raster() +
  facet_grid(depth ~ source) +
  scale_fill_viridis_c() +
  coord_equal(expand = FALSE) +
  labs(title = "SimulO simulations under melt pond (86 billions photons)") +
  labs(subtitle = "For visualization, data have been normalized (mean(value)) / sd(value))")

ggsave("graphs/simulo_simulation.pdf", device = cairo_pdf, width = 7, height = 46)


# 2D plot -----------------------------------------------------------------

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

# brks <- seq(0, 1, length.out = 10)

p <- df %>%
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() +
  facet_wrap(~ source) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  labs(title = "2D cut of radiance and irradiance from SimulO") +
  labs(subtitle = "For each panel, data have been normalized to its maximum (value = value / max(value))")
# stat_contour(aes(colour = ..level..), breaks = brks, size = 0.1)

ggsave("graphs/simulo_2d_cut.pdf", width = 10, height = 6, device = cairo_pdf)

# Ed and Lu profiles ------------------------------------------------------

res <- simulo %>%
  mutate(iso_distance = cut_interval(pixel_distance_to_center, n = 200)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value))

avg <- res %>%
  group_by(source, depth) %>%
  summarise(average_value = mean(value))

p1 <- res %>%
  ggplot(aes(x = value, y = depth, color = mid_distance, group = mid_distance)) +
  geom_path() +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  scale_color_distiller(palette = "Blues") +
  ylab("Depth (m)") +
  labs(color = "Distance from the\ncenter of the melt pond (m)") +
  geom_path(data = avg, aes(x = average_value, y = depth), inherit.aes = FALSE, size = 2) +
  theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))

p2 <- avg %>%
  group_by(source) %>%
  mutate(average_value = (average_value - mean(average_value)) / sd(average_value)) %>%
  ggplot(aes(x = average_value, y = depth, color = source)) +
  geom_line() +
  scale_y_reverse() +
  labs(subtitle = "For visualization, data have been normalized (mean(value)) / sd(value))") +
  theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))

p3 <- avg %>%
  filter(depth >= 10) %>%
  group_by(source) %>%
  mutate(average_value = (average_value - mean(average_value)) / sd(average_value)) %>%
  ggplot(aes(x = average_value, y = depth, color = source)) +
  geom_line() +
  scale_y_reverse() +
  labs(subtitle = "For visualization, data have been normalized (mean(value)) / sd(value))") +
  theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))

p <- cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO")

ggsave("graphs/simulo_ed_lu_profiles.pdf", device = cairo_pdf, height = 12)
