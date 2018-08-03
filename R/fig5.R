# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# 2D plan of irradiance and radiance.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

simulo <- read_feather("data/clean/simulo/compute-canada/simulo.feather")

simulo <- simulo %>%
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
  filter(pixel_distance_to_center <= 50) ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.

simulo <- simulo %>%
  mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value)) %>%
  ungroup()

df <- simulo %>%
  group_by(source) %>%
  nest() %>%
  mutate(interpolated = map(data, ~ akima::interp(.$mid_distance, .$depth, .$value, nx = 125, ny = 125))) %>%
  mutate(interpolated = map(interpolated, ~ akima::interp2xyz(., data.frame = TRUE))) %>%
  unnest(interpolated)

df <- df %>%
  group_by(source) %>%
  mutate(z = z / max(z))

## Just "mirror" the x-ditances, this makes a better looking plot
df <- df %>% 
  bind_rows(mutate(df, x = -x))

iso <- df %>%
  ungroup() %>%
  distinct() %>%
  mutate(light_at_surface = 1) %>%
  crossing(light_proportion = seq(0, 1, length.out = 10)) %>%
  group_by(source, x, light_proportion) %>%
  nest() %>%
  mutate(iso = map2_dbl(data, light_proportion, function(x, lp) {
    x %>%
      ggplot(aes(x = z, y = y)) +
      geom_path() +
      scale_y_reverse()

    sf <- with(x, approxfun(z, y))
    sf(lp)
  }))
  
p <- df %>%
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() +
  facet_wrap(~ source) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  theme(panel.spacing = unit(1, "lines")) +
  labs(fill = str_wrap("Normalized number of photons", 10)) +
  geom_line(data = iso, aes(x = x, y = iso, group = light_proportion), color = "white", size = 0.1, inherit.aes = FALSE) + 
  theme(legend.box = "horizontal")

ggsave("graphs/fig5.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

