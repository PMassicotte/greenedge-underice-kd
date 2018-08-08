rm(list = ls())

simulo <- read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather")

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

df %>%
  # filter(source == "Radiance (Lu)") %>% 
  filter(y == 0.5) %>% 
  ggplot(aes(x = x, y = z)) +
  geom_line() +
  facet_wrap(~source)


iso <- df %>% 
  crossing(light_proportion = c(0.1, 0.5, 0.9)) %>% 
  group_by(source, x, light_proportion) %>% 
  slice(which.min(abs(z - light_proportion)))

p <- df %>%
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() +
  facet_wrap(~ source) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  theme(panel.spacing = unit(1, "lines")) +
  labs(fill = str_wrap("Normalized number of photons", 10)) +
  geom_path(data = iso, aes(x = x, y = y, group = light_proportion), color = "white", size = 0.05) + 
  theme(legend.box = "horizontal")


simulo %>%
  bind_rows(mutate(simulo, mid_distance = -mid_distance)) %>% 
  filter(source == "Radiance (Lu)") %>% 
  ggplot(aes(x = mid_distance, y = depth, fill = value, z = value)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  facet_wrap(~ source) +
  scale_fill_viridis_c() 


simulo %>%
  bind_rows(mutate(simulo, mid_distance = -mid_distance)) %>% 
  write_csv("~/Desktop/simulo_4_sources.csv")


simulo <- read_feather("data/clean/simulo/compute-canada/simulo.feather")

simulo <- simulo %>%
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
  filter(pixel_distance_to_center <= 50) ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.

nn <- simulo %>% 
  filter(depth == 0.5) %>% 
  count(source, pixel_distance_to_center) 


nn %>% 
  ggplot(aes(x = pixel_distance_to_center, y = n)) +
  geom_path() +
  facet_wrap(~source, scales = "free")

simulo %>% 
  filter(depth == 0.5) %>% 
  filter(source == "Irradiance (Ed)") %>% 
  distinct(pixel_distance_to_center)


simulo %>% 
  filter(depth == 0.5) %>% 
  filter(source == "Irradiance (Ed)") %>% 
  count(pixel_distance_to_center) %>% 
  ggplot(aes(x = pixel_distance_to_center, y = n)) +
  geom_line()


df %>%
  filter(source == "Radiance (Lu)") %>% 
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_tile() +
  # scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() 

plotly::ggplotly()

