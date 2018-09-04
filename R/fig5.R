# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# 2D plan of irradiance and radiance.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())


simulo <- read_feather("data/clean/simulo/compute-canada/simulo_4_lambertian_sources.feather") %>% 
  filter(between(pixel_distance_to_center, 0, 50))

simulo <- simulo %>%
  mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value)) %>%
  ungroup()

simulo <- simulo %>% 
  spread(source, value)

res <- simulo %>%
  group_by(depth) %>%
  nest() %>%
  mutate(mod_gam = map(data,  ~gam(.$radiance ~ s(.$mid_distance, fx = FALSE, k=10, bs = "cr")) , data = .)) %>% 
  mutate(pred_sf = map(data, function(x) {
    
    sf <- smooth.spline(x$mid_distance, x$radiance, spar = 0.75)
    tibble(pred_sf = sf$y)
    
  })) %>% 
  mutate(pred_gam = map(mod_gam, predict)) %>% 
  unnest(data, pred_gam, pred_sf) 

p1 <- res %>%
  filter(depth == 5) %>% 
  ggplot(aes(x = mid_distance, y = intensity)) +
  geom_point() +
  ylab("Number of irradiance photon") +
  xlab("Distance from the center of the melt pond (m)") +
  theme(text = element_text(size = 10))

p2 <- res %>%
  filter(depth == 5) %>% 
  ggplot(aes(x = mid_distance, y = radiance)) +
  geom_point() +
  geom_line(aes(y = pred_gam), color = "red") +
  ylab("Number of radiance photon") +
  xlab("Distance from the center of the melt pond (m)") +
  theme(text = element_text(size = 10))

p <- cowplot::plot_grid(p1, p2, labels = "AUTO", align = "hv")  
ggsave("graphs/supp_fig_5.pdf", width = 7, height = 3, device = cairo_pdf)

res <- res %>% 
  select(-pred_sf, -radiance) %>% 
  rename(radiance = pred_gam) %>% 
  gather(source, value, intensity, radiance)

# res %>%
#   ggplot(aes(x = mid_distance, y = value)) +
#   geom_line() +
#   # geom_line(data = res, aes(x = mid_distance, y = pred_gam, color = "pred_gam")) +
#   # geom_line(data = res, aes(x = mid_distance, y = pred_sf, color = "pred_sf")) +
#   facet_wrap(~depth + source, scales = "free_y")

simulo <- res %>% 
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)"))

## So we can reuse it later
write_feather(simulo, "data/clean/simulo/compute-canada/simulo_4_lambertian_sources_smoothed_radiance.feather")

# Interpolation -----------------------------------------------------------

df <- simulo %>%
  group_by(source) %>%
  nest() %>%
  mutate(interpolated = map(data, ~akima::interp(.$mid_distance, .$depth, .$value, nx = 125, ny = 125))) %>%
  mutate(interpolated = map(interpolated, ~akima::interp2xyz(., data.frame = TRUE))) %>%
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
  crossing(light_proportion = seq(0, 1, length.out = 6)) %>%
  group_by(source, x, light_proportion) %>%
  nest() %>%
  mutate(iso = map2_dbl(data, light_proportion, function(x, lp) {
    x %>%
      ggplot(aes(x = z, y = y)) +
      geom_path() +
      scale_y_reverse()

    sf <- with(x, approxfun(z, y))
    sf(lp)
  })) %>%
  group_by(source, x, light_proportion) %>%
  drop_na()

p <- df %>%
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() +
  facet_wrap(~source) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  theme(panel.spacing = unit(1, "lines")) +
  labs(fill = str_wrap("Normalized number of photons", 10)) +
  # geom_line(data = iso, aes(x = x, y = iso, group = light_proportion), color = "white", size = 0.1, inherit.aes = FALSE) +
  theme(legend.box = "horizontal")

p

ggsave("graphs/fig5.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

