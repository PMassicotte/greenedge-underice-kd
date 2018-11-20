# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# 2D plan of irradiance and radiance.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

scientific_10x <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}

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

# Fit a Guassian curve on raidance data to remove noise -------------------

fit_gaussian <- function(df, depth) {
  
  print(depth)
  
  mod <- minpack.lm::nlsLM(
    radiance ~ a1 * exp(-((mid_distance - b1) ^ 2 / (2 * c1 ^ 2))) + k,
    data = df,
    start = list(
      a1 = mean(df$radiance),
      b1 = 0,
      c1 = 5,
      k = mean(df$radiance)
    ),
    lower = c(
      a1 = 0,
      b1 = 0,
      c1 = 0,
      k = 0
    ),
    upper = c(
      a1 = max(df$radiance),
      b1 = 0,
      c1 = 100,
      k = max(df$radiance)
    )
  )
  
  return(mod)
}

simulo <- simulo %>% 
  bind_rows(mutate(simulo, mid_distance = -mid_distance)) %>% 
  distinct()

simulo <- simulo %>% 
  spread(source, value)

simulo <- simulo %>%
  group_by(depth) %>%
  nest() %>%
  mutate(mod = map2(data, depth, fit_gaussian)) %>% 
  mutate(pred = map2(data, mod, add_predictions)) %>% 
  unnest(pred)

p1 <- simulo %>% 
  filter(depth %in% c(0.5, 5, 10, 15, 20, 25)) %>% 
  ggplot(aes(x = mid_distance, y = intensity)) +
  geom_line() +
  facet_wrap(~depth, scales = "free") +
  xlab("Distance from the center of the melt pond (m)") +
  ylab("Number of irradiance photons") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scientific_10x)

p2 <- simulo %>% 
  filter(depth %in% c(0.5, 5, 10, 15, 20, 25)) %>% 
  ggplot(aes(x = mid_distance, y = radiance)) +
  geom_line(size = 0.1) +
  geom_line(aes(y = pred, color = "red")) +
  facet_wrap(~depth, scales = "free") +
  xlab("Distance from the center of the melt pond (m)") +
  ylab("Number of radiance photons") +
  theme(legend.position = "none")

p <- cowplot::plot_grid(p1, p2, labels = "AUTO", align = "hv", ncol = 1)  
ggsave("graphs/supp_fig_2.pdf", width = 7, height = 6, device = cairo_pdf)


## Replace radiance data with smoothed value (Gaussian fits)
simulo <- simulo %>% 
  select(-radiance) %>% 
  rename(radiance = pred) %>% 
  gather(source, value, intensity, radiance) %>% 
  filter(mid_distance >= 0) %>% 
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

# df <- df %>%
#   group_by(source) %>%
#   mutate(z = z / max(z))

## Idea of Marcel
df <- df %>%
  group_by(source) %>%
  mutate(z = z / z[x == 50 & y == 0.5])

df <- df %>%
  group_by(source) %>%
  mutate(z = log(z))

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
  mutate(source2 = ifelse(source == "Irradiance (Ed)", "Downward~irradiance~(italic(E[d]))", "Upward~radiance~(italic(L[u]))")) %>% 
  ggplot(aes(x = x, y = y, fill = z, z = z)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0), name = "Depth (m)") +
  scale_fill_viridis_c() +
  facet_wrap(~source2, labeller = label_parsed) +
  scale_x_continuous(expand = c(0, 0), name = "Horizontal distance (m)") +
  theme(panel.spacing = unit(1, "lines")) +
  labs(fill = str_wrap("Normalized number of photons", 10)) +
  # geom_line(data = iso, aes(x = x, y = iso, group = light_proportion), color = "white", size = 0.1, inherit.aes = FALSE) +
  theme(legend.box = "horizontal")

p

ggsave("graphs/fig6.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

