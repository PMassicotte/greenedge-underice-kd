rm(list = ls())

scientific_10x <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}

source("R/simulo/simulo_utils.R")

simulo <- read_feather("data/clean/simulo/compute-canada/simulo_4_lambertian_sources_smoothed_radiance.feather")

reference_profile <- simulo %>%
  mutate(class_25_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.25)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_20_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.20)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_15_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.15)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_10_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.10)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_05_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.05)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_01_percent = as.character(cut(mid_distance, breaks = c(0, sqrt(25 / 0.01)), include.lowest = TRUE, right = TRUE, dig.lab = 5)))

reference_profile <- reference_profile %>%
  gather(class_distance, range, starts_with("class")) %>%
  drop_na(range)

reference_profile <- reference_profile %>%
  group_by(depth, source, range) %>%
  summarise(value = mean(value)) %>%
  ungroup()

reference_profile %>% 
  group_by(source) %>% 
  count(range)

# Fig 6 -------------------------------------------------------------------

## Calculate % of cover

cover <- c(
  "[0,10]" = "0-10.00 m (25%)",
  "[0,11.18]" = "0-11.18 m (20%)",
  "[0,12.91]" = "0-12.91 m (15%)",
  "[0,15.811]" = "0-15.81 m (10%)",
  "[0,22.361]" = "0-22.36 m (5%)",
  "[0,50]" = "0-50.00 m (1%)"
)

reference_profile <- reference_profile %>%
  mutate(range = cover[range])

p1 <- simulo %>%
  filter(source == "Irradiance (Ed)") %>% 
  ggplot() +
  # geom_path(aes(x = value, y = depth, group = mid_distance), size = 0.1, alpha = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(labels = scientific_10x) +
  geom_path(data = reference_profile %>% filter(source == "Irradiance (Ed)"), aes(x = value, y = depth, color = range)) +
  labs(color = "Surface radius (m)\nMelt pond proportion (%)") +
  ylab("Depth (m)") +
  xlab("Number of photons") +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines")) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  xlab("Downward irradiance (a.u.)")

p2 <- simulo %>%
  filter(source == "Radiance (Lu)") %>% 
  ggplot() +
  # geom_path(aes(x = value, y = depth, group = mid_distance), size = 0.1, alpha = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(labels = scientific_10x) +
  geom_path(data = reference_profile %>% filter(source == "Radiance (Lu)"), aes(x = value, y = depth, color = range)) +
  labs(color = "Surface radius (m)\nMelt pond proportion (%)") +
  ylab("Depth (m)") +
  xlab("Number of photons") +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines")) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Upward radiance (a.u.)")

p <- p1 +
  p2 +
  plot_layout(ncol = 2)

ggsave("graphs/fig7.pdf", plot = p, device = cairo_pdf, height = 2.5, width = 7)

## Some stats for the paper

simulo %>% 
  group_by(source) %>% 
  summarise(min(value), max(value))

simulo %>% 
  filter(depth == 0.5) %>% 
  group_by(source) %>% 
  summarise(n = n())

# Fig 7 -------------------------------------------------------------------

# simulo <- read_feather("data/clean/simulo/compute-canada/simulations-with-15m-melt-pond-4-lambertian-sources.feather") %>%
#   mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
#   filter(between(pixel_distance_to_center, 10, 60)) %>%
#   mutate(pixel_distance_to_center = pixel_distance_to_center - 10)

simulo <- read_feather("data/clean/simulo/compute-canada/simulo_4_lambertian_sources.feather") %>%
  filter(between(pixel_distance_to_center, 0, 50))

## Calculate average light profiles
averaged_simulo <- simulo %>%
  mutate(iso_distance = cut_interval(pixel_distance_to_center, n = 50)) %>%
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
      c1 = 5, # Limit to 5 because some have very high value outliers at 0 m (center of the melt pond)
      k = 0
    ),
    upper = c(
      a1 = max(df$radiance),
      b1 = 0,
      c1 = Inf,
      k = max(df$radiance)
    ),
    control = nls.lm.control(maxiter = 1024)
  )
  
  return(mod)
}


averaged_simulo <- averaged_simulo %>% 
  bind_rows(mutate(averaged_simulo, mid_distance = -mid_distance)) %>% 
  distinct()

averaged_simulo <- averaged_simulo %>% 
  spread(source, value)

averaged_simulo <- averaged_simulo %>%
  group_by(depth) %>%
  nest() %>%
  mutate(mod = map2(data, depth, fit_gaussian)) %>% 
  mutate(pred = map2(data, mod, add_predictions)) %>% 
  unnest(pred)

averaged_simulo %>% 
  filter(depth %in% c(0.5, 5, 10, 15, seq(20, 25, by = 0.5))) %>% 
  ggplot(aes(x = mid_distance, y = radiance)) +
  geom_line(size = 0.1) +
  geom_line(aes(y = pred, color = "red")) +
  facet_wrap(~depth, scales = "free") +
  xlab("Distance from the center of the melt pond (m)") +
  ylab("Number of radiance photons") +
  theme(legend.position = "none")

## Replace radiance data with smoothed value (Gaussian fits)
averaged_simulo <- averaged_simulo %>% 
  select(-radiance) %>% 
  rename(radiance = pred) %>% 
  gather(source, value, intensity, radiance) %>% 
  filter(mid_distance >= 0) %>% 
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)"))

## Calculate K only starting at 0 meters
averaged_simulo <- averaged_simulo %>%
  filter(between(mid_distance, 0, 50))

p1 <- averaged_simulo %>%
  filter(source == "Irradiance (Ed)") %>% 
  ggplot(aes(x = value, y = depth, color = factor(mid_distance))) +
  geom_path() +
  # facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(color = str_wrap("Distance from the center of the melt pond (mid distance, m)", 25)) +
  guides(col = guide_legend(ncol = 3)) +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.5, "lines")) +
  scale_x_continuous(labels = scientific_10x) +
  ylab("Depth (m)") +
  xlab("Downward irradiance (a.u.)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  theme(legend.position = "none")

p2 <- averaged_simulo %>%
  filter(source == "Radiance (Lu)") %>% 
  ggplot(aes(x = value, y = depth, color = factor(mid_distance))) +
  geom_path() +
  # facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(color = str_wrap("Distance from the center of the melt pond (mid distance, m)", 25)) +
  guides(col = guide_legend(ncol = 3)) +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.5, "lines")) +
  scale_x_continuous(labels = scientific_10x) +
  ylab("Depth (m)") +
  xlab("Upward radiance (a.u.)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  theme(legend.margin = margin(t = 1, unit = 'cm'))

p <- p1 +
  p2 +
  plot_layout(ncol = 2)

ggsave("graphs/fig8.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

# Fig 8 -------------------------------------------------------------------

averaged_simulo_norm <- averaged_simulo %>%
  group_by(source, mid_distance) %>%
  mutate(value = value / max(value)) ## normalize, easier to fit

k <- averaged_simulo_norm %>%
  nest() %>%
  mutate(mod = map(data, ~ minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 1, k = 0.02)))) %>%
  mutate(k = map_dbl(mod, ~ coef(.)[2])) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>% 
  mutate(r2 = map2_dbl(mod, data, modelr::rsquare))

## Supplementary figure

k %>% 
  group_by(source) %>% 
  summarise_if(is.numeric, funs(min, max))

p <- k %>% 
  ggplot(aes(x = mid_distance, y = k, color = source)) +
  geom_point() +
  geom_line() +
  xlab("Distance from the center of the melt pond (m)") +
  ylab(bquote("Attenuation coefficient"~(m^{-1}))) +
  theme(legend.position = c(0.95, 0.95), legend.justification = c(1, 1)) +
  labs(color = "Radiometric quantity") +
  scale_color_manual(
    breaks = c("Irradiance (Ed)", "Radiance (Lu)"),
    values = RColorBrewer::brewer.pal(3, "Set1"),
    labels = c(bquote(Downward~irradiance~(E[d])), bquote(Upward~radiance~(L[u])))
  )

ggsave("graphs/fig9.pdf", width = 5.5, height = 4, device = cairo_pdf)

## Propagate light 

k <- k %>%
  select(source, mid_distance, k)

predicted_light <- reference_profile %>%
  filter(source == "Irradiance (Ed)") %>%
  filter(depth == 0.5) %>% ## Propagate the value at 0.5m
  bind_rows(mutate(., source = ifelse(source == "Irradiance (Ed)", "Radiance (Lu)", "source"))) %>%
  left_join(k) %>%
  group_by(source, range, mid_distance) %>%
  nest() %>%
  mutate(predicted_light = map(data, function(x) {
    depth <- seq(0.5, 25, by = 0.5)
    predicted_light <- x$value * exp(-x$k * (depth - 0.5))
    
    tibble(depth, predicted_light)
  })) %>%
  unnest(predicted_light)

## Only keep Ed profiles. Double the rows of the df and set the new lines as
## "radiance". This is jsut to make computation easier.
reference_profile <- reference_profile %>%
  filter(source == "Irradiance (Ed)") %>%
  bind_rows(mutate(., source = ifelse(source == "Irradiance (Ed)", "Radiance (Lu)", "source")))

## Visualize

labels <- c(
  "Irradiance (Ed)" = "Propagated with Kd",
  "Radiance (Lu)" = "Propagated with KLu"
)

predicted_light2 <- predicted_light %>% 
  mutate(source2 = ifelse(source == "Irradiance (Ed)", "Propagated~with~K[d]", "Propagated~with~K[Lu]"))

p <- reference_profile %>%
  mutate(source2 = ifelse(source == "Irradiance (Ed)", "Propagated~with~K[d]", "Propagated~with~K[Lu]")) %>% 
  ggplot(aes(x = value, y = depth)) +
  facet_grid(range ~ source2, scales = "free", labeller = labeller(source2 = label_parsed, range = label_value)) +
  scale_y_reverse() +
  geom_path(data = predicted_light2, aes(x = predicted_light, color = factor(mid_distance)), size = 0.25) +
  geom_path(size = 1) +
  labs(color = str_wrap("Distance from the center of the melt pond (m)", 20)) +
  scale_x_continuous(labels = scientific_10x, breaks = c(0, 3, 6)*1e6, limits = c(0, NA), expand = c(0.15, 0)) +
  ylab("Depth (m)") +
  xlab("Downward irradiance (a.u.)") +
  theme(strip.text.y = element_text(size = 8)) +
  guides(col = guide_legend(ncol = 2)) +
  theme_bw(base_family = "IBM Plex Sans Light")

ggsave("graphs/fig10.pdf", plot = p, device = cairo_pdf, height = 10, width = 7)

# Fig 9 -------------------------------------------------------------------

int1 <- reference_profile %>%
  group_by(source, range) %>%
  nest() %>%
  mutate(integral = map_dbl(data, ~ pracma::trapz(.$depth, .$value))) %>%
  select(-data)

int2 <- predicted_light %>%
  group_by(source, range, mid_distance) %>%
  nest() %>%
  mutate(integral = map_dbl(data, ~ pracma::trapz(.$depth, .$predicted_light))) %>%
  select(-data)

res <- left_join(int1, int2, by = c("source", "range")) %>%
  gather(type, integral, starts_with("integral")) %>%
  mutate(type = ifelse(type == "integral.x", "reference", "predicted"))

p <- res %>%
  spread(type, integral) %>%
  mutate(relative_error = (reference - predicted) / reference) %>%
  mutate(source2 = ifelse(source == "Irradiance (Ed)", "Propagated~with~K[d]", "Propagated~with~K[Lu]")) %>% 
  ggplot(aes(x = mid_distance, y = relative_error, color = range)) + 
  geom_line() +
  # geom_point(show.legend = FALSE) +
  facet_wrap(~ source2, labeller = labeller(source2 = label_parsed)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, by = 0.1)) +
  xlab("Horizontal distance from the center of the melt pond (m)") +
  ylab("Relative error") +
  labs(color = "Surface radius (m)\nMelt pond proportion (%)") +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines"))  +
  theme_bw(base_family = "IBM Plex Sans Light")

ggsave("graphs/fig11.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

## Stats for the paper

res %>%
  spread(type, integral) %>% 
  mutate(relative_error = (reference - predicted) / reference) %>% 
  group_by(source) %>% 
  summarise(mean(relative_error) * 100)

res %>% 
  spread(type, integral) %>%
  mutate(relative_error = (reference - predicted) / reference) %>% 
  group_by(source, range) %>% 
  summarise(mean(relative_error) * 100, sd(relative_error))

## Pour Simon Lambert, ratio des erreurs relatives
res %>%
  spread(type, integral) %>%
  mutate(relative_error = (reference - predicted) / reference) %>% 
  select(source, range, mid_distance, relative_error) %>% 
  spread(source, relative_error) %>% 
  janitor::clean_names() %>% 
  mutate(ratio = irradiance_ed / radiance_lu) %>% 
  # filter(range == "0-50.00 m (1%)") %>% 
  ggplot(aes(x = mid_distance, ratio, color = range)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 2)
