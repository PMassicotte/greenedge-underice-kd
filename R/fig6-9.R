rm(list = ls())

simulo <-
  read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather") %>%
  mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
  filter(pixel_distance_to_center <= 50)

reference_profile <- simulo %>%
  mutate(class_25_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.25)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_20_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.20)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_15_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.15)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_10_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.10)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_05_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.05)), include.lowest = TRUE, right = TRUE, dig.lab = 5))) %>%
  mutate(class_01_percent = as.character(cut(pixel_distance_to_center, breaks = c(0, sqrt(25 / 0.01)), include.lowest = TRUE, right = TRUE, dig.lab = 5)))

# unique(reference_profile$class_0_40)

reference_profile <- reference_profile %>%
  gather(class_distance, range, starts_with("class")) %>%
  drop_na(range)

reference_profile <- reference_profile %>%
  group_by(depth, source, range) %>%
  summarise(value = mean(value)) %>%
  ungroup()


# Fig 6 -------------------------------------------------------------------

## Calculate % of cover

cover <- c(
  "[0,10]" = "0-10.00 meters (25%)",
  "[0,11.18]" = "0-11.18 meters (20%)",
  "[0,12.91]" = "0-12.91 meters (15%)",
  "[0,15.811]" = "0-15.811 meters (10%)",
  "[0,22.361]" = "0-22.361 meters (5%)",
  "[0,50]" = "0-50.00 meters (1%)"
)

reference_profile <- reference_profile %>%
  mutate(range = cover[range])

p <- simulo %>%
  ggplot() +
  geom_path(aes(x = value, y = depth, group = interaction(x, y)), size = 0.1, alpha = 0.1) +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  scale_x_continuous(labels = scales::scientific) +
  geom_path(data = reference_profile, aes(x = value, y = depth, color = range)) +
  labs(color = "Surface radius (meters)\nMelt pond proportion (%)") +
  ylab("Depth (m)") +
  xlab("Number of photons") +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines"))

ggsave("graphs/fig6.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

# Fig 7 -------------------------------------------------------------------

## Calculate average light profiles
averaged_simulo <- simulo %>%
  mutate(iso_distance = cut_interval(pixel_distance_to_center, n = 50)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value)) %>%
  ungroup()

## Calculate K only starting at 5 meters (ice ridge)
averaged_simulo <- averaged_simulo %>%
  filter(between(mid_distance, 5, 50))

p <- averaged_simulo %>%
  ggplot(aes(x = value, y = depth, color = factor(mid_distance))) +
  geom_path() +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(color = str_wrap("Distance from the center of the melt pond (mid distance, meters)", 25)) +
  guides(col = guide_legend(ncol = 3)) +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines")) +
  scale_x_continuous(labels = scales::scientific) +
  ylab("Depth (m)") +
  xlab("Number of photons") 

ggsave("graphs/fig7.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

# Fig 8 -------------------------------------------------------------------

averaged_simulo_norm <- averaged_simulo %>%
  group_by(source, mid_distance) %>%
  mutate(value = value / max(value)) ## normalize, easier to fit

k <- averaged_simulo_norm %>%
  nest() %>%
  mutate(mod = map(data, ~ minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 1, k = 0.02)))) %>%
  mutate(k = map_dbl(mod, ~ coef(.)[2])) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions))

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
  "Irradiance (Ed)" = "Propagated with KEd",
  "Radiance (Lu)" = "Propagated with KLu"
)

p <- reference_profile %>%
  ggplot(aes(x = value, y = depth)) +
  geom_path(size = 1) +
  facet_grid(range ~ source, scales = "free", labeller = labeller(source = labels)) +
  scale_y_reverse() +
  geom_path(data = predicted_light, aes(x = predicted_light, color = factor(mid_distance)), size = 0.25) +
  labs(color = str_wrap("Distance from the center of the melt pond (meters)", 15)) +
  scale_x_continuous(labels = scales::scientific) +
  ylab("Depth (m)") +
  xlab("Number of photons") +
  theme(strip.text.y = element_text(size = 8)) +
  guides(col = guide_legend(ncol = 2))

ggsave("graphs/fig8.pdf", plot = p, device = cairo_pdf, height = 9, width = 7)

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
  ggplot(aes(x = mid_distance, y = relative_error, color = range)) +
  geom_line() +
  # geom_point(show.legend = FALSE) +
  facet_wrap(~ source, labeller = labeller(source = labels)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, by = 0.1)) +
  xlab("Distance from ice ridge (meters)") +
  ylab("Relative error") +
  labs(color = "Surface radius (meters)\nMelt pond proportion (%)") +
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.75, "lines")) 

ggsave("graphs/fig9.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)

