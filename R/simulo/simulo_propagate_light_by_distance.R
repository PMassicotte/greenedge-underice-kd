rm(list = ls())

simulo <-
  read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather") %>%
  filter(pixel_distance_to_center <= 50)

reference_profile <- simulo %>%
  mutate(class_0_50 = as.character(cut(pixel_distance_to_center, breaks = c(0, 50), include.lowest = TRUE, right = TRUE))) %>%
  mutate(class_0_40 = as.character(cut(pixel_distance_to_center, breaks = c(0, 40), include.lowest = TRUE, right = TRUE))) %>%
  mutate(class_0_30 = as.character(cut(pixel_distance_to_center, breaks = c(0, 30), include.lowest = TRUE, right = TRUE))) %>%
  mutate(class_0_20 = as.character(cut(pixel_distance_to_center, breaks = c(0, 20), include.lowest = TRUE, right = TRUE))) %>%
  mutate(class_0_10 = as.character(cut(pixel_distance_to_center, breaks = c(0, 10), include.lowest = TRUE, right = TRUE)))

# unique(reference_profile$class_0_40)

reference_profile <- reference_profile %>%
  gather(class_distance, range, starts_with("class")) %>%
  drop_na(range)

reference_profile <- reference_profile %>%
  group_by(depth, source, range) %>%
  summarise(value = mean(value)) %>%
  ungroup()

p1 <- simulo %>%
  ggplot() +
  geom_path(aes(x = value, y = depth, group = interaction(x, y)), size = 0.1, alpha = 0.1) +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  geom_path(data = reference_profile, aes(x = value, y = depth, color = range)) +
  labs(color = "Distance range\n(meters)") +
  labs(title = "Raw SimulO data with averaged light profiles by distance from the center of the melt pond")

# hrbrthemes::gg_check(p1)

ggsave("graphs/simulo/simulo_averaged_light_profiles_by_distance_range_01.pdf", device = cairo_pdf, height = 5, width = 10)

p2 <- reference_profile %>%
  ggplot() +
  geom_path(aes(x = value, y = depth, color = range)) +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(color = "Distance range\n(meters)") +
  labs(title = "Raw SimulO data with averaged light profiles by distance from the center of the melt pond")

ggsave("graphs/simulo/simulo_averaged_light_profiles_by_distance_range_02.pdf", device = cairo_pdf, height = 5, width = 10)


# Calculate K -------------------------------------------------------------

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
  filter(between(mid_distance, 5, 20))

p3 <- averaged_simulo %>%
  ggplot(aes(x = value, y = depth, color = factor(mid_distance))) +
  geom_path() +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(color = str_wrap("Distance from the center of the melt pond (mid distance, meters)", 15)) +
  labs(title = "Averaged light profiles used to calculate K")

ggsave("graphs/simulo/simulo_averaged_light_profiles_calculated_k_01.pdf", device = cairo_pdf, height = 5, width = 10)

averaged_simulo <- averaged_simulo %>%
  group_by(source, mid_distance) %>%
  mutate(value = value / max(value)) ## normalize, easier to fit

k <- averaged_simulo %>%
  nest() %>%
  mutate(mod = map(data, ~ minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 1, k = 0.02)))) %>%
  mutate(k = map_dbl(mod, ~ coef(.)[2])) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions))

p4 <- k %>%
  unnest(pred) %>%
  ggplot(aes(x = value, y = depth)) +
  geom_path(aes(color = "Observed")) +
  facet_grid(mid_distance ~ source, scales = "free") +
  scale_y_reverse() +
  labs(title = str_wrap("Averaged light profiles and non-linear fits by distance", 50)) +
  geom_path(aes(x = pred, color = "Predicted")) +
  theme(legend.title = element_blank())

ggsave("graphs/simulo/simulo_averaged_light_profiles_calculated_k_02.pdf", device = cairo_pdf, height = 20, width = 5)

# Propagate light ---------------------------------------------------------

k <- k %>%
  select(source, mid_distance, k)

predicted_light <- reference_profile %>%
  filter(source == "intensity") %>%
  filter(depth == 0.5) %>% ## Propagate the value at 0.5m
  bind_rows(mutate(., source = ifelse(source == "intensity", "radiance", "source"))) %>%
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
  filter(source == "intensity") %>% 
  bind_rows(mutate(., source = ifelse(source == "intensity", "radiance", "source")))

## Visualize

labels <- c(
  intensity  = "Propagated with KEd",
  radiance  = "Propagated with KLu"
)

p5 <-  reference_profile %>% 
  ggplot(aes(x = value, y = depth)) +
  geom_path(size = 1) +
  facet_grid(range ~ source, scales = "free", labeller = labeller(source = labels)) +
  scale_y_reverse() +
  geom_path(data = predicted_light, aes(x = predicted_light, color = factor(mid_distance)), size = 0.25) +
  labs(color = str_wrap("K estimated at distance from ice ridge (meters)", 15)) +
  labs(title = "Predicted light profiles") +
  labs(subtitle = str_wrap("Black lines are the reference profiles calculated by distance range (0-10, 0-20, etc.). Colored lines are the predicted light profiles using K calculated at different distances from the ice ridge.", 100))

# hrbrthemes::gg_check(p4)

ggsave("graphs/simulo/simulo_propagated_light_profiles.pdf", device = cairo_pdf, height = 10, width = 8)

# Calculate errors --------------------------------------------------------

int1 <- reference_profile %>%
  group_by(source, range) %>% 
  nest() %>% 
  mutate(integral = map_dbl(data, ~pracma::trapz(.$depth, .$value))) %>% 
  select(-data)

int2 <- predicted_light %>% 
  group_by(source, range, mid_distance) %>% 
  nest() %>% 
  mutate(integral = map_dbl(data, ~pracma::trapz(.$depth, .$predicted_light))) %>% 
  select(-data) 

res <- left_join(int1, int2, by = c("source", "range")) %>%
  gather(type, integral, starts_with("integral")) %>% 
  mutate(type = ifelse(type == "integral.x", "reference", "predicted"))

p6 <- res %>% 
  spread(type, integral) %>% 
  mutate(relative_error = (reference - predicted) / reference) %>% 
  ggplot(aes(x = mid_distance, y = relative_error, color = range)) +
  geom_line() +
  geom_point() +
  facet_wrap(~source, labeller = labeller(source = labels)) +
  scale_x_continuous(breaks = seq(5.5, 20.5)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Distance from ice ridge (meters)") +
  ylab("Relative error ((reference - predicted) / reference)") +
  labs(color = "Distance range\n(meters)") +
  labs(title = "Relative error estimated by integral differences")

ggsave("graphs/simulo/simulo_propagated_light_errors.pdf", device = cairo_pdf, height = 6, width = 16)