kseg <- function(nseg, r_max) {
  # https://www.quora.com/What-is-the-easiest-way-to-cut-a-pizza-into-11-equal-slices
  # https://math.stackexchange.com/questions/270287/how-to-divide-a-circle-into-9-rings-1-inner-circle-with-the-same-area
  r_max * sqrt(0:nseg / nseg)
   
}

simulo_melt_pond <- read_feather("data/clean/simulo/compute-canada/simulo.feather")

res <- simulo_open_water %>% 
  filter(pixel_distance_to_center <= 50) %>% 
  mutate(distance_class = cut_interval(pixel_distance_to_center, n = 50)) %>%
  # mutate(distance_class = cut_number(pixel_distance_to_center, n = 50)) %>%
  group_by(depth, source, distance_class) %>% 
  summarise(value = mean(value), n = n()) %>% 
  mutate(lower = str_match(distance_class, "(\\d+\\.?\\d*),")[, 2]) %>% 
  mutate(upper = str_match(distance_class, ",(\\d+\\.?\\d*)")[, 2]) %>% 
  mutate_at(vars(lower, upper), parse_number) %>% 
  mutate(middle = lower + (upper - lower) / 2)

## Number of pixel in each distance class
p1 <- res %>% 
  ungroup() %>% 
  distinct(distance_class, n) %>% 
  ggplot(aes(x = distance_class, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of pixel by distance") +
  geom_text(aes(label = n), vjust = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1

p2 <- res %>% 
  # filter(distance_class == "[0,0.5]") %>% 
  ggplot(aes(x = value, y = depth, group = distance_class)) +
  geom_path() +
  scale_y_reverse(breaks = seq(0, 50, by = 1)) +
  facet_wrap(~source, scales = "free")

# Check if klu is more stable than ked ------------------------------------

kd <- res %>% 
  # filter(depth <= 10) %>%
  filter(between(depth, 0, 10)) %>% 
  group_by(source, distance_class, lower, upper, middle) %>% 
  mutate(value = value / max(value)) %>% 
  nest() %>% 
  mutate(kd = map(data, ~minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), start = list(a0 = 1, k = 0.03), data = .)))

pred <- kd %>% 
  mutate(pred = map2(data, kd, modelr::add_predictions)) %>% 
  unnest(pred) %>% 
  mutate(id = interaction(source, distance_class))

# pred %>% 
#   ggplot(aes(x = value, y = depth, group = distance_class)) +
#   geom_point() +
#   geom_line(aes(x = pred, color = "predicted")) +
#   facet_wrap(~id, scales = "free") +
#   scale_y_reverse()

kd %>% 
  mutate(kd = map_dbl(kd, ~coef(.)[2])) %>% 
  ggplot(aes(x = middle, y = kd, color = source)) +
  geom_point() + 
  geom_line() +
  xlab("Distance from the center of the melt pond (m)")

