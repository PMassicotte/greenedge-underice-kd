rm(list = ls())

simulo_melt_pond <- read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather")

simulo_melt_pond <- simulo_melt_pond %>%
  mutate(source = ifelse(source == "radiance", "Lu", "Ed")) %>%
  filter(pixel_distance_to_center <= 50)

## Calculate mean ed and lu profiles for each iso distance
df <- simulo_melt_pond %>%
  mutate(iso_distance = cut_interval(pixel_distance_to_center, n = 50)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value))

p1 <- df %>%
  ggplot(aes(x = value, y = depth, group = mid_distance)) +
  geom_path() +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(title = "Averaged Ed and Lu profiles (50 distance classes)")

ggsave("graphs/simulo/001_simulo_averaged_ed_lu.pdf", device = cairo_pdf)

## Normalize Ed and Lu
res <- df %>%
  group_by(source, mid_distance) %>%
  # mutate(value = (value - mean(value)) / sd(value))
  mutate(value = value / max(value))

p2 <- res %>%
  ggplot(aes(x = value, y = depth, group = mid_distance)) +
  geom_path() +
  facet_wrap(~ source, scales = "free") +
  scale_y_reverse() +
  labs(title = str_wrap("Averaged Ed and Lu profiles (50 distance classes). Profils are noramlized by their maximum value."))

ggsave("graphs/simulo/002_simulo_averaged_normalized_ed_lu_1.pdf", device = cairo_pdf)

p3 <- res %>%
  ggplot(aes(x = value, y = depth, color = source)) +
  geom_path() +
  facet_wrap(~ mid_distance, scales = "free") +
  scale_y_reverse() +
  labs(title = "Normalized Ed and Lu (numbers in the boxes are distances to the melt pond).")

ggsave("graphs/simulo/003_simulo_averaged_normalized_ed_lu_2.pdf", device = cairo_pdf, width = 16, height = 12)

# K over distance ---------------------------------------------------------

## Is KLu stable as a function of the distance from the center hole?

k <- res %>% 
  # filter(depth >= 10) %>% 
  group_by(source, mid_distance) %>% 
  nest() %>% 
  mutate(mod = map(data, ~nls(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 1, k = 0.02)))) %>% 
  mutate(k = map_dbl(mod, ~coef(.)[2]))

k %>% 
  ggplot(aes(x = mid_distance, y = k, color = source)) +
  geom_point() +
  geom_line() +
  labs(title = "K as a function of the distance to the melt pond")

ggsave("graphs/simulo/004_simulo_k_ed_lu_distance.pdf", device = cairo_pdf)


# k %>% 
#   select(-mod, -data) %>% 
#   spread(source, k) %>% 
#   ggplot(aes(x = Ed, y = Lu, color = mid_distance)) +
#   geom_point()
