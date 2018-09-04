rm(list = ls())

simulo1 <- read_feather("data/clean/simulo/compute-canada/simulo_4_lambertian_sources.feather") %>% 
  mutate(simul = "5m-mp") %>% 
  filter(between(pixel_distance_to_center, 0, 50)) 

simulo2 <- read_feather("data/clean/simulo/compute-canada/simulations-with-15m-melt-pond-4-lambertian-sources.feather") %>% 
  mutate(simul = "15m-mp") %>% 
  filter(between(pixel_distance_to_center, 10, 60)) %>% 
  mutate(pixel_distance_to_center = pixel_distance_to_center - 10)

simulo <- bind_rows(simulo1, simulo2)


simulo <- simulo %>% mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
    separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
    mutate_at(vars(start_distance, end_distance), parse_number) %>%
    mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
    group_by(depth, source, simul, mid_distance) %>%
    summarise(value = mean(value)) %>%
    ungroup()

simulo %>% 
  filter(depth == 0.5) %>% 
  ggplot(aes(x = mid_distance, y = value)) +
  geom_line(aes(color = simul)) +
  facet_wrap(~source, scales = "free")

simulo %>% 
  filter(depth == 0.5) %>% 
  spread(source, value) %>% 
  mutate(dif = intensity / radiance) %>% 
  ggplot(aes(x = mid_distance, y = dif)) +
  geom_line(aes(color = simul))


simulo %>% 
  filter(depth == 0.5) %>% 
  spread(source, value) %>% 
  mutate(dif = intensity / radiance) %>% 
  spread(simul, dif)
