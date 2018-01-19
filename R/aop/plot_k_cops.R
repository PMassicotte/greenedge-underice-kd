rm(list = ls())

k <- read_feather("data/clean/k_cops.feather")

k <- k %>% 
  mutate(date = str_extract(profile_filename, "\\d{6}")) %>% 
  mutate(date = lubridate::parse_date_time(date, orders = "ymd")) %>% 
  filter(r2 >= 0.99)

k <- k %>% 
  group_by(date, depth_range, type, wavelength) %>% 
  summarise(k = mean(k))

k %>% 
  filter(type == "edz") %>% 
  ggplot(aes(x = date, y = k, color = factor(wavelength), group = wavelength)) +
  geom_line() +
  geom_point() +
  facet_wrap(~depth_range, scales = "free")

ggsave("graphs/cops/time_series_ked_cops.pdf", device = cairo_pdf, width = 15, height = 9)
