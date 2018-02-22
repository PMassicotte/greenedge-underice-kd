
## ----------------------------------------------------------------

rm(list = ls())

k_cops <- read_feather("data/clean/k_cops.feather") %>% 
  rename(k_cops = k) %>% 
  filter(type == "edz") %>% 
  filter(r2 >= 0.99) %>% 
  mutate(date = lubridate::parse_date_time(str_extract(profile_filename, "\\d{6}"), orders = "ymd")) %>% 
  mutate(date = as.Date(date)) %>% 
  select(start_depth, end_depth, wavelength, k_cops, date)


iop <- read_feather("data/clean/iop_interpolated.feather")
mud <- read_feather("data/clean/mud_trios.feather")

k_iop <- inner_join(iop, mud) %>% 
  mutate(k = (mean_a_interpol + mean_bbp_interpol) / mean_mud) %>% 
  select(date:wavelength, k) %>% 
  rename(k_iop = k)

k <- inner_join(k_cops, k_iop) %>% 
  unite(depth_range, start_depth, end_depth, remove = FALSE, sep = "-")

## Plot

p1 <- k %>% 
  ggplot(aes(x = k_iop, y = k_cops)) +
  geom_point() +
  labs("Global relation between Ked from IOP and COPS")

p2 <- k %>% 
  filter(wavelength <= 500) %>% 
  ggplot(aes(x = k_iop, y = k_cops)) +
  geom_point(aes(color = depth_range)) +
  geom_smooth(method = "lm")  + 
  labs("Global relation between Ked from IOP and COPS but under 500 nm")

p3 <- k %>% 
  filter(wavelength <= 500) %>% 
  ggplot(aes(x = k_iop, y = k_cops)) +
  geom_point(aes(color = factor(wavelength))) +
  geom_smooth(method = "lm")  + 
  labs("Global relation between Ked from IOP and COPS but under 500 nm")

p4 <- k %>% 
  filter(wavelength %in% seq(400, 700, by = 100)) %>%
  ggplot(aes(x = k_cops, y = k_iop)) +
  geom_point(aes(color = factor(date))) +
  facet_wrap(~depth_range + wavelength, scales = "free", ncol = 7) +
  geom_smooth(method = "lm")  

pdf("graphs/ked_iop_vs_cops.pdf", width = 16, height = 12)
p1
p2
p3
p4
dev.off()

# ggsave("graphs/ked_iop_vs_cops.pdf", device = cairo_pdf, width = 24, height = 12)


