# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Time series of kPAR (Ed and Lu) at the ice camp.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops.feather") %>% 
  filter(lubridate::year(posixct_date_utc) == 2016) %>% 
  drop_na(edz, luz) %>% 
  select(-euz) %>% 
  filter(depth <= 10) %>% 
  filter(between(wavelength, 400, 700))

cops <- cops %>% 
  gather(light_type, value, edz, luz)

cops %>% 
  filter(wavelength == 412) %>% 
  ggplot(aes(x = value, y = depth, group = profile_filename)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~light_type, scales = "free")

kd <- cops %>% 
  group_by(profile_filename, wavelength, posixct_date_utc, light_type) %>% 
  nest() %>% 
  mutate(mod = map(data, ~minpack.lm::nlsLM(value ~ a0 * exp(-k * depth), data = ., start = list(a0 = 0, k = 0.02)))) %>% 
  mutate(k = map_dbl(mod, ~coef(.)[2]))

kd %>% 
  group_by(wavelength, date = lubridate::as_date(posixct_date_utc), light_type) %>% 
  summarise(k = mean(k)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = k, color = light_type)) +
  geom_point() +
  geom_path() +
  facet_wrap(~wavelength, scales = "free")
