rm(list = ls())

cops <- read_feather("data/clean/cops.feather")

cops <- cops %>% 
  filter(profile_filename == "GE2015.ICMP_ICEP_150706_CAST_005")
# 
#   filter(posixct_date_utc == lubridate::parse_date_time("2015−04−09 18:42:43", orders = "Ymd HMS", tz = "UTC"))

cops <- cops %>% 
  select(depth:hole_type, matches("edz.+\\d{3}|euz.+\\d{3}")) %>% 
  gather(temp, irradiance, matches("edz.+\\d{3}|euz.+\\d{3}")) %>% 
  extract(temp, into = c("sensor", "wavelength"), regex = "(edz|euz).+(\\d{3})", convert = TRUE) %>% 
  spread(sensor, irradiance) %>% 
  arrange(wavelength)

p1 <- cops %>% 
  filter(wavelength == 443) %>% 
  ggplot(aes(x = edz, y = depth)) +
  geom_point(aes(color = depth)) +
  viridis::scale_color_viridis() +
  facet_wrap(~wavelength, scales = "free") +
  scale_y_reverse() +
  scale_x_log10()

p2 <- cops %>% 
  filter(wavelength == 443) %>% 
  ggplot(aes(x = euz, y = depth)) +
  geom_point(aes(color = depth)) +
  viridis::scale_color_viridis() +
  facet_wrap(~wavelength, scales = "free") +
  scale_y_reverse() +
  scale_x_log10()

cops <- cops %>% 
  filter(wavelength == 443)

res <- cops %>% 
  group_by(profile_filename, wavelength) %>% 
  nest() %>% 
  mutate(k_z = map(data, function(df) {
    
    kd_z <- -xts::diff.xts(log(df$edz), na.pad = TRUE) / xts::diff.xts(df$depth, na.pad = TRUE)
    ku_z <- -xts::diff.xts(log(df$euz), na.pad = TRUE) / xts::diff.xts(df$depth, na.pad = TRUE)
    return(data.frame(kd_z, ku_z))
    
  })) %>% 
  unnest(data, k_z)

p3 <- res %>% 
  filter(kd_z > 0 & ku_z > 0) %>% 
  drop_na(kd_z, ku_z) %>% 
  ggplot(aes(x = kd_z, y = ku_z, group = profile_filename)) +
  geom_point(aes(color = depth)) +
  viridis::scale_color_viridis() +
  geom_abline(slope = 1, intercept = 0, lty = 2)

cowplot::plot_grid(p1, p2, p3, labels = "AUTO")


res %>% 
  ggplot(aes(x = ku_z, y = depth)) +
  geom_point() +
  geom_point(aes(x = kd_z), color = "red") +
  scale_y_reverse()

