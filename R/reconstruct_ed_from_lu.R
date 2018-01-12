rm(list = ls())

cops <- read_rds("data/clean/cops_ked_keu_klu_models.rds")


# test --------------------------------------------------------------------

df <- cops %>% 
  filter(wavelength == 443 & profile_filename == "GE2016.ICMP_ICEP_160615_CAST_005") %>% 
  unnest(pred)
  
df

df %>% 
  filter(type == "edz") %>% 
  ggplot(aes(x = value, y = depth, color = "Raw data <= 10m")) +
  geom_point() +
  scale_y_reverse()

dat_mod <- cops %>% 
  filter(str_detect(profile_filename, "2016")) %>% 
  filter(wavelength == 443) %>% 
  select(profile_filename, type, k) %>%
  distinct() %>% 
  spread(type, k)

mod <- lm(edz ~ luz, data = dat_mod)
summary(mod)

res <- df %>% 
  select(profile_filename, type, wavelength, k) %>% 
  distinct() %>% 
  filter(type == "luz") %>% 
  mutate(pred_ked = predict(mod, newdata = list(luz = k)))

tmp <- read_feather("data/clean/cops.feather") %>% 
  filter(wavelength == 443 & profile_filename == "GE2016.ICMP_ICEP_160615_CAST_005") %>% 
  mutate(pred_ked = res$pred_ked) %>% 
  mutate(a0 = edz[depth == 80] / exp(-pred_ked * 80)) %>% ## Pred ou value????? 
  mutate(pred_edz_from_klu = a0 * exp(-pred_ked * depth))

write_csv(tmp, "presentation/data_reconstruction_edz_from_klu.csv")

last_plot() +
  geom_path(data = tmp, aes(x = edz, y = depth, color = "All raw edz data")) +
  geom_path(data = tmp, aes(x = pred_edz_from_klu, y = depth, color = "Reconstructed from Klu")) +
  labs(title = "Reconstruction of edz") +
  theme(legend.title = element_blank())

ggsave("graphs/reconstruction.pdf")
embed_fonts("graphs/reconstruction.pdf")
