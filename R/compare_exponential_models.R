# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Compare two different models to predict the exponential decrease of light.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops.feather")

dat <- cops %>% 
  filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002" & wavelength == 665)

dat %>% 
  ggplot(aes(x = luz, y = depth)) +
  geom_point() +
  scale_y_reverse()

dat <- dat %>% 
  group_by(profile_filename) %>% 
  nest() %>% 
  mutate(mod1 = map(data, ~minpack.lm::nlsLM(luz ~ a0 * exp(-k * depth), data = ., start = list(a0 = 0, k = 0.02)))) %>% 
  mutate(mod2 = map(data, ~minpack.lm::nlsLM(luz ~ a0 * exp(-k * depth) + c, data = ., start = list(a0 = 0, k = 0.02, c = 0)))) %>% 
  mutate(pred_luz_mod1 = map2(data, mod1, modelr::add_predictions, var = "pred_luz_mod1")) %>% 
  mutate(pred_luz_mod2 = map2(data, mod2, modelr::add_predictions, var = "pred_luz_mod2")) %>% 
  mutate(res_luz_mod1 = map2(data, mod1, modelr::add_residuals, var = "res_luz_mod1")) %>% 
  mutate(res_luz_mod2 = map2(data, mod2, modelr::add_residuals, var = "res_luz_mod2")) %>% 
  unnest(pred_luz_mod1, pred_luz_mod2, res_luz_mod1, res_luz_mod2)

p1 <- dat %>% 
  ggplot(aes(x = luz, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  geom_path(aes(x = pred_luz_mod1), color = "red") +
  # scale_x_continuous(limits = c(0, 1e-04)) +
  annotate("text", x = 1e-5, y = 75, label = expression(y==a0%*%e^{-kd}))

p2 <- dat %>% 
  ggplot(aes(x = depth, y = res_luz_mod1)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  ylab("Residuals")

p3 <- dat %>% 
  ggplot(aes(x = luz, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  geom_path(aes(x = pred_luz_mod2), color = "red") +
  # scale_x_continuous(limits = c(0, 1e-04)) +
  annotate("text", x = 1e-5, y = 75, label = expression(y==a0%*%e^{-kd}+c))

p4 <- dat %>% 
  ggplot(aes(x = depth, y = res_luz_mod2)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  ylab("Residuals")

p <- cowplot::plot_grid(p1, p2, p3, p4, align = "hv", labels = "AUTO")
ggsave("graphs/compare_exponential_models.pdf", device = cairo_pdf)


