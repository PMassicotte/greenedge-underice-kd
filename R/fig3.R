# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure showing the average R2, slope and intercept of the regressions between
# Edz and Luz.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>%
  filter(between(wavelength, 400, 700)) %>%
  filter(depth >= 10) %>%
  group_by(profile_filename, wavelength) %>%
  mutate_at(.vars = vars(edz, luz), funs(. / .[depth == 10])) %>%
  drop_na(depth, edz, luz)

res <- df %>%
  group_by(profile_filename, wavelength) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(edz ~ luz, data = .))) %>%
  mutate(glance = map(mod, broom::glance)) %>%
  mutate(tidy = map(mod, broom::tidy))

# res$data[[1156]] %>%
#   ggplot(aes(x = edz, y = luz)) +
#   geom_point()

res2 <- res %>%
  unnest(glance) %>%
  unnest(tidy)

res2 <- res2 %>%
  dplyr::select(profile_filename:r.squared, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names() %>%
  group_by(wavelength) %>%
  summarise_if(is.numeric, .funs = funs(mean, sd))


# Descriptive stats -------------------------------------------------------

range(res2$r_squared_mean)
range(res2$luz_mean)
range(res2$x_intercept_mean)

# Plots -------------------------------------------------------------------

p1 <- res2 %>%
  ggplot(aes(x = wavelength, y = r_squared_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = r_squared_mean - r_squared_sd, ymax = r_squared_mean + r_squared_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop("Determination", "coefficient" ~ "("*R ^ 2*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

p2 <- res2 %>%
  ggplot(aes(x = wavelength, y = luz_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = luz_mean - luz_sd, ymax = luz_mean + luz_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop(Slope, "("*mu*W%*%cm^{-2}%*%nm^{-1}*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

p3 <- res2 %>%
  ggplot(aes(x = wavelength, y = x_intercept_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = x_intercept_mean - x_intercept_sd, ymax = x_intercept_mean + x_intercept_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop(Intercept, "("*mu*W%*%cm^{-2}%*%nm^{-1}*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

p <- cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO", align = "hv")
ggsave("graphs/fig3.pdf", p, width = 4, height = 8, device = cairo_pdf)

