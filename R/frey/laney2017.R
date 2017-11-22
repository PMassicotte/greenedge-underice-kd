# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Explore the model proposed by Laney, S. R., Krishfield, R. A., & Toole, J. M.
# (2017). The euphotic zone under Arctic Ocean sea ice: Vertical extents and
# seasonal trends. Limnology and Oceanography, 62(5), 1910–1934.
# https://doi.org/10.1002/lno.10543
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_csv("data/clean/cops.csv")

z <- seq(0, 100, by = 0.25)

i0 <- 250
kd <- 0.02
kns <- 0.03
ins <- 0.05

i <- i0 * exp(-kd * z) - (i0 - ins) * exp(-kns * z)

df <- data_frame(z, i)

df %>% 
  ggplot(aes(x = i, y = z)) +
  geom_path() +
  scale_y_reverse()

dat <- cops %>% 
  filter(profile_filename == "GE2015.ICMP_ICEP_150504_CAST_005") %>% 
  drop_na(par_d_p24h_ein_m_2_day_1)

dat %>% 
  ggplot(aes(x = par_d_p24h_ein_m_2_day_1, y = depth)) +
  geom_path() +
  scale_y_reverse()

mod <- minpack.lm::nlsLM(par_d_p24h_ein_m_2_day_1 ~ i0 * exp(-kd * depth) - (i0 - ins) * exp(-kns * depth),
           data = dat,
           start = c(i0 = 0.05, kd = 0.02, kns = 0.0, ins = 0.05),
           lower = c(i0 = 0, kd = 0.001, kns = 0.1, ins = 0.01),
           upper = c(i0 = 500, kd = 1, kns = 1, ins = 1))

# ins is the value right bellow ice/snow
predict(mod, newdata = list(depth = 0))

dat <- dat %>%
  modelr::add_predictions(mod)

dat %>% 
  ggplot(aes(x = par_d_p24h_ein_m_2_day_1, y = depth)) +
  geom_path() +
  scale_y_reverse() +
  geom_path(aes(x = pred), col = "red")


optim(par = c(
  p = 0.1,
  r = 4,
  n = 11,
  i = 0.008,
  k = 0.06),
  ff,
  data = dat)


# Compare kd --------------------------------------------------------------

frey2001 <- read_csv("data/clean/simulations_frey.csv")

fit_laney <- function(df) {
 
  minpack.lm::nlsLM(
    par24h ~ i0 * exp(-kd * depth) - (i0 - ins) * exp(-kns * depth),
    data = df,
    start = c(
      i0 = 0.05,
      kd = 0.02,
      kns = 0.05,
      ins = 0.05
    ),
    lower = c(
      i0 = 0,
      kd = 0.001,
      kns = 0,
      ins = 0.01
    ),
    upper = c(
      i0 = 500,
      kd = 1,
      kns = 1,
      ins = 1
    )
  )
   
}

mod <- frey2001 %>% 
  # filter(simulation_id == 1) %>% 
  group_by(simulation_id, k) %>% 
  nest() %>% 
  mutate(mod = map(data, fit_laney)) 

res <- mod %>% 
  mutate(param = map(mod, broom::tidy)) %>% 
  unnest(param) %>% 
  filter(term == "kd")

res %>% 
  ggplot(aes(x = k, y = estimate, group = simulation_id)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

tmp <- simulation[755, ] %>% 
  unnest(profils)

tmp <- modelr::add_predictions(tmp, mod$mod[[755]])

tmp %>% 
  ggplot(aes(x = par24h, y = depth)) +
  geom_path() +
  scale_y_reverse() +
  geom_path(aes(x = pred), color = "red")
