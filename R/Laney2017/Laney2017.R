# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Experimentation with the model proposed by Laney2017. Laney, S. R.,
# Krishfield, R. A., & Toole, J. M. (2017). The euphotic zone under Arctic Ocean
# sea ice: Vertical extents and seasonal trends. Limnology and Oceanography,
# 62(5), 1910–1934. https://doi.org/10.1002/lno.10543
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ipar <- function(params, z) {
  ipar <- with(params, i0 * exp(-kd * z) - (i0 - ins) * exp(-kns * z))
  return(data.frame(z, ipar))
}

par <- function(params, z) {
  par <- with(params, i0 * exp(-kd * z))
  return(data.frame(z, par))
}

n_simul <- 10000
par_max <- 10

df <- tibble(
  sim = 1:n_simul,
  i0 = runif(n_simul, 0, 500),
  kd = runif(n_simul, 0.001, 1),
  kns = runif(n_simul, 0, 1),
  ins = runif(n_simul, 0.01, par_max)
)

df

z <- seq(0, 100, by = 1)

res <- df %>%
  group_by(sim) %>%
  nest() %>%
  mutate(ipar = map(data, ipar, z = z)) %>%
  mutate(par = map(data, par, z = z)) %>% 
  unnest(ipar, par)
# 
# res %>%
#   ggplot(aes(y = z)) +
#   geom_path(aes(ipar, color = "ipar")) +
#   geom_path(aes(par, color = "par")) +
#   scale_y_reverse() +
#   facet_wrap(~sim, scales = "free")

res %>% 
  ggplot(aes(x = ipar, y = z)) +
  geom_path(aes(group = sim), size = 0.25, alpha = 0.25) +
  scale_y_reverse() +
  geom_path(data = group_by(res, z) %>% summarise(ipar = mean(ipar)), color = "red")

# library(data.table)
# df2 <- data.table(df)
# 
# df2[, ipar(.SD, z = z),  by = .(sim)]
