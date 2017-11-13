rm(list = ls())

cops <- read_csv("data/clean/cops.csv")


df <- cops %>% 
  filter(posixct_date_utc %in% c(as.POSIXct("2015-05-06 15:32:05", tz = "UTC"))) %>% 
  drop_na(par_d_p24h_ein_m_2_day_1)

ff <- function(par, data) {
  
  y <- data$par_d_p24h_ein_m_2_day_1
  x <- data$depth
  
  p <- par[1]
  r <- par[2]
  n <- par[3]
  i <- par[4]
  k <- par[5]
  
  yy <- pi * i * (1 + p * (n - 1) * cos(atan(r / x)) ^ 2) * exp(-k * x)
  
  rss <- sum((y - yy)^2)
  
  return(rss)
  
}

calculate_light <- function(par, x) {
  
  p <- par[1]
  r <- par[2]
  n <- par[3]
  i <- par[4]
  k <- par[5]
  
  yy <- pi * i * (1 + p * (n - 1) * cos(atan(r / x)) ^ 2) * exp(-k * x)
  
  return(yy)
  
}

res <- optim(par = c(
  p = 0.1,
  r = 4,
  n = 11,
  i = 0.008,
  k = 0.06),
  ff,
  data = df)

minpack.lm::nls.lm(par = c(
  p = 0.1,
  r = 4,
  n = 11,
  i = 0.008,
  k = 0.06),
  fn = ff,
  data = df)

yy <- calculate_light(res$par, df$depth)

df <- df %>% 
  mutate(pred = yy)

df %>% 
  ggplot(aes(x = par_d_p24h_ein_m_2_day_1, y = depth)) +
  geom_point() +
  scale_y_reverse() +
  geom_path(aes(x = pred), color = "red")
  
ggsave("~/Desktop/cops.pdf")
