# Frey, K. E., Perovich, D. K., & Light, B. (2011). The spatial distribution of
# solar radiation under a melting Arctic sea ice cover. Geophysical Research
# Letters, 38(22), n/a-n/a. https://doi.org/10.1029/2011GL049421

rm(list = ls())

cops <- read_csv("data/clean/cops.csv")


# Simple model ------------------------------------------------------------

z <- seq(0, 50, by = 0.25)

p <- 2
r <- 3
n <- 1.7 # Values for N at each wavelength were determined from the transmittances in Figure 2
# N serait le ratio entre MP et BI (radiance)
i <- 0.008 # "i" at each wavelength were determined from transmittances directly below the ice as shown in Figure 3b
k <- 0.06


f <- pi * i * (1 + p * (n - 1) * cos(atan(r / z))^2) * exp(-k * z)

dat <- data_frame(z, f) %>% 
  mutate(f_open = k * exp(-k * z))

dat %>% 
  ggplot(aes(x = f, y = z)) +
  geom_path() +
  scale_y_reverse() +
  geom_line(aes(x = f_open), color = "red")


# test --------------------------------------------------------------------

sin(pi)^2 + cos(pi)^2

df <- cops %>% 
  filter(posixct_date_utc %in% c(as.POSIXct("2015-05-06 15:32:05", tz = "UTC"))) %>% 
  drop_na(par_d_p24h_ein_m_2_day_1)

df %>% 
  ggplot(aes(x = par_d_p24h_ein_m_2_day_1, y = depth)) +
  geom_path() +
  scale_y_reverse()

mod <-
  minpack.lm::nlsLM(
    par_d_p24h_ein_m_2_day_1 ~ pi * i * (1 + p * (n - 1) * cos(atan(r / depth)) ^ 2) * exp(-k * depth),
    data = df,
    start =  c(
      p = 0.1,
      r = 4,
      n = 11,
      i = 0.008,
      k = 0.06
    )
  )

plot(df$depth, predict(mod))

write_csv(df, "~/Desktop/dat.csv")

