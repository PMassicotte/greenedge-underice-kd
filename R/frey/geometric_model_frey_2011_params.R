rm(list = ls())

z <- seq(0, 50, by = 0.25)

p <- 0.3
r <- 5
n <- 15
i <- 0.3165 
k <- 0.0444

f <- function(df) {
  
  y <- pi * df$i * (1 + df$p * (df$n - 1) * cos(atan(df$r / z))^2) * exp(-df$k * z)
  
  return(data.frame(z, y))
  
}

params <- crossing(p, r, n, i, k)


res <- purrrlyr::by_row(params, f, .to = "dat") 

dat <- res %>% 
  unnest(dat)

pp <- group_by(dat, p, r, n, i, k) %>% 
  filter(y == max(y))

pp

dat %>% 
  ggplot(aes(x = y, y = z, group = interaction(p, r, n, i, k))) +
  geom_path() +
  scale_y_reverse() +
  geom_vline(xintercept = 1, lty = 2) +
  xlab("Relative transmittance (%)") +
  ylab("Depth (m)") +
  labs(
    title = "Relative transmittance as a function of depth",
    subtitle = str_wrap("At 0 meter, the relative transmittance is always 1. Irradiance just bellow the bare ice sheet can be multiplyed by the relative transmittance profil."),
    caption = str_wrap("Data from the geometric model proposed by Frey, K. E., Perovich, D. K., & Light, B. (2011). The spatial distribution of solar radiation under a melting Arctic sea ice cover. Geophysical Research Letters, 38(22), n/a-n/a. https://doi.org/10.1029/2011GL049421", 100)
  )

ggsave("graphs/geometric_model.pdf")
embed_fonts("graphs/geometric_model.pdf")
