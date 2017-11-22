rm(list = ls())

a <- seq(0, pi, length.out = 10)

f <- function(a) {
  
  z <- seq(0, 100, by = 0.25)
  
  i <- 0.01
  k <- 0.075
  n <- 12
  p <- 0.15
  r <- 2.5
  
  y <- a * i * (1 + p * (n - 1) * cos(atan(r / z)) ^ 2) * exp(-k * z)
  
  return(data.frame(a, z, y))
  
}

res <- map(a, f) %>% 
  bind_rows() %>% 
  as_tibble()

res %>% 
  ggplot(aes(x = y, y = z, color = factor(a))) +
  geom_path() +
  scale_y_reverse() +
  annotate("text", x = 0.03, y = 75, label = "F(z) == pi*I*(1+P(N-1)~cos^2*(theta))*e^{-kz}", parse = TRUE) +
  annotate("text", x = 0.03, y = 80, label = "F(z) == a*I*(1+P(N-1)~cos^2*(theta))*e^{-kz}", parse = TRUE) +
  labs(
    title = "Effect of isotropy on light profiles",
    subtitle = "Frey's model assumed a completly isotropic light field (pi * I). Here I allowed the value to change between 0 and pi.",
    color = "Values of 'a'"
  )

ggsave("graphs/test_isotropy.pdf")
embed_fonts("graphs/test_isotropy.pdf")
