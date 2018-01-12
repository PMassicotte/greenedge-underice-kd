rm(list = ls())

# Plot ked vs klu ---------------------------------------------------------

k <- read_feather("data/clean/k.feather")

r2_threshold <- 0.99

k2 <- k %>%
  filter(r2 >= r2_threshold & k > 0) %>% 
  dplyr::select(profile_filename, depth_range2, type, wavelength, k) %>% 
  spread(type, k) %>% 
  rename(ked = edz, klu = luz) %>% 
  drop_na(ked, klu)

k2 <- k2 %>%
  filter(depth_range2 != "1-80")

formula <- y ~ x

p <- k2 %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = depth_range2), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Depth (m)") +
  guides(color = guide_legend(
    keywidth = 0.2,
    keyheight = 0.2,
    default.unit = "inch",
    ncol = 1
  )) +
  stat_poly_eq(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = formula,
    parse = TRUE,
    label.x.npc = "right",
    label.y.npc = "bottom",
    vjust = 0.25
  )

ggsave("graphs/supp_fig_2.pdf", device = cairo_pdf, width = 6, height = 4)
