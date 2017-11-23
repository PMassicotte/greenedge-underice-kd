# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# This script calculate Ked, Keu and Klu from COPS profiles.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(minpack.lm)

cops <- read_feather("data/clean/cops.feather") %>% 
  select(-longitude_dec_degrees, -latitude_dec_degrees, -sun_zenith_angle_degrees, -hole_type) %>% 
  filter(lubridate::year(posixct_date_utc) == 2016) %>% 
  select(-euz)

cops <- cops %>% 
  filter(wavelength >= 395 & wavelength <= 710) %>% 
  group_by(profile_filename) %>% 
  filter(max(depth) >= 80) %>% ## Keep profils that go to at least 80 m
  ungroup() %>% 
  filter(between(depth, 10, 80))

cops <- cops %>% 
  gather(type, value, edz, luz) %>% 
  drop_na(value) %>% 
  group_by(profile_filename, type, wavelength) %>% 
  nest()

## Make sure all profiles kept are between 10 and 80 meters
stopifnot(all(map_dbl(cops$data, ~min(.$depth)) == 10))
stopifnot(all(map_dbl(cops$data, ~max(.$depth)) == 80))

# Do the models

# cops <- cops %>% 
#   filter(wavelength == 395 & type == "edz")
  # filter(profile_filename == "GE2016.ICMP_ICEP_160504_CAST_002" & wavelength == 395 & type == "edz")

## Create 2 columns that will serve to vary the depth at which k is calculated

dat <- cops %>%
  unnest() %>%
  expand(
    nesting(
      start_depth = c(1, seq(10, 75, by = 5)),
      end_depth = c(80, seq(10, 75, by = 5) + 5)),
    nesting(
      profile_filename,
      type,
      wavelength,
      depth,
      flag_quality,
      posixct_date_utc,
      value
    )
  ) %>%
  group_by(profile_filename, start_depth, end_depth, type, wavelength) %>%
  nest()

dat

fit <- function(df, start_depth, end_depth) {
  
  df2 <- df %>%
    filter(between(depth, start_depth, end_depth))
  
  mod <- minpack.lm::nlsLM(
    value ~ a0 * exp(-k * depth),
    data = df2,
    start = list(a0 = 0, k = 0.02),
    control = nls.lm.control(maxiter =  200)
  )
  
  return(mod)

}

# ~ 5 minutes
res <- dat %>% 
  mutate(mod = pmap(list(data, start_depth, end_depth), fit)) %>% 
  mutate(pred = map2(data, mod, modelr::add_predictions)) 

# p <- res %>%
#   unnest(pred) %>%
#   ggplot(aes(x = value, y = depth)) +
#   geom_point() +
#   geom_path(aes(x = pred, color = interaction(start_depth, end_depth))) +
#   scale_y_reverse(breaks = seq(10, 80, by = 10)) +
#   facet_wrap( ~ profile_filename, scales = "free") +
#   labs(color = "Depth range over\nwhich model is fitted") +
#   theme(strip.text.x = element_text(size = 6)) +
#   labs(title = "Effect of depth range on k",
#        subtitle = "Based on 83 COPS profiles (GE2016-IC) with Edz data at 395 nm.")
# 
# ggsave("graphs/effect_of_depth_on_k.pdf", device = cairo_pdf, width = 18, height = 14)

k <- res %>% 
  # filter(wavelength %in% c(395, 443)) %>% 
  mutate(k = map(mod, summary)) %>% 
  mutate(k = map(k, "coefficients")) %>% 
  mutate(k = map_dbl(k, 2)) %>% 
  unite(depth_range, start_depth, end_depth, sep = "-")

k <- k %>%
  mutate(depth_range2 = factor(depth_range, levels = unique(depth_range)))

# Plot ked vs klu ---------------------------------------------------------

## Boxplot
p <- k %>% 
  ggplot(aes(x = depth_range2, y = k, fill = type)) +
  geom_boxplot(position = position_dodge(1), outlier.size = 0.5, size = 0.25) +
  facet_wrap(~wavelength, ncol = 3) +
  labs(title = "Effect of depth range on k",
       subtitle = "Based on 83 COPS profiles (GE2016-IC).") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank()) +
  xlab("Depth range (m)") +
  ylab(bquote(K~(m^{-1})))

ggsave("graphs/effect_of_depth_on_k_boxplots.pdf", device = cairo_pdf, height = 14, width = 12)

## Scatter plot of Ked vs Klu

k2 <- k %>% 
  select(profile_filename, depth_range2, type, wavelength, k) %>% 
  spread(type, k) %>% 
  rename(ked = edz, klu = luz)

p1 <- k2 %>%
  filter(depth_range2 != "1-80") %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = depth_range2)) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  labs(title = "Ked vs Klu",
       subtitle = "Includes 34860 observations (no distinction between depth ranges over which K have been calculated and wavelengths).")

p2 <- p1 +
  facet_wrap(~wavelength) +
  labs(title = "Ked vs Klu",
       subtitle = "Includes 34860 observations by wavelengths (no distinction between depth ranges over which K have been calculated).")

p3 <- p1 + 
  facet_grid(depth_range2 ~ wavelength) +
  labs(title = "Ked vs Klu",
       subtitle = "Includes 34860 observations (divided by depth ranges and wavelengths).")

p4 <- p1 + 
  facet_wrap(~depth_range2) +
  labs(title = "Ked vs Klu",
       subtitle = "Includes 34860 observations (divided by depth ranges).")

## Save the graphs
ggsave("graphs/compare_ked_klu_a.pdf", p1, device = cairo_pdf)
ggsave("graphs/compare_ked_klu_b.pdf", p2, device = cairo_pdf)
ggsave("graphs/compare_ked_klu_c.pdf", p3, device = cairo_pdf, width = 20, height = 20)
ggsave("graphs/compare_ked_klu_d.pdf", p4, device = cairo_pdf)
