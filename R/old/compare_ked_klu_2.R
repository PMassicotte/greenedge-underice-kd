# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# This script calculate Ked, Keu and Klu from COPS profiles.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

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

## Compute R2 for each model
get_r2 <- function(pred, start_depth, end_depth) {
  
  df2 <- pred %>%
    filter(between(depth, start_depth, end_depth))
  
  return(cor(df2$value, df2$pred)^2)
  
}

# ~ 5 minutes
res <- dat %>% 
  mutate(mod = pmap(list(data, start_depth, end_depth), fit)) %>% 
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>% 
  mutate(r2 = pmap(list(pred, start_depth, end_depth), get_r2)) %>% 
  unnest(r2)

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

k %>% 
  select(-(data:pred)) %>% 
  write_feather("data/clean/k.feather")

# Plot ked vs klu ---------------------------------------------------------

k <- read_feather("data/clean/k.feather")

r2_threshold <- 0.99

# ## Boxplot
# p <- k %>% 
#   filter(r2 >= r2_threshold & k > 0) %>% 
#   ggplot(aes(x = depth_range2, y = k, fill = type)) +
#   geom_boxplot(position = position_dodge(1), outlier.size = 0.5, size = 0.25) +
#   facet_wrap(~wavelength, ncol = 3) +
#   labs(title = "Effect of depth range on k",
#        subtitle = "Based on 83 COPS profiles (GE2016-IC).") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(legend.title = element_blank()) +
#   xlab("Depth range (m)") +
#   ylab(bquote(K~(m^{-1})))
# 
# ggsave("graphs/effect_of_depth_on_k_boxplots.pdf", device = cairo_pdf, height = 14, width = 12)

## Scatter plot of Ked vs Klu

k2 <- k %>%
  filter(r2 >= r2_threshold & k > 0) %>% 
  select(profile_filename, depth_range2, type, wavelength, k) %>% 
  spread(type, k) %>% 
  rename(ked = edz, klu = luz) %>% 
  drop_na(ked, klu)

k2 <- k2 %>%
  filter(depth_range2 != "1-80")

p1 <- k2 %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = depth_range2), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Depth (m)") +
  labs(title = bquote(K[Ed]~vs~K[Lu]),
       subtitle = str_wrap(sprintf("Includes %d observations (no distinction between depth ranges over which K have been calculated and wavelengths).", nrow(k2)))) 

p2 <- p1 +
  facet_wrap(~wavelength) +
  labs(title = "Ked vs Klu",
       subtitle = str_wrap(sprintf("Includes %d observations by wavelengths (no distinction between depth ranges over which K have been calculated).", nrow(k2))))

p3 <- p1 + 
  facet_grid(depth_range2 ~ wavelength) +
  labs(title = "Ked vs Klu",
       subtitle =  str_wrap(sprintf("Includes %d observations (divided by depth ranges and wavelengths).", nrow(k2))))

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

color <- lapply(unique(k2$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(k2$wavelength))

p4 <- k2 %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = factor(wavelength)), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Depth (m)") +
  scale_color_manual(values = color) +
  facet_wrap(~depth_range2, scales = "free", ncol = 3) +
  labs(title = "Ked vs Klu",
       subtitle =  str_wrap(sprintf("Includes %d observations (divided by depth ranges).", nrow(k2))))

p5 <- k2 %>% 
  filter(wavelength < 589) %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = factor(wavelength)), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Depth (m)") +
  scale_color_manual(values = color) +
  facet_wrap(~depth_range2, scales = "free", ncol = 3) +
  labs(title = "Ked vs Klu",
       subtitle =  str_wrap(sprintf("Includes %d observations (divided by depth ranges).", nrow(k2))))

## Save the graphs
ggsave("graphs/compare_ked_klu_a.pdf", p1, device = cairo_pdf, height = 6, width = 8)
ggsave("graphs/compare_ked_klu_b.pdf", p2, device = cairo_pdf, height = 6, width = 8)
ggsave("graphs/compare_ked_klu_c.pdf", p3, device = cairo_pdf, width = 20, height = 20)
ggsave("graphs/compare_ked_klu_d.pdf", p4, device = cairo_pdf, height = 8, width = 9)
ggsave("graphs/compare_ked_klu_e.pdf", p5, device = cairo_pdf, height = 8, width = 9)

## Save the graphs
ggsave("graphs/compare_ked_klu_a.svg", p1, height = 6, width = 8)
ggsave("graphs/compare_ked_klu_b.svg", p2, height = 6, width = 8)
ggsave("graphs/compare_ked_klu_c.svg", p3, width = 20, height = 20)
ggsave("graphs/compare_ked_klu_d.svg", p4, height = 8, width = 9)
ggsave("graphs/compare_ked_klu_e.svg", p5, height = 8, width = 9)

# Exploration -------------------------------------------------------------

## Try to understand why relation between ked and klu is not great at 694 for the 10-15m layer

k2 %>% 
  filter(wavelength == 694 & depth_range2 == "10-15") %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = profile_filename))


p1 <- k %>% 
  filter(wavelength == 694 & depth_range2 == "10-15" & r2 >= r2_threshold) %>% 
  unnest(pred) %>% 
  ggplot(aes(x = value, y = depth, group = profile_filename)) +
  geom_path() +
  facet_wrap(~type, scales = "free") +
  scale_y_reverse() +
  labs(title = "At 694 nm (10-15m)")

p2 <- k %>% 
  filter(wavelength == 694 & depth_range2 == "10-15" & r2 >= r2_threshold) %>% 
  unnest(pred) %>% 
  filter(between(depth, 10, 15)) %>% 
  ggplot(aes(x = value, y = depth, group = profile_filename)) +
  geom_point() +
  geom_path(aes(x = pred), color = "red") +
  facet_wrap(~type, scales = "free") +
  scale_y_reverse() +
  labs(title = "At 694 nm (10-15m)")

cowplot::plot_grid(p1, p2, ncol = 1, align = "hv")


# Extra -------------------------------------------------------------------

k %>% 
  filter(r2 >= 0.95 & k >= 0) %>% 
  filter(depth_range != "1-80") %>% 
  ggplot(aes(x = wavelength, y = k, group = profile_filename)) +
  # geom_point() +
  geom_path(color = "gray50", alpha = 0.50) +
  facet_grid(type~depth_range, scales = "free")

## Possible to have good R2 but with a negative relation...
data <- k %>% 
  filter(r2 >= 0.90 & k < 0) %>% 
  slice(10)

data

data %>% 
  unnest(pred) %>%
  filter(between(depth, 50, 55)) %>% 
  ggplot(aes(x = value, y = depth)) +
  geom_point() +
  geom_line(aes(x = pred)) +
  scale_y_reverse() +
  facet_wrap(~type, scales = "free")
