# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Format IOPs for further use.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

iop <- read_feather("data/raw/iop/IOP.2016.AVG.MEAN.N.d.u.feather") %>% 
  janitor::clean_names(case = "old_janitor") %>% 
  filter(up_down == "d") %>% 
  mutate(date = as.Date(lubridate::parse_date_time(date, order = "%Y%j")))

iop <- iop %>%
  select(
    date,
    depth_grid,
    contains("mean_a_"),
    contains("mean_bbp_"),
    contains("mean_c_"),
    n = n_a_440
  )

# iop <- iop %>%
#   # select(
#   #   date,
#   #   depth_grid,
#   #   contains("mean_a_520"),
#   #   contains("mean_bbp_510"),
#   #   contains("mean_c_520"),
#   #   n
#   # ) %>%
#   filter(depth_grid <= 5) %>%
#   filter(date == "2016-05-04 00:00:00")

res <- iop %>%
  gather(type,
         value,
         contains("mean_a_"),
         contains("mean_bbp_"),
         contains("mean_c_")) %>% 
  extract(type, into = c("type", "wavelength"), regex = "(mean\\S+)_(\\d{3})", convert = TRUE) %>% 
  spread(type, value) %>% 
  arrange(wavelength)

# Save --------------------------------------------------------------------

write_feather(res, "data/clean/iop.feather")

# Plots -------------------------------------------------------------------

color <- lapply(unique(res$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(res$wavelength))

## Absorption
p1 <- res %>% 
  drop_na(mean_a) %>% 
  ggplot(aes(x = mean_a, y = depth_grid, color = factor(wavelength))) +
  geom_path() +
  facet_wrap(~date) +
  scale_y_reverse() +
  scale_color_manual(values = color) 

ggsave("graphs/iop/iop_a.pdf", width = 15, height = 10, device = cairo_pdf)

## Attenuation
p2 <- res %>% 
  drop_na(mean_c) %>% 
  ggplot(aes(x = mean_c, y = depth_grid, color = factor(wavelength))) +
  geom_path() +
  facet_wrap(~date) +
  scale_y_reverse() +
  scale_color_manual(values = color) 

ggsave("graphs/iop/iop_c.pdf", width = 15, height = 10, device = cairo_pdf)

## bbp
p3 <- res %>% 
  drop_na(mean_bbp) %>% 
  ggplot(aes(x = mean_bbp, y = depth_grid, color = factor(wavelength))) +
  geom_path() +
  facet_wrap(~date) +
  scale_y_reverse() +
  scale_color_manual(values = color) 

ggsave("graphs/iop/iop_bbp.pdf", width = 15, height = 10, device = cairo_pdf)

# Test --------------------------------------------------------------------

res %>% 
  drop_na(mean_a) %>% 
  # filter(date == "2016-05-04") %>%
  # filter(depth_grid %in% c(10)) %>%
  ggplot(aes(x = wavelength, y = mean_a, color = (depth_grid), group = depth_grid)) +
  geom_path(size = 0.1) +
  facet_wrap(~date)
