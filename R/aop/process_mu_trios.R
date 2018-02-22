# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Process mu data from Trios measurments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <- list.files("data/raw/trios/", pattern = "^\\d+", full.names = TRUE)

read_trios <- function(file) {
 
  df_planar <- readxl::read_excel(file, sheet = "epi2") %>% 
    mutate(type = "planar") %>% 
    slice(6:nrow(.))
   
  df_scalar <- readxl::read_excel(file, sheet = "esi2") %>% 
    mutate(type = "scalar") %>% 
    slice(6:nrow(.))
  
  df <- bind_rows(df_planar, df_scalar) %>% 
    janitor::clean_names() %>% 
    mutate(date_time = lubridate::make_datetime(year, month, day, hour, minute, second, tz = "UTC")) %>% 
    mutate(file_name = basename(file)) %>% 
    select(-(year:second)) %>% 
    gather(wavelength, value, starts_with("x"), convert = TRUE) %>% 
    spread(type, value) %>% 
    mutate(wavelength = parse_number(wavelength)) %>% 
    mutate(mud = planar / scalar)
  
  return(df)
  
}

trios <- map(files, read_trios) %>% 
  bind_rows()

range(trios$mud)

## Data cleaning

trios <- trios %>% 
  filter(between(mud, 0, 1)) %>% 
  group_by(file_name, depth_m, wavelength) %>% 
  summarise(mud = mean(mud, na.rm = TRUE))

## Plot

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

color <- lapply(unique(trios$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(trios$wavelength))

trios %>% 
  # filter(wavelength %in% c(628)) %>%
  # filter(date == "2016-06-27") %>% 
  ggplot(aes(x = mud, y = depth_m, color = factor(wavelength), group = interaction(file_name, wavelength))) +
  geom_path() +
  facet_wrap(~file_name) +
  scale_y_reverse() +
  theme(legend.position = "none") +
  scale_color_manual(values = color)

ggsave("graphs/trios/mud_raw.pdf", device = cairo_pdf)

## Average to only get one profile

avg <- function(df) {
  
  depth_m <- seq(10, 20, by = 0.25)
  sf <- approxfun(df$depth_m, df$mud)
  mud <- sf(depth_m)
  
  res <- data_frame(depth_m, mud)
  return(res)
  
}

pred <- trios %>% 
  group_by(file_name, wavelength) %>% 
  nest() %>% 
  mutate(pred = map(data, avg))
  
# pred %>% 
#   filter(file_name == "14June2016_P1.xlsx" & wavelength == 400) %>% 
#   unnest(data) %>% 
#   ggplot(aes(x = mud, y = depth_m)) +
#   geom_path() +
#   scale_y_reverse()
# 
# tt <- pred %>% 
#   filter(file_name == "14June2016_P1.xlsx" & wavelength == 400) %>% 
#   unnest(pred)
#  
#   last_plot() +
#   geom_point(data = tt, aes(mud, depth_m), color = "red")

pred <- pred %>% 
  unnest(pred) %>% 
  group_by(wavelength, depth_m) %>% 
  summarise(mud = mean(mud, na.rm = TRUE))

## Take the value at 20 m and use it up til 80m

pred <- pred %>%
  complete(depth_m = seq(10, 80, by = 0.25), wavelength) %>% 
  group_by(wavelength) %>% 
  fill(mud) %>% 
  ungroup()

# Plot --------------------------------------------------------------------

p1 <- pred %>%
  filter(depth_m <= 20) %>% 
  ggplot(aes(x = mud, y = depth_m, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  theme(legend.position = "none") +
  scale_color_manual(values = color) +
  labs(title = "Average of 11 Trios measurements")

p2 <- pred %>%
  ggplot(aes(x = mud, y = depth_m, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  theme(legend.position = "none") +
  scale_color_manual(values = color) +
  labs(title = "Same as A but with value at 20m used up to 80m")

p <- cowplot::plot_grid(p1, p2, align = "hv", labels = "AUTO")
ggsave("graphs/trios/mud_averaged.pdf", device = cairo_pdf, width = 12)

# Average by depth range --------------------------------------------------

pred <- pred %>%
  expand(nesting(
    start_depth = seq(10, 75, by = 5),
    end_depth = seq(10, 75, by = 5) + 5
  ),
  nesting(wavelength,
          depth_m,
          mud)) %>%
  group_by(start_depth, end_depth, wavelength) %>%
  filter(between(depth_m, start_depth, end_depth)) %>% 
  nest() %>% 
  mutate(mean_mud = map_dbl(data, ~mean(.$mud)))

pred %>% 
  # filter(wavelength %in% c(400, 401)) %>% 
  mutate(mid_depth = (start_depth + end_depth) / 2) %>% 
  filter(mid_depth <= 35) %>% 
  ggplot(aes(x = mean_mud, y = mid_depth, color = factor(wavelength))) +
  geom_path() +
  geom_point() +
  scale_y_reverse() +
  theme(legend.position = "none") +
  scale_color_manual(values = color)

ggsave("graphs/trios/mud_averaged_by_5m.pdf", device = cairo_pdf)


# Save --------------------------------------------------------------------

pred %>% 
  select(-data) %>% 
  write_feather("data/clean/mud_trios.feather")
