# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# This script interpolates IOPs to match wavelengths of the COPS.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

wl <- seq(400, 700, by = 10)

## Read iop data.
iop <- read_feather("data/clean/iop.feather") %>% 
  filter(between(depth_grid, 10, 80)) ## Only 0-80 m to match cops depths

# iop %>% 
#   group_by(date, depth_grid) %>%
#   summarise_at(.vars = vars(contains("mean")), funs(sum(!is.na(.)))) %>% 
#   filter_at(.vars = vars(contains("mean")), any_vars(. == 0)) %>% 
#   View()
# 
# filter(iop, date == "2016-05-13" & depth_grid == 1) %>% View()

## Interpolate data

interpol <- function(df, wl) {
  
  ## Here I can do the 3 interpolations (a, c, bb)
  sf <- approxfun(df$wavelength, df$mean_a)
  mean_a_interpol <- sf(wl)
  
  sf <- approxfun(df$wavelength, df$mean_bbp)
  mean_bbp_interpol <- sf(wl)
  
  # sf <- splinefun(df$wavelength, df$mean_c)
  # mean_c_interpol <- sf(cops_wl)
  
  res <- data.frame(wavelength = wl, mean_a_interpol, mean_bbp_interpol)
  
  return(res)
  
}

# Interpolation -----------------------------------------------------------

## Use DT, much faster
setDT(iop)
res <- iop[, interpol(.SD, wl), by = .(date, depth_grid)] %>% 
  as_tibble()

# Plot --------------------------------------------------------------------

## Check results at different depths
p1 <- iop %>% 
  drop_na(mean_bbp) %>% 
  filter(depth_grid %in% c(10, 25, 50)) %>% 
  ggplot(aes(x = wavelength, y = mean_bbp)) +
  geom_line() +
  geom_point() +
  facet_grid(date ~ depth_grid) +
  geom_line(data = filter(res, depth_grid %in% c(10, 25, 50)), aes(y = mean_bbp_interpol), color = "red")

p2 <- iop %>% 
  drop_na(mean_a) %>% 
  filter(depth_grid %in% c(10, 25, 50)) %>% 
  ggplot(aes(x = wavelength, y = mean_a)) +
  geom_line() +
  geom_point() +
  facet_grid(date ~ depth_grid) +
  geom_line(data = filter(res, depth_grid %in% c(10, 25, 50)), aes(y = mean_a_interpol), color = "red")

pdf("graphs/iop/interpolated_iop.pdf", width = 6, height = 25)
p1
p2
dev.off()

# ggsave("graphs/iop/interpolated_iop.pdf", device = cairo_pdf, width = 6, height = 25)


# bbp ---------------------------------------------------------------------

## Use the value of bbp at 420 as the one of 400 and 410

res <- res %>% 
  group_by(date, depth_grid) %>% 
  fill(mean_bbp_interpol, .direction = "up") %>% 
  ungroup()

# By depth range ----------------------------------------------------------

res <- res %>%
  expand(
    nesting(
      start_depth = seq(10, 75, by = 5),
      end_depth = seq(10, 75, by = 5) + 5),
    nesting(
      wavelength,
      depth_grid,
      date,
      mean_a_interpol,
      mean_bbp_interpol
    )
  ) %>%
  group_by(date, start_depth, end_depth, wavelength) %>%
  filter(between(depth_grid, start_depth, end_depth)) %>% 
  summarise_at(vars(mean_a_interpol, mean_bbp_interpol), mean)

write_feather(res, "data/clean/iop_interpolated.feather")
