# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Interpolate COPS wavelengths. Only for 2016.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_feather("data/clean/cops.feather") %>% 
  filter(lubridate::year(posixct_date_utc) == 2016) %>% 
  select(-euz) %>% 
  drop_na(edz, luz)

# Interpolate wavelengths -------------------------------------------------

cops <- cops %>%
  group_by(
    depth,
    profile_filename,
    flag_quality,
    posixct_date_utc,
    latitude_dec_degrees,
    longitude_dec_degrees,
    sun_zenith_angle_degrees,
    hole_type
  ) %>% 
  nest()

interpol_wavelength <- function(df) {
  
  wavelength <- seq(400, 700, by = 10)
  
  sf_edz <- approxfun(df$wavelength, df$edz)
  sf_luz <- approxfun(df$wavelength, df$luz)
  
  res <- data_frame(
    wavelength,
    edz = sf_edz(wavelength),
    luz = sf_luz(wavelength)
  )
  
  return(res)
    
}

res <- cops %>% 
  mutate(pred = map(data, interpol_wavelength))

res %>% 
  unnest(pred) %>% 
  write_feather("data/clean/cops_wavelength_interpolated.feather")


# Visualise ---------------------------------------------------------------

# res$profile_filename[1]

dat1 <- res %>%
  filter(profile_filename == "GE2016.ICMP_ICEP_160713_CAST_006") %>%
  unnest(pred)

dat2 <- res %>%
  filter(profile_filename == "GE2016.ICMP_ICEP_160713_CAST_006") %>%
  unnest(data)

p <- dat2 %>%
  ggplot(aes(x = wavelength, y = edz)) +
  geom_line() +
  facet_wrap(~depth, scales = "free") +
  geom_line(data = dat1, color = "red")
# 
ggsave("graphs/cops/interpolation_wavelengths.pdf", height = 20, width = 40, device = cairo_pdf)
