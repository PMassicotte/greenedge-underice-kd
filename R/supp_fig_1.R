# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Supplementary Fig. 1 (Study area).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

wm <- rworldmap::getMap(resolution = "high")

longitude_ic <- -63.78953
latitude_ic <- 67.47973

label <- data_frame(
  longitude = c(longitude_ic),
  latitude = c(latitude_ic),
  label = c("ice camp")
)

wm %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map(
    projection = "stereo",
    xlim = c(-70,-60),
    ylim = c(65, 70)
  ) +
  geom_text(
    data = label,
    aes(x = longitude_ic, y = latitude_ic, label = label),
    inherit.aes = FALSE,
    vjust = -0.25,
    hjust = -0.25,
    color = "red"
  ) +
  geom_point(
    data = label,
    aes(x = longitude, y = latitude),
    color = "red",
    inherit.aes = FALSE
  ) +
  xlab("Longitude") +
  ylab("Latitude")

ggsave("graphs/supp_fig_1.pdf", device = cairo_pdf)


# test --------------------------------------------------------------------


stere <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +datum=WGS84 +units=m"

wm %>% 
  sp::spTransform( CRS(stere))

wm %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map(
    projection = "stereo",
    # xlim = c(-70,-60),
    ylim = c(65, 70)
  ) + 
  scale_y_continuous(breaks = seq(-180, 180, by = 50)) +
  theme_light()
