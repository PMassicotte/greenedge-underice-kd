# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Supplementary Fig. 1 (Study area).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

wm <- rworldmap::getMap(resolution = "high")

label <- data_frame(
  longitude = c(-63.78953, -64.033),
  latitude = c(67.47973, 67.550),
  label = c("Ice camp", "Qikiqtarjuaq"),
  color = c("red", "blue")
)

p <- wm %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map(
    projection = "stereo",
    xlim = c(-65,-62.5),
    ylim = c(67, 68)
  ) +
  geom_text(
    data = label,
    aes(x = latitude, y = longitude, label = label, color = label),
    inherit.aes = FALSE,
    vjust = -0.25,
    hjust = -0.25,
    show.legend = FALSE
  ) +
  geom_point(
    data = label,
    aes(x = longitude, y = latitude, color = label),
    inherit.aes = FALSE,
    size = 3
  ) +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.9, 0.9), legend.justification = c(1, 1))

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
    xlim = c(-70,-60),
    ylim = c(65, 70)
  ) + 
  scale_y_continuous(breaks = seq(-180, 180, by = 50)) +
  theme_light()
