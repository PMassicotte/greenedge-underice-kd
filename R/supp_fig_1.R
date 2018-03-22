rm(list = ls())

stere <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

baffin <- st_read("../green-edge/green-edge/data/shapefiles/baffin/baffin.shp")
baffin <- st_transform(baffin, stere)

label <- data_frame(
  longitude = c(-63.78953),
  latitude = c(67.47973),
  label = c("Qikiqtarjuaq ice camp")
) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(stere)

# data_frame(
#   longitude = c(-63),
#   latitude = c(72)
# ) %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
#   st_transform(stere)

p <- ggplot(baffin) +
  geom_sf(size = 0.1) +
  theme(panel.grid.major = element_line(size = 0.05, color = "gray90")) +
  geom_sf(data = label, color = "red") +
  coord_sf(
    ylim = c(-3.6e06, -1e06),
    xlim = c(-2600000, -100000)
  ) +
  scale_x_continuous(breaks = seq(-90, -30, by = 10)) +
  scale_y_continuous(breaks = seq(50, 80, by = 3)) +
  annotate("text", x = -795630.7, y = -2338551, label = "Qikiqtarjuaq ice camp", size = 2.5, vjust = -1, hjust = -0.1, family = "IBM Plex Sans") +
  annotate("text", x = -2174308, y = -2533284, label = "Hudson Bay", family = "IBM Plex Sans") +
  annotate("text", x = -76357.57, y = -2184929, label = "Greenland", angle = 90, family = "IBM Plex Sans") +
  annotate("text", x = -1720983, y = -3435623, label = "Québec", family = "IBM Plex Sans") +
  annotate("text", x = -607354.6, y = -1915067, label = "Baffin Bay", family = "IBM Plex Sans") +
  theme(axis.title = element_blank()) +
  scalebar(x.min = -1720983, x.max = -70357.57, y.min = -3535623, y.max = -1584929, dd2km = FALSE, dist = 100, st.size = 2)

ggsave("graphs/supp_fig_1.pdf")
embed_fonts("graphs/supp_fig_1.pdf")
