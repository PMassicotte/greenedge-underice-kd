# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Supplementary Fig. 1 (Study area).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# stere <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +datum=WGS84 +units=m"

stere <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

baffin <- st_read("../green-edge/green-edge/data/shapefiles/baffin/baffin.shp")
land <- st_read("data/shapefiles/ne_10m_land/ne_10m_land.shp") 
ocean <- st_read("data/shapefiles/ne_10m_ocean/ne_10m_ocean.shp") 

# test <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") %>% 
#   st_intersection(baffin) %>% 
#   st_transform(crs = stere)
# 
# test %>% 
#   ggplot() +
#   geom_sf()

land <- st_transform(land, stere)
ocean <- st_transform(ocean, stere)
baffin <- st_transform(baffin, stere)

label <- data_frame(
  longitude = c(-63.78953, -64.033),
  latitude = c(67.47973, 67.550),
  label = c("Ice camp", "Qikiqtarjuaq"),
  color = c("red", "blue")
) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_transform(stere) %>% 
  as.data.frame()

label <- data_frame(
  longitude = c(-795630.7),
  latitude = c(-2338551),
  label = c("Ice camp")
)

p <- ggplot() +
  geom_sf(data = ocean, fill = "#98afd6") +
  geom_sf(
    data = land,
    fill = "gray95",
    size = 0.05,
    color = "black"
  ) +
  coord_sf(ylim = c(-3.6e06,-1e06),
           xlim = c(-2600000,-100000)) +
  geom_text(
    data = label,
    aes(
      x = latitude,
      y = longitude,
      label = label
    )
  ) +
  geom_point(
    data = label,
    aes(x = longitude, y = latitude),
    inherit.aes = FALSE,
    size = 3, 
    color = "red"
  ) +
  theme(axis.title = element_blank())

ggsave("graphs/supp_fig_1.pdf")
embed_fonts("graphs/supp_fig_1.pdf")
