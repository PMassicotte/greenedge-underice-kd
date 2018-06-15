# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Geometries of the simulations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

circle <- function(radius, angle = seq(0, 2 * pi, length.out = 200)) {
  tibble(
    x = radius * cos(angle),
    y = radius * sin(angle)
  )
}

r <- c(seq(5.5, 50, by = 5), 50)

sampling_circle <- map(r, circle) %>%
  set_names(r) %>%
  bind_rows(.id = "radius") %>%
  mutate(radius = parse_number(radius))

melt_pond <- circle(5)

configuration <- map2(sqrt(25 / c(0.25, 0.20, 0.15, 0.10, 0.05, 0.01)), seq(0, pi / 2, length.out = 6), circle) %>%
  set_names(c(0.25, 0.20, 0.15, 0.10, 0.05, 0.01)) %>%
  bind_rows(.id = "radius") %>%
  mutate(radius = parse_number(radius))

p <- ggplot() +
  geom_path(data = sampling_circle, aes(x = x, y = y, color = factor(radius)), size = 0.25) +
  geom_polygon(data = melt_pond, aes(x = x, y = y, fill = "Melt pond")) +
  scale_x_continuous(limits = c(-50, 50)) +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_fill_manual(values = c("Melt pond" = "#36454F")) +
  # scale_color_brewer(palette = "BuPu") +
  coord_equal() +
  labs(fill = "") +
  labs(color = "Sampling\ndistance (m)") +
  xlab("x-distance (m)") +
  ylab("y-distance (m)") +
  geom_segment(data = configuration, aes(x = 0, y = 0, xend = x, yend = y, group = radius), size = 0.25) +
  geom_label(data = configuration, aes(x = x, y = y, label = sprintf("%2.2f%% melt pond cover", radius * 100)), hjust = 0, label.size = 0, nudge_x = 1, size = 2, label.padding = unit(0.05, "lines")) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 1
  ))

ggsave("graphs/fig4.pdf", device = cairo_pdf, width = 5, height = 5)


# Panel A showing transmittance -------------------------------------------

r <- c(5, 50)

sampling_circle <- map(r, circle) %>%
  set_names(c(0.4, 0.75)) %>%
  bind_rows(.id = "transmittance") %>%
  mutate(transmittance = parse_number(transmittance))

sampling_circle %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y), fill = "red") +
  coord_equal() +
  labs(fill = "") +
  labs(color = "Sampling\ndistance (m)") +
  xlab("x-distance (m)") +
  ylab("y-distance (m)") 

