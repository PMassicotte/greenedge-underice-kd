# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Geometries of the simulations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())


# Plot 1 ------------------------------------------------------------------

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

frame <- tibble(xmin = -50, xmax = 50, ymin = -50, ymax = 50)

p1 <- ggplot() +
  geom_rect(data = frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", color = "#57e5d6") +
  geom_path(data = sampling_circle, aes(x = x, y = y, color = factor(radius)), size = 0.25) +
  geom_polygon(data = melt_pond, aes(x = x, y = y, fill = "Melt pond")) +
  scale_x_continuous(limits = c(-50, 50)) +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_fill_manual(values = c("Melt pond" = "#6097ce")) +
  # scale_color_brewer(palette = "BuPu") +
  labs(fill = "") +
  labs(color = "Sampling\ndistance (m)") +
  xlab("x-distance (m)") +
  ylab("y-distance (m)") +
  geom_segment(data = configuration, aes(x = 0, y = 0, xend = x, yend = y, group = radius), size = 0.25) +
  geom_label(data = configuration, aes(x = x, y = y, label = sprintf("%2.0f%% melt pond cover", radius * 100)), hjust = 0, label.size = 0, nudge_x = 1, size = 2, label.padding = unit(0.05, "lines")) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    nrow = 4
  )) +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.position = "top", legend.direction = "horizontal")


# Plot 2 ------------------------------------------------------------------

frame <- tibble(xmin = -120, xmax = 120, ymin = -150, ymax = 0)
segment <- tibble(x = rep(-120, 6), y = seq(-25, 0, by = 5), xend = rep(120, 6), yend = seq(-25, 0, by = 5))
ice <- tibble(xmin = -120, xmax = 120.25, ymin = 0, ymax = 5)
mp <- tibble(xmin = -5, xmax = 5, ymin = 0, ymax = 5)

p2 <- ggplot() +
  geom_rect(data = frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", color = "black") +
  geom_segment(data = segment, aes(x = x, y = y, xend = xend, yend = yend), lty = 2, size = 0.25) +
  geom_rect(data = ice, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#36454F") +
  geom_rect(data = mp, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#6097ce", color = "transparent") +
  annotate("segment", x = -50, xend = -50, y = 0, yend = -25, lty = 1, size = 0.75, color = "#57e5d6") +
  annotate("segment", x = 50, xend = 50, y = 0, yend = -25, lty = 1, size = 0.75, color = "#57e5d6") +
  annotate("segment", x = 50, xend = -50, y = -25, yend = -25, lty = 1, size = 0.75, color = "#57e5d6") +
  
  annotate("text", x = 35, y = -50, label = str_wrap("3D volume for which simulated data was extracted", width = 30), size = 3, family = "IBM Plex Sans Light") +
  annotate("segment", x = 25, xend = 25, y = -40, yend = -28, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm"))) +
  
  annotate("text", x = -75, y = -50, label = str_wrap("2D detectors placed every 0.5 m depth", width = 20), size = 3, family = "IBM Plex Sans Light") +
  annotate("segment", x = -75, xend = -75, y = -40, yend = -28, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm"))) +
  
  # annotate("label", x = 28, y = -5, label = "Melt pond", size = 3, label.size = NA) +
  # annotate("segment", x = 10, xend = 5, y = -5, yend = -2, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm"))) +
  
  annotate("segment", x = 0, xend = 0, y = 0, yend = -15, lty = 1, size = 1, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = 0, xend = 10, y = 0, yend = -10, lty = 1, size = 1, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = 0, xend = -10, y = 0, yend = -10, lty = 1, size = 1, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  
  annotate("segment", x = 75, xend = 75, y = 0, yend = -15, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = 75, xend = 85, y = 0, yend = -10, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = 75, xend = 65, y = 0, yend = -10, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  
  annotate("segment", x = -75, xend = -75, y = 0, yend = -15, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = -75, xend = -85, y = 0, yend = -10, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  annotate("segment", x = -75, xend = -65, y = 0, yend = -10, lty = 1, size = 0.25, arrow = arrow(length = unit(1.5, "mm")), color = "orange") +
  
  annotate("text", x = -110, y = -100, label = "Homogeneous water column:", size = 3.5, hjust = 0, fontface = 2, family = "IBM Plex Sans Light") +
  annotate("text", x = -110, y = -110, label = expression("a = b = 0.05"*m^{-1}), size = 3, hjust = 0, family = "IBM Plex Sans Light") +
  annotate("text", x = -110, y = -120, label = "VSF: Fourrier-Forand 3%", size = 3, hjust = 0, family = "IBM Plex Sans Light") +
  
  theme(panel.grid = element_blank()) +
  theme(panel.border = element_blank()) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-120, 120.25)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-150, 0, by = 20)) +
  xlab("Horizontal distance (m)") +
  ylab("Depth (m)")


# Save --------------------------------------------------------------------

p <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO", rel_heights = c(1, 0.7))

ggsave("graphs/fig1.pdf", device = cairo_pdf, width = 4, height = 8)



# Test --------------------------------------------------------------------

# p2 +
#   geom_path(data = filter(sampling_circle, y > 0), aes(x = x, y = y, color = factor(radius)), size = 0.25) +
#   geom_segment(data = configuration, aes(x = 0, y = 0, xend = x, yend = y, group = radius), size = 0.25) +
#   geom_label(data = configuration, aes(x = x, y = y, label = sprintf("%2.0f%% melt pond cover", radius * 100)), hjust = 0, label.size = 0, nudge_x = 1, size = 2, label.padding = unit(0.05, "lines"))
# 
# ggsave("graphs/fig1.pdf", device = cairo_pdf, width = 12, height = 8)
