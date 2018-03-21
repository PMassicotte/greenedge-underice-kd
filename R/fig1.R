# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Fig 1 of the article.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

cops <- read_feather("data/clean/cops_wavelength_interpolated.feather")

df <- cops %>% 
  filter(profile_filename == "GE2016.ICMP_ICEP_160620_CAST_006") %>% 
  dplyr::select(depth, wavelength, edz, luz) %>% 
  gather(type, light, -depth, -wavelength) %>% 
  filter(wavelength %in% seq(400, 700, by = 20))

color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

mylabel <- c(
  edz = "Downwelling irradiance (Ed)",
  luz = "Upwelling radiance (Lu)"
)

p1 <- df %>% 
  drop_na() %>% 
  filter(type == "edz") %>% 
  ggplot(aes(light, depth, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~type, scales = "free", labeller = labeller(type = mylabel)) +
  ylab("Depth (m)") +
  xlab(bquote(Ed~"("*mu*W%*%cm^{-2}%*%nm^{-1}*")")) +
  labs(color = "Wavelengths (nm)") +
  theme(legend.position = "none") +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 2
  )) +
  scale_color_manual(values = color)

p2 <- df %>% 
  drop_na() %>% 
  filter(type == "luz") %>% 
  ggplot(aes(light, depth, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~type, scales = "free", labeller = labeller(type = mylabel)) +
  ylab("Depth (m)") +
  xlab(bquote(Lu~"("*mu*W%*%cm^{-2}%*%nm^{-1}%*%sr^{-1}*")")) +
  labs(color = "Wavelengths (nm)") +
  theme(
  legend.position = c(0.99, 0.01),
  legend.justification = c(1, 0)
) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 3
  )) +
  scale_color_manual(values = color)

p <- cowplot::plot_grid(p1, p2, ncol = 2, labels = "AUTO", align = "hv")

ggsave("graphs/fig1.pdf", plot = p, device = cairo_pdf, height = 3, width = 7)  

