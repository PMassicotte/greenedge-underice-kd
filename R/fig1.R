rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

cops <- read_feather("data/clean/cops.feather")

df <- cops %>% 
  filter(profile_filename == "GE2016.ICMP_ICEP_160620_CAST_005") %>% 
  select(depth, wavelength, edz, luz) %>% 
  gather(type, light, -depth, -wavelength) %>% 
  filter(between(wavelength, 400, 700))

color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

mylabel <- c(
  edz = "Downwelling irradiance (Ed)",
  luz = "Upwelling radiance (Lu)"
)

p <- df %>% 
  drop_na() %>% 
  ggplot(aes(light, depth, color = factor(wavelength))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~type, scales = "free", labeller = labeller(type = mylabel)) +
  ylab("Depth (m)") +
  xlab(bquote(Light~(mu*W%*%cm^{-2}%*%nm^{-1}))) +
  labs(color = "Wavelengths") +
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(
    keywidth = 0.1,
    keyheight = 0.1,
    default.unit = "inch",
    ncol = 2
  )) +
  scale_color_manual(values = color)

ggsave("graphs/fig1.pdf", device = cairo_pdf, height = 3, width = 6)  
