# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Fig. 2 showing that Ed and Lu light profiles follow roughly the same patterns
# after 10 m. 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")


df <- read_feather("data/clean/cops.feather") %>% 
  filter(profile_filename == "GE2016.ICMP_ICEP_160620_CAST_006") %>% 
  filter(between(wavelength, 400, 700)) %>% 
  filter(depth >= 10) %>%
  group_by(wavelength) %>% 
  mutate_at(.vars = vars(edz, luz), function(x) x/max(x, na.rm = TRUE)) %>% 
  drop_na(depth, edz, luz)

color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

p <- df %>%
  ggplot(aes(y = depth, color = factor(wavelength))) +
  geom_path(aes(x = edz, linetype = "Downwelling irradiance (Ed)")) +
  geom_path(aes(x = luz, linetype = "Upwelling radiance (Lu)")) +
  scale_y_reverse(limits = c(NA, 0)) +
  theme(legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 3
  )) +
  labs(color = "Wavelengths (nm)",
       linetype = "Light type") +
  scale_color_manual(values = color) +
  facet_wrap(~wavelength, scales = "free", ncol = 3) +
  xlab("Normalized light (%)") +
  ylab("Depth (m)") +
  theme(legend.box = "horizontal")

ggsave("graphs/fig2.pdf", device = cairo_pdf, height = 8, width = 6)  
