# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Fig. 4 showing that Ed and Lu light profiles follow roughly the same patterns
# after 10 m.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

# https://stackoverflow.com/questions/27435453/how-to-normalise-subgroups-from-a-grouped-data-frame-in-r

df <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>%
  filter(profile_filename == "GE2016.ICMP_ICEP_160620_CAST_006") %>%
  filter(between(wavelength, 400, 700)) %>%
  filter(depth >= 10) %>%
  group_by(wavelength) %>%
  mutate_at(.vars = vars(edz, luz), funs(. / .[depth == 10])) %>%
  drop_na(depth, edz, luz)

color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

appender <- function(string, suffix = " nm") paste0(string, suffix)

p <- df %>%
  select(depth, wavelength, edz, luz) %>% 
  filter(wavelength %in% c(seq(400, 700, by = 20))) %>%
  gather(type, value, edz, luz) %>% 
  ggplot(aes(x = value, y = depth, color = factor(wavelength), linetype = type)) +
  geom_path() +
  scale_y_reverse(limits = c(NA, 0)) +
  scale_x_continuous(labels = scales::percent, expand = c(0.05, 0.05)) +
  theme(
    legend.position = c(0.9, 0),
    legend.justification = c(1, 0)
  ) +
  labs(
    linetype = "Radiometric quantity"
  ) +
  scale_color_manual(values = color) +
  facet_wrap(~wavelength, ncol = 3, labeller = as_labeller(appender)) +
  xlab("Normalized profiles") +
  ylab("Depth (m)") +
  theme(legend.box = "horizontal") +
  theme(axis.text = element_text(size = 10)) +
  scale_linetype_manual(
    breaks = c("edz", "luz"),
    values = c(1, 2),
    labels = c(
      bquote(Downward~irradiance~(italic(E[d]))),
      bquote(Upward~radiance~(italic(L[u])))
    )
  ) +
  guides(colour = FALSE)

ggsave("graphs/fig4.pdf", device = cairo_pdf, height = 8, width = 6)


# Correlation coefficients ------------------------------------------------

# df <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>%
#   filter(between(wavelength, 400, 700)) %>%
#   filter(depth >= 10) %>%
#   group_by(profile_filename, wavelength) %>%
#   mutate_at(.vars = vars(edz, luz), funs(. / .[depth == 10])) %>%
#   drop_na(depth, edz, luz)
#
# res <- df %>%
#   group_by(profile_filename, wavelength) %>%
#   summarise(r = cor(edz, luz)) %>%
#   group_by(wavelength) %>%
#   summarise(mean_r = mean(r), sd_r = sd(r), n = n())
#
# res %>%
#   ggplot(aes(x = wavelength, y = mean_r)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymin = mean_r - sd_r, ymax = mean_r + sd_r), alpha = 0.25) +
#   xlab("Wavelength (nm)") +
#   ylab("Mean Pearson correlation\ncoefficient (r)") +
#   scale_x_continuous(breaks = seq(400, 700, by = 50))
#
# ggsave("graphs/fig2b.pdf", device = cairo_pdf, height = 3, width = 4)