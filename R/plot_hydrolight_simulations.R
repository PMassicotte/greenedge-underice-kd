# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Plot Hydrolight simulations done by Simon Belanger.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

read_hydrolight <- function(file, variable) {
  df <- readxl::read_excel(file, sheet = variable, skip = 3) %>%
    gather(depth, !!variable, -wavelen) %>%
    mutate_all(as.numeric) %>%
    janitor::clean_names()

  return(df)
}

# Without Fluo ------------------------------------------------------------

ed <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_NoFluo.xlsx", "Kd")
lu <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_NoFluo.xlsx", "KLu")

df <- inner_join(ed, lu) %>%
  filter(between(wavelen, 400, 700))

color <- lapply(unique(df$wavelen), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelen))

p1a <- df %>%
  ggplot(aes(x = kd, y = klu, color = factor(wavelen))) +
  geom_point() +
  facet_wrap(~ depth) +
  geom_abline(color = "red", lty = 2) +
  scale_color_manual(values = color) +
  labs(title = "Without fluorescence")

p1b <- df %>%
  ggplot(aes(x = kd, y = klu, color = depth)) +
  geom_point() +
  facet_wrap(~ wavelen, scales = "free") +
  geom_abline(color = "red", lty = 2) +
  labs(title = "Without fluorescence")

# With fluo ---------------------------------------------------------------

ed <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_FCHL_RAMAN_FDOM2.xlsx", "Kd")
lu <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_FCHL_RAMAN_FDOM2.xlsx", "KLu")

df <- inner_join(ed, lu) %>%
  filter(between(wavelen, 400, 700))


color <- lapply(unique(df$wavelen), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelen))

p2a <- df %>%
  ggplot(aes(x = kd, y = klu, color = factor(wavelen))) +
  geom_point() +
  facet_wrap(~ depth) +
  geom_abline(color = "red", lty = 2) +
  scale_color_manual(values = color) +
  labs(title = "With fluorescence")

p2b <- df %>%
  ggplot(aes(x = kd, y = klu, color = depth)) +
  geom_point() +
  facet_wrap(~ wavelen, scales = "free") +
  geom_abline(color = "red", lty = 2) +
  labs(title = "With fluorescence")

# Save plots --------------------------------------------------------------

pdf("graphs/hydrolight.pdf", width = 16, height = 12)

p1a
p1b
p2a
p2b

dev.off()
