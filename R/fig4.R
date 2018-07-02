# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Fig. 3 showing the relation between Ked and Klu. Data are generated in the
# script "calculate_k.R".
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

# Plot ked vs klu ---------------------------------------------------------

k <- read_feather("data/clean/k_cops.feather")

r2_threshold <- 0.99

k2 <- k %>%
  filter(r2 >= r2_threshold & k > 0) %>% 
  unite(depth_range, start_depth, end_depth, remove = FALSE, sep = "-") %>% 
  dplyr::select(profile_filename, depth_range, type, wavelength, k) %>% 
  spread(type, k) %>% 
  rename(ked = edz, klu = luz) %>% 
  drop_na(ked, klu)


color <- lapply(unique(k2$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(k2$wavelength))

# https://stackoverflow.com/questions/28436855/change-the-number-of-breaks-using-facet-grid-in-ggplot2
equal_breaks <- function(n = 4, s = 0.05, ...) {
  function(x) {
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    # seq(min(x) + d, max(x) - d, length = n).
    round(seq(min(x) + d, max(x) - d, length = n), 2)
  }
}

formula <- y ~ x

p <- k2 %>%
  filter(wavelength < 589) %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = factor(wavelength)), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Wavelength (nm)") +
  scale_color_manual(values = color) +
  facet_wrap(~depth_range, scales = "free", ncol = 3) +
  theme(legend.position = c(0.99, -0.05),
        legend.justification = c(1, 0)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 3
  ))  +
  scale_x_continuous(breaks = equal_breaks(n = 3, s = 0.05)) +
  stat_poly_eq(
    aes(label =  paste("atop(", ..eq.label.., ")", sep = ",")),
    formula = formula,
    parse = TRUE,
    label.x.npc = "right",
    label.y.npc = "bottom",
    vjust = -0.3,
    size = 2
  ) +
  stat_poly_eq(
    aes(label =  paste("atop(", ..rr.label.., ")", sep = ",")),
    formula = formula,
    parse = TRUE,
    label.x.npc = "right",
    label.y.npc = "bottom",
    vjust = 0,
    size = 2
  )

ggsave("graphs/fig4.pdf", device = cairo_pdf, height = 8, width = 7)  


# Some stats for the paper ------------------------------------------------

mod <- k2 %>% 
  filter(wavelength < 589) %>% 
  group_by(depth_range) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(ked ~ klu, data = .))) %>% 
  mutate(r2 = map(mod, broom::glance)) %>% 
  unnest(r2)

mod

range(mod$r.squared)

# Supplementary figure ----------------------------------------------------

## Show the global relation 

p <- k2 %>%
  ggplot(aes(x = klu, y = ked)) +
  geom_point(aes(color = factor(wavelength)), size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
  xlab(bquote(K[Lu]~(m^{-1}))) +
  ylab(bquote(K[Ed]~(m^{-1}))) +
  labs(color = "Wavelength (nm)") +
  scale_color_manual(values = color) +
  facet_wrap(~depth_range, scales = "free", ncol = 3) +
  theme(legend.position = c(0.99, -0.05),
        legend.justification = c(1, 0)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 4
  ))  +
  scale_x_continuous(breaks = equal_breaks(n = 3, s = 0.05))

ggsave("graphs/supp_fig_3.pdf", device = cairo_pdf, height = 8, width = 7)

# Table -------------------------------------------------------------------

mod <- k2 %>%
  filter(wavelength < 589) %>% 
  group_by(depth_range) %>%
  nest() %>% 
  mutate(mod = map(data, ~lm(ked ~ klu, data = .)))

mod <- mod %>%
  mutate(mysummary = map(mod, summary)) %>% 
  mutate(r2 = map_dbl(mysummary, "r.squared")) %>% 
  mutate(coef = map(mod, function(x) {
    
    df <- data_frame(intercept = coef(x)[1], slope = coef(x)[2])
    return(df)
  })) %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  unnest(coef) 

table <- mod %>% 
  dplyr::select(depth_range, intercept, slope, r2, n)

# https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf

knitr::kable(
  table,
  format = "latex",
  booktabs = TRUE,
  caption = "Regression coefficients used to convert KLu into Ked."
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>% 
  writeLines("article/applied-sciences//tables/table1.tex")

# p5 <- k2 %>% 
#   filter(wavelength < 589) %>% 
#   ggplot(aes(x = klu, y = ked)) +
#   geom_point(aes(color = factor(wavelength)), size = 1) +
#   geom_smooth(method = "lm") +
#   geom_abline(slope = 1, intercept = 0, lty = 2, color = "grey50") +
#   xlab(bquote(K[Lu]~(m^{-1}))) +
#   ylab(bquote(K[Ed]~(m^{-1}))) +
#   labs(color = "Depth (m)") +
#   scale_color_manual(values = color) +
#   facet_wrap(~depth_range2, scales = "free", ncol = 3) +
#   labs(title = "Ked vs Klu",
#        subtitle =  str_wrap(sprintf("Includes %d observations (divided by depth ranges).", nrow(k2))))

## Save the graphs
# ggsave("graphs/compare_ked_klu_a.pdf", p1, device = cairo_pdf, height = 6, width = 8)
# ggsave("graphs/compare_ked_klu_b.pdf", p2, device = cairo_pdf, height = 6, width = 8)
# ggsave("graphs/compare_ked_klu_c.pdf", p3, device = cairo_pdf, width = 20, height = 20)
# ggsave("graphs/compare_ked_klu_d.pdf", p4, device = cairo_pdf, height = 8, width = 9)
# ggsave("graphs/compare_ked_klu_e.pdf", p5, device = cairo_pdf, height = 8, width = 9)