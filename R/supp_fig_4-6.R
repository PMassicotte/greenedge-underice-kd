# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure showing the average R2, slope and intercept of the regressions between
# Edz and Luz.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_feather("data/clean/cops_wavelength_interpolated.feather") %>%
  filter(between(wavelength, 400, 700)) %>%
  filter(depth >= 10) %>%
  group_by(profile_filename, wavelength) %>%
  mutate_at(.vars = vars(edz, luz), funs(. / .[depth == 10])) %>%
  drop_na(depth, edz, luz)

res <- df %>%
  group_by(profile_filename, wavelength) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(edz ~ luz, data = .))) %>%
  mutate(glance = map(mod, broom::glance)) %>%
  mutate(tidy = map(mod, broom::tidy))

# res$data[[1156]] %>%
#   ggplot(aes(x = edz, y = luz)) +
#   geom_point()

res2 <- res %>%
  unnest(glance) %>%
  unnest(tidy)

res2 <- res2 %>%
  dplyr::select(profile_filename:r.squared, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names(case = "old_janitor") %>%
  group_by(wavelength) %>%
  summarise_if(is.numeric, .funs = funs(mean, sd))


# Descriptive stats -------------------------------------------------------

range(res2$r_squared_mean)
range(res2$luz_mean)
range(res2$x_intercept_mean)

# Plots -------------------------------------------------------------------

p1 <- res2 %>%
  ggplot(aes(x = wavelength, y = r_squared_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = r_squared_mean - r_squared_sd, ymax = r_squared_mean + r_squared_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop("Determination", "coefficient" ~ "("*italic(R^2)*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

p2 <- res2 %>%
  ggplot(aes(x = wavelength, y = luz_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = luz_mean - luz_sd, ymax = luz_mean + luz_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop(Slope, "("*mu*W%*%cm^{-2}%*%nm^{-1}*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

p3 <- res2 %>%
  ggplot(aes(x = wavelength, y = x_intercept_mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = x_intercept_mean - x_intercept_sd, ymax = x_intercept_mean + x_intercept_sd), alpha = 0.25) +
  xlab("Wavelength (nm)") +
  ylab(bquote(atop(Intercept, "("*mu*W%*%cm^{-2}%*%nm^{-1}*")"))) +
  scale_x_continuous(breaks = seq(400, 700, by = 50))

# p <- cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO", align = "hv")
ggsave("graphs/supp_fig_4.pdf", p1, width = 3 * 1.61803398875, height = 3, device = cairo_pdf)



# Hydrolight compare Kd and KLu -------------------------------------------

read_hydrolight <- function(file, variable) {
  df <- readxl::read_excel(file, sheet = variable, skip = 3) %>%
    set_names(gsub("in air", "-1", names(.))) %>%
    set_names(gsub("X__1", "wavelength", names(.))) %>%
    gather(depth, !!variable, -wavelength) %>%
    filter(!grepl("air", depth)) %>%
    mutate_all(as.numeric) %>%
    janitor::clean_names(case = "old_janitor") %>%
    mutate(file_name = basename(file))
  
  return(df)
}


ed_without_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/sans_raman_0501.xlsx", "Kd")
lu_without_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/sans_raman_0501.xlsx", "KLu")
without_fluo <- inner_join(ed_without_fluo, lu_without_fluo)

ed_with_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/avec_raman_0501.xlsx", "Kd")
lu_with_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/avec_raman_0501.xlsx", "KLu")
with_fluo <- inner_join(ed_with_fluo, lu_with_fluo)

df <- bind_rows(without_fluo, with_fluo) %>%
  filter(between(wavelength, 400, 700)) %>%
  mutate(simulation_type = ifelse(str_detect(file_name, "sans_raman"), "Without modeled Raman", "With modeled Raman"))

df <- df %>%
  select(file_name, simulation_type, wavelength, depth, kd, klu) %>% 
  filter(depth >= 0)

rm(list = ls(pattern = "with"))

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")
color <- lapply(unique(df$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(df$wavelength))

p <- df %>% 
  # gather(type, k, kd, klu) %>% 
  # mutate(type = ifelse(type == "kd", "K[d]", "K[Lu]")) %>% 
  ggplot(aes(y = kd, x = klu, color = factor(wavelength))) +
  geom_point(size = 0.25, alpha = 0.5) +
  facet_wrap(~simulation_type) +
  scale_color_manual(values = color) +
  ylab(bquote(italic(K[d])~(m^{-1}))) +
  xlab(bquote(italic(K[Lu])~(m^{-1}))) +
  labs(color = "Wavelength (nm)") +
  geom_abline(lty = 2, size = 0.25) +
  guides(color = guide_legend(
    keywidth = 0.15,
    keyheight = 0.15,
    default.unit = "inch",
    ncol = 2,
    override.aes = list(size=1)
  )) 
  
ggsave("graphs/supp_fig_5.pdf", width = 7, height = 3, device = cairo_pdf)

# Hydrolight compare EdPAR and LuPAR --------------------------------------

read_hydrolight <- function(file, variable) {
  df <- readxl::read_excel(file, sheet = variable, skip = 3) %>%
    set_names(gsub("in air", "-1", names(.))) %>%
    set_names(gsub("X__1", "wavelength", names(.))) %>%
    gather(depth, !!variable, -wavelength) %>%
    filter(!grepl("air", depth)) %>%
    mutate_all(as.numeric) %>%
    janitor::clean_names(case = "old_janitor") %>%
    mutate(file_name = basename(file))
  
  return(df)
}

ed_with_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/avec_raman_0501.xlsx", "Kd")
lu_with_fluo <- read_hydrolight("data/raw/hydrolight/simon_lambert/avec_raman_0501.xlsx", "KLu")
with_fluo <- inner_join(ed_with_fluo, lu_with_fluo)

k <- with_fluo %>%
  filter(between(wavelength, 400, 700)) %>% 
  select(file_name, wavelength, depth, kd, klu) %>% 
  filter(depth >= 0)

rm(list = ls(pattern = "with"))


# Hydrolight compare EdPAR and LuPAR --------------------------------------

ed <- read_hydrolight("data/raw/hydrolight/simon_lambert/avec_raman_0501.xlsx", "Ed") %>%
  filter(between(wavelength, 400, 700)) %>% 
  filter(depth >= 0) %>%
  group_by(wavelength) %>%
  mutate(ed0 = ed[depth == 0])


# Interpolate Ed ----------------------------------------------------------

## Ed values have not the same dapth as K values

unique(ed$depth)
unique(k$depth)

ed <- ed %>% 
  group_by(wavelength) %>% 
  nest() %>% 
  mutate(res = map(data, function(x) {
    
    depth <- seq(0.5, 49.5, by = 1)
    
    af <- with(x, approxfun(depth, ed))
    tibble(depth, ed = af(depth), ed0 = unique(x$ed0))
    
  })) %>% 
  unnest(res)

# Associate K and Ed ------------------------------------------------------

df <- inner_join(ed, k, by = c("wavelength", "depth"))


# Propagate Ed0 using KLu -------------------------------------------------

df <- df %>% 
  mutate(ed_from_kd = ed0 * exp(-kd * depth)) %>% 
  mutate(ed_from_klu = ed0 * exp(-klu * depth))

df %>% 
  ggplot(aes(x = ed, y = depth)) +
  geom_point() +
  facet_wrap(~wavelength, scales = "free") +
  scale_y_reverse() +
  geom_path(aes(x = ed_from_klu), color = "red")

df %>% 
  ggplot(aes(x = ed_from_klu, y = ed_from_kd)) +
  geom_point() +
  facet_wrap(~wavelength, scales = "free")

# Calculate PAR -----------------------------------------------------------

h  <- 6.62607004e-34 # m2.kg.s-1, or J.s
c  <- 299792458      # m.s-1
av <- 6.022140857e23 # mol-1

res <- df %>%
  group_by(wavelength) %>%
  mutate(wavelength2 = wavelength * 1e-9) %>%
  mutate(ed_from_klu = ed_from_klu * wavelength2 / (h * c)) %>%
  mutate(ed_from_kd = ed_from_kd * wavelength2 / (h * c)) %>%
  arrange(wavelength, depth) %>%
  group_by(depth) %>%
  summarise(
    par_ed = pracma::trapz(wavelength2 / 1e-9, ed_from_kd),
    par_lu = pracma::trapz(wavelength2 / 1e-9, ed_from_klu),
    par_ed =  par_ed * 1E6 / av,
    par_lu =  par_lu * 1E6 / av
  )

res %>% 
  ggplot(aes(x = par_ed, y = par_lu, color = depth)) +
  geom_point() +
  geom_abline()

p <- res %>% 
  gather(type, par, -depth) %>% 
  ggplot(aes(x = type, y = par)) +
  geom_boxplot() +
  ylab(bquote("PAR"~(mu*Ein~m^{-2}~s^{-1}))) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  scale_x_discrete(
    breaks = c("par_ed", "par_lu"),
    labels = c(
      bquote(PAR[{K[d]}]),
      bquote(PAR[{K[Lu]}]))
    ) +
  theme(axis.title.x = element_blank())

ggsave("graphs/supp_fig_6.pdf", width = 3 * 1.61803398875, height = 3, device = cairo_pdf)

# Anova to check if significant difference --------------------------------

res2 <- res %>% 
  gather(type, par, -depth)

mod <- aov(par ~ type, data = res2)
summary(mod)

TukeyHSD(mod)


