# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Use Hydrolight simulations to propagate Ed using Klu. Then quantify the effect
# of Raman scattering on calculated PAR. This is done because the relation
# between Ked and Klu is not good in the red region.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

# Read data ---------------------------------------------------------------

read_hydrolight <- function(file, variable) {
  df <- readxl::read_excel(file, sheet = variable, skip = 3) %>%
    gather(depth, !!variable, -wavel) %>%
    filter(!grepl("air", depth)) %>%
    mutate_all(as.numeric) %>%
    janitor::clean_names(case = "old_janitor") %>%
    mutate(file_name = basename(file)) %>%
    rename(wavelength = wavel)

  return(df)
}

ed_without_fluo <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_NoFluo.xlsx", "Ed")
lu_without_fluo <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_NoFluo.xlsx", "Lu")
without_fluo <- inner_join(ed_without_fluo, lu_without_fluo)

ed_with_fluo <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_FCHL_RAMAN_FDOM.xlsx", "Ed")
lu_with_fluo <- read_hydrolight("~/Desktop/HL_simulations_IML4/MIML4_20150810_FCHL_RAMAN_FDOM.xlsx", "Lu")
with_fluo <- inner_join(ed_with_fluo, lu_with_fluo)

df <- bind_rows(without_fluo, with_fluo) %>%
  filter(between(wavelength, 400, 700)) %>%
  mutate(simulation_type = ifelse(str_detect(file_name, "NoFluo"), "without_raman", "with_raman"))

df <- df %>%
  select(file_name, simulation_type, wavelength, depth, ed, lu)

rm(list = ls(pattern = "with"))

# Effect of scattering ----------------------------------------------------

df

res <- df %>%
  filter(simulation_type == "with_raman") %>%
  group_by(file_name, simulation_type, wavelength) %>%
  arrange(depth) %>%
  mutate(ed2 = lead(ed)) %>%
  mutate(lu2 = lead(lu)) %>%
  mutate(depth2 = lead(depth)) %>%
  mutate(ked = (log(ed) - log(ed2)) / (depth2 - depth)) %>%
  mutate(klu = (log(lu) - log(lu2)) / (depth2 - depth)) %>%
  drop_na()

p1 <- res %>%
  ggplot(aes(y = ked, x = klu, color = factor(wavelength))) +
  geom_point() +
  geom_abline(lty = 2) +
  theme(legend.position = "none")

## Models to predict ked from klu

pp <- res %>%
  ggplot(aes(y = ked, x = klu)) +
  geom_point() +
  geom_abline(lty = 2) +
  facet_wrap(~wavelength, scales = "free") +
  geom_smooth(method = "lm")


res <- res %>%
  group_by(wavelength) %>%
  nest() %>%
  mutate(mod = map(data, ~ lm(ked ~ klu, data = .))) %>%
  mutate(ked_pred = map2(data, mod, modelr::add_predictions, var = "ked_pred")) %>%
  unnest(ked_pred)

## Now calculat PAR from true Ked and predicted Ked

res <- res %>%
  group_by(wavelength) %>%
  nest()

## Take Ed at maximum depth and propagate it to surface using true Ked and
## predicted Ked.
propagate <- function(df) {

  # df <- res$data[[1]]

  max_depth <- max(df$depth2)
  e_start <- df$ed2[df$depth2 == max_depth]
  e_start2 <- e_start

  e_pred_true_ked <- vector(mode = "numeric", length = nrow(df))
  e_pred_estimated_ked <- vector(mode = "numeric", length = nrow(df))

  for (i in 30:1) {
    e_pred_true_ked[i] <- e_start * exp(-df$ked[i] * (df$depth[i] - df$depth2[i]))
    e_start <- e_pred_true_ked[i]

    e_pred_estimated_ked[i] <- e_start2 * exp(-df$ked_pred[i] * (df$depth[i] - df$depth2[i]))
    e_start2 <- e_pred_estimated_ked[i]
  }

  e_pred_true_ked

  df <- df %>%
    mutate(e_pred_true_ked, e_pred_estimated_ked)
}

res2 <- res %>%
  mutate(pred = map(data, propagate)) %>%
  unnest(pred)

## Calculate PAR
res2 <- res2 %>%
  group_by(depth) %>%
  nest() %>%
  mutate(res = map(data, function(x) {
    par_true <- pracma::trapz(x$wavelength, y = x$e_pred_true_ked)
    par_estimated <- pracma::trapz(x$wavelength, y = x$e_pred_estimated_ked)

    tibble(par_true, par_estimated)
  })) %>%
  unnest(res)

p2 <- res2 %>%
  ggplot(aes(y = depth)) +
  geom_line(aes(x = par_true, color = "True PAR")) +
  geom_line(aes(x = par_estimated, color = "Estimated PAR")) +
  scale_y_reverse() +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  xlab("PAR")

p3 <- res2 %>%
  ggplot(aes(x = par_true, y = par_estimated)) +
  geom_point() +
  geom_abline(lty = 2) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "lb")

p4 <- res2 %>%
  select(-data) %>%
  gather(type, par, -depth) %>%
  ggplot(aes(x = type, y = par, fill = type)) +
  geom_boxplot() +
  scale_y_log10() +
  annotation_logticks(sides = "l")

p <- cowplot::plot_grid(p1, p2, p3, p4, labels = "AUTO")

ggsave("graphs/effect_scattering_par.pdf", device = cairo_pdf, width = 10)


# Absolute difference -----------------------------------------------------

p1 <- res2 %>% 
  unnest(data) %>% 
  mutate(ed_difference = e_pred_estimated_ked - e_pred_true_ked) %>%  
  ggplot(aes(x = wavelength, y = ed_difference)) +
  geom_line() +
  facet_wrap(~depth, scales = "free") +
  labs(title = "Absolute difference between Ed_estimated - Ed_true (variable Y scale)",
       subtitle = "Each plot corresponds to a specific depth")

p2 <- res2 %>% 
  unnest(data) %>% 
  mutate(ed_difference = e_pred_estimated_ked - e_pred_true_ked) %>%  
  ggplot(aes(x = wavelength, y = ed_difference)) +
  geom_line() +
  facet_wrap(~depth) +
  labs(title = "Absolute difference between Ed_estimated - Ed_true (fixed Y scale)",
       subtitle = "Each plot corresponds to a specific depth")

p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv", labels = "AUTO")

ggsave("graphs/effect_scattering_absolute_difference.pdf", device = cairo_pdf, width = 16, height = 16)
