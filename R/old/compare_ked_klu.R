# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Using GE2016 IC data, this script use klu to reconstruct the edz profil.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


# Prepare data ------------------------------------------------------------

rm(list = ls())

## Return the attenuation coefficient k
extract_k <- function(model) { return(coef(model)["k"]) }

## Only use 2016 data since it includes both edz and luz data
cops <- read_rds("data/clean/cops_ked_keu_klu_models.rds") %>% 
  filter(str_detect(profile_filename, "GE2016"))

dat <- cops %>% 
  select(profile_filename:data) %>% 
  spread(type, data)

mod <- cops %>% 
  select(profile_filename:wavelength, mod_k) %>% 
  spread(type, mod_k) %>% 
  rename(model_edz = edz, model_luz = luz)

## Add predictions and extract k
df <- inner_join(dat, mod, by = c("profile_filename", "wavelength")) %>% 
  mutate(pred_edz = map2(edz, model_edz, modelr::add_predictions)) %>%
  mutate(pred_luz = map2(luz, model_luz, modelr::add_predictions)) %>% 
  mutate(ked = map_dbl(model_edz, extract_k)) %>% 
  mutate(klu = map_dbl(model_luz, extract_k))

# Model between klu and ked -----------------------------------------------

# df %>% 
#   select(profile_filename, wavelength) %>% 
#   count(wavelength)

## 86 COPS profiles made in 2016

# df %>% 
#   unnest(pred_luz) %>% 
#   filter(lubridate::year(posixct_date_utc) == 2016) %>% 
#   distinct(profile_filename)

df %>% 
  ggplot(aes(x = klu, y = ked)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(~wavelength) +
  xlab(bquote(K[lu]~(m^{-1}))) +
  ylab(bquote(K[ed]~(m^{-1}))) +
  labs(title = "Ked vs Klu",
       subtitle = "Data from GE2016 (ice camp). Each regression is based on 86 observations.") +
  geom_smooth(method = "lm")

ggsave("graphs/cops_kd_ku.pdf", device = cairo_pdf)


# Effect of high wavelength -----------------------------------------------

## Normalize by spectra maximum
dat <- df %>% 
  # filter(profile_filename == "GE2016.ICMP_ICEP_160624_CAST_007") %>% 
  unnest(pred_edz, pred_luz) %>% 
  select(profile_filename, depth, wavelength, edz = value, pred_edz = pred, luz = value1, pred_luz = pred1) %>% 
  group_by(profile_filename, wavelength) %>% 
  mutate_at(.vars = vars(edz:pred_luz), function(x) x / max(x))
  
plot_data <- function(df, profile_filename) {
  
  df %>%
    ggplot(aes(y = depth)) +
    geom_point(aes(x = luz)) +
    geom_path(aes(x = pred_luz, color = "luz")) +
    geom_point(aes(x = edz)) +
    geom_path(aes(x = pred_edz, color = "edz")) +
    scale_y_reverse() +
    facet_wrap(~wavelength, scales = "free") +
    labs(title = profile_filename,
         subtitle = "Data have been normalized to their maximum (range is now between 0-1 for both edz and luz data).") +
    xlab("Light") +
    theme(legend.title = element_blank())
  
}

dat <- dat %>% 
  group_by(profile_filename) %>% 
  nest() %>% 
  mutate(p = map2(data, profile_filename, plot_data))
  

pdf("graphs/normalized_ked_vs_klu.pdf", width = 9.53, height = 7.24)
dat$p
dev.off()

embed_fonts("graphs/normalized_ked_vs_klu.pdf")