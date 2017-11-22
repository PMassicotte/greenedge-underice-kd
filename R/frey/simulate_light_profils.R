# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Randomly simulate PAR profils under heterogenious snow/ice surface. For this,
# I use parameter estimates calulated in "model_profile.R". Then, I use the mean
# and the sd to generate n number of light profils.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

set.seed(123)

params <- read_csv("data/clean/high_snow_geometric_model_parameters.csv")

p <- params %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap( ~ parameter, scales = "free") +
  labs(title = "Histograms of parameters estimated from 60 COPS measurements")

ggsave("graphs/insitu_geometric_parameter_histogram.pdf", height = 6)

## Calculate log of the mean and sd
params %>% 
  group_by(parameter) %>% 
  summarise(mean_param = mean(log(value)), sd_param = sd(log(value)))

## Generate 1000 random values for each paramater. Here I choosed log-normal
## since params can't be negative.
n_simul <- 1000

res <- params %>% 
  group_by(parameter) %>% 
  summarise(mean_param = mean(log(value)), sd_param = sd(log(value))) %>% 
  purrrlyr::by_row(function(df) {rlnorm(n_simul, df$mean_param, df$sd_param)}, .to = "pred") %>% 
  unnest(pred) %>% 
  mutate(simulation_id = rep(1:n_simul, times = 5))

p <- res %>% 
  ggplot(aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = "free") +
  labs(title = "Histograms of parameters from 1000 simulations using a log-normal distribution")

ggsave("graphs/simulated_geometric_parameter_histogram.pdf", height = 6)

## Generate profils based on the random parameters
simulation <- res %>% 
  select(simulation_id, parameter, pred) %>% 
  spread(parameter, pred) %>% 
  group_by(simulation_id) %>% 
  nest() %>% 
  mutate(profils = map(data, function(params) {
    
    x <- seq(0, 100, by = 0.25)
    y <- with(params, pi * i * (1 + p * (n - 1) * cos(atan(r / x)) ^ 2) * exp(-k * x))
    
    res <- data.frame(depth = x, par24h = y)
    
  }))

## Save simulated data
unnest(simulation, data) %>% 
  unnest(profils) %>% 
  write_csv("data/clean/simulations_frey.csv")

mean_simulation <- simulation %>%
  unnest(profils) %>%
  group_by(depth) %>%
  summarise(mean_par24h = mean(par24h),
            sd_par24h = sd(par24h),
            cv_par24h = (sd_par24h / mean_par24h) * 100,
            iqr_par24h = IQR(par24h))

p <- simulation %>%
  unnest(profils) %>%
  ggplot(aes(x = par24h, y = depth, group = simulation_id)) +
  geom_path(alpha = 0.25) +
  scale_y_reverse() +
  geom_path(
    data = mean_simulation,
    aes(x = mean_par24h, y = depth, color = "Mean profil"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Results of the 1000 randomly generated PAR profils",
    subtitle = str_wrap(
      "These 1000 profils can be viewed as how the light is transmitted in the water column under 1000 different scenarios of snow/ice cover heterogeneity. All profils seem to converge around 80."
    )
  ) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.9, 0.1), legend.justification = c(1, 0))

ggsave("graphs/simulated_par_profils.pdf")


# Sub-surface maximal light -----------------------------------------------

## Check at which depth the maximal value of PAR is reached

ssm <- simulation %>% 
  unnest(profils) %>% 
  group_by(simulation_id) %>% 
  filter(par24h == max(par24h))

ssm %>% 
  ggplot(aes(x = depth)) +
  geom_histogram()

mean(ssm$depth)

res <- ssm %>% 
  ungroup() %>% 
  mutate(bin = cut(depth, seq(0, 100, by = 0.25), include.lowest = TRUE)) %>% 
  count(bin) %>%
  separate(bin, into = c("from", "to"), convert = TRUE, sep = ",") %>% 
  mutate_at(vars(from:to), parse_number)

res %>% 
  mutate(percent = n / sum(n)) %>% 
  ggplot(aes(x = from, y = percent)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent)
