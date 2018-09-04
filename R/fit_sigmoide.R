rm(list = ls())

# https://stackoverflow.com/questions/33033176/using-r-to-fit-a-sigmoidal-curve
# https://stackoverflow.com/questions/48153158/fit-sigmoidal-curve-to-data-in-r

simulo <- read_feather("data/clean/simulo/compute-canada/simulo_4_lambertian_sources.feather") %>% 
  filter(between(pixel_distance_to_center, 0, 50))

simulo <- simulo %>%
  mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
  separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
  mutate_at(vars(start_distance, end_distance), parse_number) %>%
  mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
  group_by(depth, source, mid_distance) %>%
  summarise(value = mean(value)) %>%
  ungroup()

simulo <- simulo %>% 
  filter(source == "radiance" & depth == 0.5)

fit_sigmoid <- function(df, depth) {
  
  print(unique(depth))
  
  minpack.lm::nlsLM(
    value ~ a / (1 + exp(-b * (mid_distance - c))) + d,
    data = df,
    start = list(
      a = max(df$value),
      b = -1,
      c = 5,
      d = min(df$value)
    ), control = nls.lm.control(maxiter = 1024)
  )
}

res <- simulo %>%
  group_by(source, depth) %>%
  nest() %>%
  mutate(mod = map2(data, depth, fit_sigmoid)) %>%
  mutate(mod_gam = map(data,  ~gam(.$value ~ s(.$mid_distance, fx = FALSE, k=10, bs = "cr")) , data = .)) %>% 
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>% 
  mutate(pred2 = map(mod_gam, predict)) %>% 
  unnest(pred, pred2)

simulo %>% 
  ggplot(aes(x = mid_distance, y = value)) +
  geom_line() +
  geom_line(data = res, aes(x = mid_distance, y = pred), color = "red") +
  geom_line(data = res, aes(x = mid_distance, y = pred2), color = "blue")


# a <- 10
# b <- -0.5
# c <- 15
# d <- 0
# 
# # -22.326 112.359   2.901  78.958 
# 
# x <- 0:50
# y <- a / (1 + exp(-b * (x - c))) + d
# 
# plot(x, y, type = "o")
# 
