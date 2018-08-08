# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Compare our method with Laney's method.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

simulo <-
  read_feather("data/clean/simulo/compute-canada/simulo_45degrees.feather") %>%
  filter(pixel_distance_to_center <= 50)

res <- simulo %>%
  group_by(depth, source, pixel_distance_to_center) %>%
  summarise(value = mean(value)) %>%
  group_by(source, pixel_distance_to_center) %>%
  mutate(value = value / max(value))

res %>%
  ggplot(
    aes(
      x = value,
      y = depth,
      group = pixel_distance_to_center,
      text = pixel_distance_to_center
    )
  ) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ source, scales = "free")

# plotly::ggplotly()

df <- res %>%
  filter(pixel_distance_to_center == 6)

df %>%
  ggplot(
    aes(
      x = value,
      y = depth,
      group = pixel_distance_to_center,
      text = pixel_distance_to_center
    )
  ) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ source, scales = "free")


# Fit Laney model ---------------------------------------------------------

laney <- df %>%
  filter(source == "intensity") %>%
  group_by(source, pixel_distance_to_center) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      value ~ i0 * exp(-kd * depth) - (i0 - ins) * exp(-kns * depth),
      data = .,
      start = c(
        i0 = 2,
        kd = 0.02,
        kns = 0.0,
        ins = 1
      ),
      lower = c(
        i0 = 0,
        kd = 0.0001,
        kns = 0.1,
        ins = 0.01
      ),
      upper = c(
        i0 = 50,
        kd = 1,
        kns = 1,
        ins = 10
      )
    )
  )) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions))

laney %>%
  unnest(pred) %>%
  ggplot(
    aes(
      x = value,
      y = depth,
      group = pixel_distance_to_center,
      text = pixel_distance_to_center
    )
  ) +
  geom_path() +
  geom_path(aes(x = pred), color = "red") +
  scale_y_reverse() +
  facet_wrap(~ source, scales = "free") +
  geom_path(
    data = modelr::add_predictions(data.frame(depth = seq(0, 25, length.out = 100)), laney$mod[[1]]),
    aes(x = pred, y = depth),
    inherit.aes = FALSE,
    color = "orange"
  )
