#' Change from xy plane to polar projection (circle)
#'
#' @param simulo Data frame returned by tidy_simulo
#' @param min_distance Min distance to use
#' @param max_distance Max distance to use
#'
#' @return
#' @export
#'
#' @examples
simulo_xy_to_polar <- function(simulo, min_distance, max_distance) {
  
  # simulo <- read_feather("data/clean/simulo/compute-canada/simulations-with-15m-melt-pond-4-lambertian-sources.feather")
  
  simulo <- simulo %>%
    mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
    filter(pixel_distance_to_center <= max_distance) %>% ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.
    filter(pixel_distance_to_center >= min_distance) %>% ## Ne pas prendre les données entre 0-10 metres, car pas assez de pixels
    mutate(pixel_distance_to_center = pixel_distance_to_center - min_distance)
  
  simulo <- simulo %>%
    mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
    separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",", remove = F) %>%
    mutate_at(vars(start_distance, end_distance), parse_number) %>%
    mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>% 
    group_by(depth, source, mid_distance) %>%
    summarise(value = mean(value), n = n()) %>%
    ungroup()
  
  # ## How many pixels in each bin
  # simulo %>% 
  #   ggplot(aes(x = mid_distance, y = n)) +
  #   geom_bar(stat = "identity") +
  #   labs(title = "Number of pixel by distance") +
  #   # geom_text(aes(label = n), vjust = -1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   facet_wrap(~source)
  # 
  # ## Check the number of photons over the de horizontal distance
  # simulo %>% 
  #   filter(depth %in% c(0.5, 2.5, 5, 10, 15)) %>%
  #   group_by(depth, source) %>% 
  #   mutate(value = value / max(value)) %>% 
  #   ggplot(aes(x = mid_distance, y = value)) +
  #   geom_line() +
  #   facet_grid(depth~source, scales = "free") +
  #   ylab("Number of photon") +
  #   xlab("Distance from the melt pond") 
  
  
  return(simulo)
  
}

smooth_pixels <- function(simulo) {
  
  simulo <- simulo %>% 
    group_by(depth, source) %>% 
    nest() %>% 
    mutate(s = map(data, function(x) {
      
      sf <- smooth.spline(x$mid_distance, x$value, spar = 0.1)
      tibble(smoothed_value = sf$y)
      
    })) %>% 
    unnest(data, s)
  
  return(simulo)
  
}
