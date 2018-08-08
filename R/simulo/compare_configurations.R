# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Compare SImulO configurations to find out why there are light peaks around the
# center of the melt pond in Fig. 5.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

extract_simulo <- function(file) {
  
  simulo <- read_feather(file)
  
  simulo <- simulo %>%
    mutate(source = ifelse(source == "radiance", "Radiance (Lu)", "Irradiance (Ed)")) %>%
    filter(pixel_distance_to_center <= 50) ## il ne faut pas représenter les profils au delà de 50m du centre car on est alors soumis aux effets de bord.
  
  simulo <- simulo %>%
    mutate(iso_distance = cut_width(pixel_distance_to_center, width = 0.1)) %>%
    separate(iso_distance, into = c("start_distance", "end_distance"), sep = ",") %>%
    mutate_at(vars(start_distance, end_distance), parse_number) %>%
    mutate(mid_distance = start_distance + (end_distance - start_distance) / 2) %>%
    group_by(depth, source, mid_distance) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>% 
    mutate(file_name = tools::file_path_sans_ext(basename(file))) %>% 
    group_by(source) %>% 
    filter(depth == min(depth)) %>% 
    mutate(value = value / max(value)) %>% 
    ungroup()
  
  return(simulo)
  
}

files <- list.files("data/clean/simulo/compute-canada/", ".feather", full.names = TRUE, recursive = TRUE)

df <- map(files, extract_simulo) %>% 
  bind_rows()
  
df %>% 
  ggplot(aes(x = mid_distance, y = value)) +
  geom_line() +
  facet_grid(source ~ file_name, scales = "free") +
  ylab("Normalized number of photon") +
  xlab("Distance from the melt pond") +
  labs(subtitle = "Depth at 0.5 meter except for the simulation without melt pond which is at 1 meter.")

ggsave("graphs/simulo/simulo_compare_configurations.pdf", device = cairo_pdf, width = 12, height = 5)
