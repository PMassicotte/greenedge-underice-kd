# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Plot COPS data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_csv("data/clean/cops.csv")  %>% 
  mutate(year = lubridate::year(posixct_date_utc)) %>% 
  filter(year == 2015)

plot_cops <- function(df, date) {
  
  df <- df %>% 
    select(hole_type, depth, par_d_p24h_ein_m_2_day_1, par_u_p24h_ein_m_2_day_1) %>% 
    gather(par_type, p24h_ein_m_2_day_1, -depth, -hole_type) %>% 
    drop_na(p24h_ein_m_2_day_1)

  df %>% 
    ggplot(aes(x = p24h_ein_m_2_day_1, y = depth)) +
    geom_path() +
    facet_wrap(~par_type, scales = "free_x") +
    scale_y_reverse() +
    labs(title = date,
         subtitle = sprintf("Hole type: %s", unique(df$hole_type)))
      
}


# All the data ------------------------------------------------------------

res <- cops %>% 
  group_by(profile_filename, posixct_date_utc) %>% 
  nest() %>% 
  mutate(p = map2(data, posixct_date_utc, plot_cops))

## Plot all profils
pdf("graphs/cops.pdf", height = 4, width = 8)
res$p
dev.off()

embed_fonts("graphs/cops.pdf")


# Plot only HS ------------------------------------------------------------

res <- cops %>% 
  filter(hole_type == "H") %>% 
  group_by(profile_filename, posixct_date_utc) %>% 
  nest() %>% 
  mutate(p = map2(data, posixct_date_utc, plot_cops))

## Plot all profils
pdf("graphs/cops_high_snow.pdf", height = 4, width = 8)
res$p
dev.off()

embed_fonts("graphs/cops_high_snow.pdf")
