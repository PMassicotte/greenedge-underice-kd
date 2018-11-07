# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Angular distribution (Simon's data)
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df1 <- readxl::read_excel("data/raw/angular_distribution.xlsx", n_max = 2, na = "NaN") %>% 
  janitor::remove_empty() %>% 
  gather(angle, value) %>% 
  set_names(c("angle", "Measured under-ice\ndownward radiance distribution")) %>% 
  mutate_all(parse_number)

df2 <- readxl::read_excel("data/raw/angular_distribution.xlsx", skip = 4, n_max = 2) %>% 
  janitor::remove_empty() %>% 
  gather(angle, value) %>% 
  set_names(c("angle", "Emitting source chosen\nfor the simulation")) %>% 
  mutate_all(parse_number)

df <- bind_rows(df1, df2) %>% 
  gather(source, value, -angle) %>% 
  drop_na()

df %>% 
  ggplot(aes(x = angle, y = value, color = source)) +
  geom_path() +
  xlab(expression("Azimuthal angle"~"("^degree*")")) +
  ylab("Normalized raidance") +
  theme(legend.title = element_blank()) +
  theme(legend.justification = c(0, 0)) +
  theme(legend.position = c(0.01, 0.01)) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme(legend.key.height = unit(1, "cm"))

ggsave("graphs/fig2.pdf", device = cairo_pdf, width = 3 * 1.61803398875, height = 3)
