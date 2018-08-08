file <- "http://greenedge:GE2015-16@www.obs-vlfr.fr/proof/ftpv/greenedge/db/DATA/2016/AP/GE2016_ap_spec_20170605_nulcor700_v1.out"
destfile <- tempfile(fileext = ".out")
curl::curl_download(file, destfile)


df <- read_table2(destfile, col_names = FALSE)
df <- melt(read.table(header = TRUE, text = do.call(paste, transpose(df))), 1:9, var = "wavelength", value.name = "ap") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate_at("wavelength", parse_number)

df <- df %>%
  filter(str_detect(type, "^SW")) %>%
  filter(between(wavelength, 400, 700))

df %>%
  filter(j_day == 153) %>%
  ggplot(aes(x = wavelength, y = ap)) +
  geom_line() +
  facet_grid(j_day ~ water_depth_m)

df %>%
  filter(wavelength %in% seq(400, 700, by = 20)) %>%
  group_by(wavelength) %>% 
  mutate(mean_wl = mean(ap, na.rm = TRUE)) %>% 
  ggplot(aes(x = ap, y = water_depth_m, group = j_day)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~ wavelength) +
  geom_vline(aes(xintercept = mean_wl), colour = "red")
