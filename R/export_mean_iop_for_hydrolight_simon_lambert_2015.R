rm(list = ls())

iop <- read_feather("data/raw/iop/IOP.2015.AVG.MEAN.N.d.u.feather") %>%
  filter(up_down == "d") %>%
  filter(date %in% c("2015157", "2015159")) %>%
  janitor::clean_names() %>%
  select(date, depth_grid, starts_with("mean_a"), starts_with("mean_c")) %>%
  set_names(gsub("mean_", "", names(.))) %>%
  filter(depth_grid <= 80) %>%
  select(-contains("400"))

# iop <- iop %>%
#   mutate_at(.vars = vars(starts_with("c_")), .funs = function(x) {x + 0.5})

sum(is.na(iop))

write_hydrolight <- function(df, outfile) {
  write("AAAA", outfile)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)
  write("AAAA", outfile, append = TRUE)

  v <- (c(" ", length(seq(405, 740, by = 5)), seq(405, 740, by = 5)))
  write(v, outfile, append = TRUE, sep = "  ", ncolumns = length(v) + 1)

  ## Add ice at 1m. Set all value of C at 1m with 1000
  # df <- df %>%
  #   complete(depth_grid = c(1, unique(depth_grid))) %>%
  #   fill((starts_with("a_")), .direction = "up") %>%
  #   mutate_at(vars(starts_with("c_")), funs(if_else(is.na(.), 1000, .)))

  write.table(
    add_column(df, test = "", .before = 1),
    outfile,
    append = TRUE,
    col.names = FALSE,
    row.names = FALSE,
    sep = "   ",
    quote = FALSE
  )

  v <- c(-1, rep(0, times = (length(v) - 1)))
  write(v, outfile, append = TRUE, ncolumns = length(v), sep = "   ")
}

iop %>%
  filter(date == "2015157") %>%
  select(-date) %>%
  write_hydrolight("data/clean/averaged_a_c_for_hydrolight_2015157.txt")

iop %>%
  filter(date == "2015159") %>%
  select(-date) %>%
  write_hydrolight("data/clean/averaged_a_c_for_hydrolight_2015159.txt")
