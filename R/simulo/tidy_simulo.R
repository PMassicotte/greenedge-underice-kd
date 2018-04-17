tidy_simulo <- function(simulo_data) {
  # simulo_data <- DataS

  nx <- dim(simulo_data$detectors[[1]])[1]
  ny <- dim(simulo_data$detectors[[1]])[2]

  pixel <- expand.grid(x = 1:nx, y = 1:ny)

  ## Tidy simulo data into a dataframe
  res <- map(simulo_data$detectors, matrix_to_dataframe, pixel = pixel) %>%
    bind_rows(.id = "source") %>%
    as_tibble()

  ## Metadata
  res <- res %>%
    separate(source, into = c("depth", "f", "source"), sep = "_") %>%
    mutate(depth = parse_number(depth)) %>%
    mutate_at(.vars = vars(f, source), tolower)

  ## For each pixel, calculate its distance from the center (125, 125). The 0,0
  ## is the lower left pixel.
  res <- res %>%
    mutate(pixel_distance_to_center = sqrt((x - nx / 2)^2 + (y - ny / 2)^2))

  return(res)
}

matrix_to_dataframe <- function(m, pixel) {
  df <- pixel %>%
    cbind(value = as.vector(m))

  return(df)
}
