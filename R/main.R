#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(extrafont)
library(tidyverse)
library(feather)

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# Process COPS ------------------------------------------------------------

source("R/process_cops.R")
source("R/plot_cops.R")

# Analysis ----------------------------------------------------------------

source("R/calculate_k.R")
source("R/compare_ked_klu.R")

# Frey 2011 ---------------------------------------------------------------

## Fit COPS data
# source("R/model_profils.R")

# source("R/simulate_light_profils.R")
# source("R/test_isotropy.R")