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
library(minpack.lm)
library(ggpmisc)

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

# Figures -----------------------------------------------------------------

source("R/fig1.R")
source("R/fig2.R")
source("R/fig3.R")

source("R/supp_fig_1.R")
source("R/supp_fig_2.R")

# Frey 2011 ---------------------------------------------------------------

## Fit COPS data
# source("R/model_profils.R")

# source("R/simulate_light_profils.R")
# source("R/test_isotropy.R")