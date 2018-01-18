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
library(data.table)

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# Process COPS ------------------------------------------------------------

source("R/cops/process_cops.R")
source("R/cops/plot_cops.R")
source("R/cops/calculate_k.R")

# Process IOPs ------------------------------------------------------------

source("R/iop/process_iop.R")
source("R/iop/interpolate_iop.R")
source("R/iop/iop2ked.R")
source("R/iop/process_mu.R")

# Analysis ----------------------------------------------------------------

# TODO

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