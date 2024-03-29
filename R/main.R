#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(raster)
library(extrafont)
library(tidyverse)
library(feather)
library(minpack.lm)
library(ggpmisc)
library(data.table)
library(parallel)
library(pbapply)
library(sf)
library(ggsn)
library(modelr)
library(glue)
library(scales)
library(patchwork)

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "IBM Plex Sans Light"))

# Process AOPs ------------------------------------------------------------

## COPS
source("R/aop/process_cops.R")
source("R/aop/interpolate_cops_wavelengths.R")
source("R/aop/plot_cops.R")
source("R/aop/calculate_k_cops.R")

## Camlum
source("R/aop/process_mu_camlum.R")
source("R/aop/interpolate_camlum.R")

# Process IOPs ------------------------------------------------------------

source("R/iop/process_iop.R")
source("R/iop/interpolate_iop.R")
source("R/iop/iop2ked.R")

# Simulo ------------------------------------------------------------------

# source("R/simulo/process_simulo.R") ## Takes 2-3 hours
source("R/simulo/explore_simulations.R")
source("R/simulo/simulo_compare_ed_melt_pond_and_open_water.R")
source("R/simulo/simulo_propagate_light_by_distance.R")
rmarkdown::render("reports/simulo/explore_simulo.Rmd")

# Figures -----------------------------------------------------------------

source("R/fig1.R")
source("R/fig2.R")
source("R/fig3.R")
source("R/fig4.R")
source("R/fig5.R")
source("R/fig6.R")
source("R/fig7-10.R")

source("R/supp_fig_1.R") # Map
source("R/supp_fig_4-6.R") # Raman scattering


# Frey 2011 ---------------------------------------------------------------

## Fit COPS data
# source("R/model_profils.R")

# source("R/simulate_light_profils.R")
# source("R/test_isotropy.R")