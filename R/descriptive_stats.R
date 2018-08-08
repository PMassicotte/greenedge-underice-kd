# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Various descriptive statistics for the paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Snow and ice thickness --------------------------------------------------

rm(list = ls())

file <- curl::curl_download(
  "http://greenedge:GE2015-16@www.obs-vlfr.fr/proof/ftpv/greenedge//db/DATA/2016/GALINDO_ICE_THICKNESS/GE16_IceSnow_thickness_Icecoring_Site.xlsx",
  "data/raw/snow_ice_thickness.xlsx"
)

snow_ice_thickness <- readxl::read_excel(file, skip = 2) %>% 
  janitor::clean_names(case = "old_janitor")

range(snow_ice_thickness$snow_cm)
range(snow_ice_thickness$ice_cm)


# COPS --------------------------------------------------------------------

rm(list = ls())

cops <- read_feather("data/clean/cops.feather") %>% 
  filter(lubridate::year(posixct_date_utc) == 2016)

unique(cops$profile_filename)
