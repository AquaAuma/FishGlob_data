#######################################################################################################################################
#### R code to standardize spatio-temporal and taxa per survey
#### Coding: Laura Mannocci (laura.mannocci@fondationbiodiversite.fr) and Nicolas Casajus (nicolas.casajus@fondationbiodiversite.fr) 
#### Updated December 2022 by Aurore
#######################################################################################################################################

#########   Preliminary steps

#install required packages that are not already installed
required_packages <- c("data.table",
                       "devtools",
                       "dggridR",
                       "dplyr",
                       "fields",
                       "forcats",
                       "ggplot2",
                       "here",
                       "magrittr",
                       "maps",
                       "maptools",
                       "raster",
                       "rcompendium",
                       "readr",
                       "remotes",
                       "rrtools",
                       "sf",
                       "sp",
                       "tidyr",
                       "usethis")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)


#load pipe operator
library(magrittr)


#load all functions
source(here::here("standardization_steps", "functions.R"))



#########   Read FISHGLOB data
# function from Laura M.
dat <- read_clean_data()



#########   Apply taxonomic flagging per region

#get vector of regions (here the survey column)
regions <- levels(as.factor(dat$survey))

#run flag_spp function in a loop
for (r in regions) {
  flag_spp(dat, r)
}


######### Apply trimming per survey_unit method 1
#apply trimming for hex size 7
# removed one haul without long lats because otherwise the function does not work!
dat_new_method1_hex7 <- apply_trimming_per_survey_unit_method1(dat, 7)

#apply trimming for hex size 8
dat_new_method1_hex8 <- apply_trimming_per_survey_unit_method1(dat, 8)


######### Apply trimming per survey_unit method 2
dat_new_method2 <- apply_trimming_per_survey_unit_method2(dat)


