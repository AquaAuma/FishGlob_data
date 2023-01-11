################################################################################
#### R code to merge all separate datasets
#### Coding: Juliano Palacios Abrantes & Aurore A. Maureaud, January 2023
################################################################################

rm(list=ls())

# Load libraries
library(googledrive)
library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(readxl)

# Load relevant functions
source(here("./functions/write_clean_data.r"))
source("./functions/read_clean_data.R")

#-------------------------------------------------------------------------------------------#
#### Read all processed survey files ####
#-------------------------------------------------------------------------------------------#
# Load cleaned surveys
surveys <- c("AI",
             "BITS",
             "EBS",
             "EVHOE",
             "FR-CGFS",
             "GMEX",
             "GOA",
             "GSL-N",
             "GSL-S",
             "HS",
             "IE-IGFS",
             "NEUS",
             "NIGFS",
             "NOR-BTS",
             "NS-IBTS",
             "PT-IBTS",
             "QCS",
             "ROCKALL",
             "SCS",
             "SWC-IBTS",
             "SOG",
             "SEUS",
             "WCANN",
             "WCHG",
             "WCTRI",
             "WCVI")

# create compiled dataset
fishglob <- read_clean_data(surveys)

# survey compiled survey product
write_clean_data(data = fishglob, survey = "FishGlob_public",
                 overwrite = T, compiled = TRUE)


#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#

fishglob_std <- read_clean_data(surveys, std = TRUE)
# Just run this routine should be good for all
write_clean_data(data = fishglob_std, survey = "FishGlob_public_std",
                 overwrite = T, compiled = TRUE)


#-------------------------------------------------------------------------------------------#
#### METADATA PRODUCT ####
#-------------------------------------------------------------------------------------------#

fishglob_public_metadata <- fishglob %>%
  select(survey, latitude, longitude, year, survey_unit) %>%
  distinct()

write_clean_data(data = fishglob_public_metadata,
                 survey = "FishGlob_public_metadata",
                 overwrite = TRUE, compiled = TRUE)
