################################################################################
#### R code to merge all separate datasets
#### Coding: Juliano Palacios Abrantes & Aurore A. Maureaud, November 2023
################################################################################

# Load libraries
library(googledrive)
library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(readxl)
library(data.table)
library(R.utils)

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
             "SP-ARSA",
             "SP-NORTH",
             "SP-PORC",
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
# MLP 8 Dec 2025: had to delete outputs/Compiled_data/FishGlob_public_clean.csv.gz before running the next line
write_clean_data(data = fishglob, survey = "FishGlob_public",
                 overwrite = T, compiled = TRUE, gzip = TRUE)


#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#

fishglob_std <- read_clean_data(surveys, std = TRUE)

# Just run this routine should be good for all
# MLP 8 Dec 2025: had to delete outputs/Compiled_data/FishGlob_public_std_clean.csv.gz before running the next line
write_clean_data(data = fishglob_std, survey = "FishGlob_public_std",
                 overwrite = T, compiled = TRUE, gzip = TRUE)


#-------------------------------------------------------------------------------------------#
#### METADATA PRODUCT ####
#-------------------------------------------------------------------------------------------#

fishglob_public_metadata <- fishglob %>%
  select(survey, latitude, longitude, year, survey_unit) %>%
  distinct()

# MLP 8 Dec 2025: had to delete outputs/Compiled_data/FishGlob_public_metadata_clean.csv.gz before running the next line
write_clean_data(data = fishglob_public_metadata,
                 survey = "FishGlob_public_metadata",
                 overwrite = TRUE, compiled = TRUE, gzip = TRUE)
