################################################################################
#### R code to clean trawl survey for Gulf of St. Lawrence North
#### Public data Ocean Adapt
#### Contacts: Government of Canada; Fisheries and Oceans Canada	
#####gddaiss-dmsaisb.XLAU@dfo-mpo.gc.ca
#### Coding: Michelle Stuart, Dan Forrest, Zoë Kitchel November 2021
################################################################################
#NB: there are multiple events at similar locations on the same day because 
#there is more than one vessel sampling, keep an eye on vessel name and haul_id
#--------------------------------------------------------------------------------------#
#### LOAD LIBRARIES AND FUNCTIONS ####
#--------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(googledrive)
library(taxize) # for getting correct species names
library(magrittr) # for names wrangling
library(readr)
library(dplyr)
library(PBSmapping)
library(readxl)

source("functions/clean_taxa.R")
source("functions/write_clean_data.R")
fishglob_data_columns <- read_excel("standard_formats/fishglob_data_columns.xlsx")

#Data for the Gulf of St. Lawrence North can be accessed using the public Pinsky 
#Lab OceanAdapt Git Hub Repository.
#Contact malin.pinsky@rutgers.edu for questions or help accessing

#Note that there have been gear changes and required calibrations
#for GSL-N
#and described well in here: 
#Bourdages, H., Brassard, C., Desgagnés, M., Galbraith, P., Gauthier, J., Lambert, J., Légaré,
#B., Parent, E. and Schwab P. 2015. Preliminary results from the groundfish and shrimp
#multidisciplinary survey in August 2014 in the Estuary and northern Gulf of St. Lawrence.
#DFO Can. Sci. Advis. Sec. Res. Doc. 2014/115. v + 96 p.
#The analysis of 2014 abundance and biomass data were integrated into the combined
#annual summer survey series initiated in 1990. This combined series was developed
#following a comparative study between the two vessel-gear tandems (1990-2005: CCGS
#Alfred Needler – URI 81’/114’ trawl; 2004-2012: CCGS Teleost – Campelen 1800 trawl) to
#establish specific correction factors for about twenty species caught (Bourdages et al.
#2007). This resulted in adjustment of Needler catches into Teleost equivalent catches.
#Note that the distinction between the two redfish species, Sebastes fasciatus and S.
#mentella, is based on the analysis of the soft anal fin rays count and the depth of capture
#of individuals (H. Bourdages, DFO Mont-Joli, pers. comm.).

#--------------------------------------------------------------------------------------#
#### PULL IN AND EDIT RAW DATA FILES ####
#--------------------------------------------------------------------------------------#

#GSL North Sentinel

GSLnor_sent <- read.csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_sentinel.csv")

#GSL North Gadus

GSLnor_gad <- read.csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_gadus.csv")

#GSL North Hammond

GSLnor_ham <- read.csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_hammond.csv")

#GSL North Needler

GSLnor_need <- read.csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_needler.csv")

#GSL North Teleost

GSLnor_tel <- read.csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_teleost.csv")

#--------------------------------------------------------------------------------------#
#### REFORMAT AND MERGE DATA FILES ####
#--------------------------------------------------------------------------------------#

#Bind all datasets

GSLnor <- plyr::rbind.fill(GSLnor_sent, GSLnor_gad, GSLnor_ham, GSLnor_need, GSLnor_tel)
GSLnor$lat <-as.numeric(as.character(GSLnor$Latit_Deb))
GSLnor$lon <-as.numeric(as.character(GSLnor$Longit_Deb))
GSLnor$depth <-as.numeric(as.character(GSLnor$Prof_Max))
GSLnor$Dist_Chalute_Position <-as.numeric(as.character(GSLnor$Dist_Chalute_Position))
GSLnor$Pds_Capture <- as.double(GSLnor$Pds_Capture)
GSLnor$Nb_Ind_Capture <- as.numeric(as.character(GSLnor$Nb_Ind_Capture))
GSLnor$Date <-as.Date(GSLnor$Date_Deb_Trait)
GSLnor$year <- as.integer(year(GSLnor$Date))
GSLnor$verbatim_name <- trimws(as.character(GSLnor$Nom_Scient_Esp), which = "right")



GSLnor <- GSLnor[!is.na(GSLnor$lat),] #only keep rows with latitude
GSLnor <- GSLnor[!is.na(GSLnor$depth),] #only keep rows with depth

GSLnor <- GSLnor %>%
  # Create a unique haul_id
  mutate(
    haul_id = paste(GSLnor$Nom_Navire, GSLnor$No_Releve,GSLnor$Trait,
                    GSLnor$Date_Deb_Trait,GSLnor$Hre_Deb, sep="-"),
    #area in km^2 = 
#Dist_Chalute_Position (nautical miles) * 1852 m/1 nautical mile * 
#                                               trawl width *(1km^2/1000000m^2)
    area_swept = Dist_Chalute_Position * 1852 * 12.497 *(1/1000000),
    wgt = Pds_Capture, #in kg
    num = Nb_Ind_Capture, #in pieces
    # (via Daniel Ricard) trawl width, 12.497 m. Hurlbut and Clay (1990)
    # catch weight (kg.) per tow	/km^2,
    wgt_cpue = (Pds_Capture)/area_swept,
    #weight in kg/time in minutes*60minutes/1hour
    wgt_h = (Pds_Capture)/Duree*60,
    #abundance in number/km^2
    num_cpue = Nb_Ind_Capture/area_swept,
    #abundance in number/hour
    num_h = Nb_Ind_Capture/Duree*60,
  )

GSLnor <- GSLnor %>%
  filter(
    # remove unidentified spp and non-species
    verbatim_name != "" | !is.na(verbatim_name), 
    !grepl("EGG", verbatim_name), 
    !grepl("UNIDENTIFIED", verbatim_name)) %>%
  # add survey column
  mutate(survey = "GSL-N")

#check that the number of unique haul_ids * 
#                     spp combinations is the same as the number of rows in mar
nrow(GSLnor) == nrow(unique(GSLnor[,c("haul_id","verbatim_name")]))

#it's not, so let's see why we have extras
#which(duplicated(GSLnor[,c("haul_id","verbatim_name")]))

GSLnor <- GSLnor %>% 
  # Adding extra columns and setting proper format
  mutate(
    country = "Canada",
    sub_area = NA,
    continent = "n_america",
    stat_rec = NA,
    station = NA,
    stratum = NA,
    season = NA,
    latitude = lat,
    longitude = lon,
    month = month(Date),
    day = day(Date),
    haul_dur = ifelse(Duree > 0, Duree/60, NA), 
    #get rid of negative duration values and code them as NA
    gear = Engin,
    sbt = NA,
    sst = NA,
    quarter = ifelse(month %in% c(1,2,3),1,
                     ifelse(month %in% c(4,5,6),2,
                            ifelse(month %in% c(7,8,9),3,
                                   4
                            )
                     )
    ),
    aphia_id = NA,
    verbatim_aphia_id = NA,
  ) %>%
  select(survey, haul_id, country, sub_area, continent, stat_rec, station, stratum,
         year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept,
         gear, depth, sbt, sst, verbatim_name, num, num_h, num_cpue,
         wgt, wgt_h, wgt_cpue, verbatim_name, verbatim_aphia_id)

#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA FROM TAXA ANALYSIS ####
#--------------------------------------------------------------------------------------#

# Get WoRM's id for sourcing
wrm <- gnr_datasources() %>% 
  filter(title == "World Register of Marine Species") %>% 
  pull(id)

### Automatic cleaning
# Set Survey code
GSLnor_survey_code <- "GSL-N"

GSLnor <- GSLnor %>%
  mutate(
    taxa2 = str_squish(verbatim_name),
    taxa2 = str_remove_all(taxa2," spp.| sp.| spp| sp|NO "),
    taxa2 = str_to_sentence(str_to_lower(taxa2))
  )

# Get clean taxa
clean_auto <- clean_taxa(unique(GSLnor$taxa2),
                         input_survey = GSLnor_survey_code, save = F, output=NA,
                         fishbase=T)

#This leaves out the following species, all of which are inverts
#Eualus gaimardii belcheri (invert)

#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA in GSL-North survey data ####
#--------------------------------------------------------------------------------------#

correct_taxa <- clean_auto %>% 
  select(-survey)

clean_GSLnor <- left_join(GSLnor, correct_taxa, by=c("taxa2"="query")) %>% 
  filter(!is.na(taxa)) %>% # query does not indicate taxa entry that were 
  #removed in the cleaning procedure
  # so all NA taxa have to be removed from the surveys because: non-existing, 
  #non marine or non fish
  rename(accepted_name = taxa,
         aphia_id = worms_id) %>% 
  mutate(verbatim_aphia_id = NA,
         source = "DFO",
         timestamp = "2021") %>% 
  select(fishglob_data_columns$`Column name fishglob`)


# -------------------------------------------------------------------------------------#
#### SAVE DATABASE IN GOOGLE DRIVE ####
# -------------------------------------------------------------------------------------#

# Just run this routine should be good for all
write_clean_data(data = clean_GSLnor, survey = "GSL-N_v2", overwrite = T)

