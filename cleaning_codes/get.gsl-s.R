################################################################################
#### R code to clean trawl survey for Gulf of St. Lawrence South
#### Public data Ocean Adapt
#### Contacts: Government of Canada; Fisheries and Oceans Canada
####gddaiss-dmsaisb.XLAU@dfo-mpo.gc.ca
#### Coding: Michelle Stuart, Dan Forrest, Zoë Kitchel December 2022
################################################################################
#NB: there are multiple events at similar locations on the same day because there
#is more than one vessel sampling, keep an eye on vessel name and haul_id

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

source("functions/clean_taxa.R")
source("functions/write_clean_data.R")
fishglob_data_columns <- read_excel("standard_formats/fishglob_data_columns.xlsx")

########Types of gear: 
########"Western IIA trawl width 12.497m": 1987-2019; 0.041 km^2 in 30 minutes
#(avg trawl length)
########"Yankee #36 otter trawl width 10.668m" 1970-1986; 0.035 km^2 in 30 minutes
#(avg trawl length)
#Source: Page 11; https://waves-vagues.dfo-mpo.gc.ca/Library/115732.pdf

#Southern GSL -  
#  Trawl Distance, 1.75 nau mi. 30 minute tow at 3.5 knots (via Daniel Ricard)	

#We need to check to confirm that data we use are corrected for the gear change
#described above (Zoe, 22 Nov 2022)

#Data for the Gulf of St. Lawrence South can be accessed using the public 
#Pinsky Lab OceanAdapt Git Hub Repository.
#Contact malin.pinsky@rutgers.edu for questions or help accessing


#--------------------------------------------------------------------------------------#
#### PULL IN AND EDIT RAW DATA FILES ####
#--------------------------------------------------------------------------------------#


GSLsouth <- read_csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLsouth.csv")

GSLsouth$haul_id <- paste(GSLsouth$year,GSLsouth$month,GSLsouth$day,
                          GSLsouth$start.hour,GSLsouth$start.minute, GSLsouth$longitude,
                          GSLsouth$latitude, sep="-")

GSLsouth <- GSLsouth %>% 
  mutate(
    wgt = weight.caught,
    num = number.caught,
    sub_area = NA,
    depth = NA, #No depth data available - fill with NA
    station = NA,
    sst = NA,
    sbt = NA,
    season = NA_character_,
    haul_dur = 0.5, #hours
    area_swept = ifelse(gear.str == "Western IIA trawl", 0.041,
                        ifelse(
                          gear.str == "Yankee #36 otter trawl", 0.035, NA)), 
    #average swept area values from document above
    gear = gear.str,
    country = "Canada",
    continent = "n_america",
    stat_rec = NA,
    num_h = num/haul_dur,
    wgt_h = wgt/haul_dur,
    num_cpue = num/area_swept,
    wgt_cpue = wgt/area_swept,
    verbatim_name = latin.name,
    quarter = case_when(month %in% c(1,2,3) ~ 1,
                        month %in% c(4,5,6) ~ 2,
                        month %in% c(7,8,9) ~ 3,
                        month %in% c(10,11,12) ~ 4),
  )


GSLsouth <- GSLsouth %>%
  filter(
 #  remove unidentified spp and non-species
  verbatim_name != "" | !is.na(verbatim_name), 
  !grepl("EGG", verbatim_name), 
  !grepl("UNIDENTIFIED", verbatim_name)) %>%
  mutate(survey = "GSL-S",
         stratum = NA) %>% 
  # add survey column
  select(survey, haul_id, country, sub_area, continent, stat_rec, station,
         stratum, year, month, day, quarter, season, latitude, longitude, haul_dur,
         area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)

#check that the number of unique haul_ids * spp combinations is the same
#as the number of rows in mar
nrow(GSLsouth) == nrow(unique(GSLsouth[,c("haul_id","verbatim_name")]))

#it's not, so let's see why we have extras
which(duplicated(GSLsouth[,c("haul_id","verbatim_name")]))

#Haul_ID "1994-9-22-8-27--61.6333333333333-46.4166666666667," Gadus morhua
#has two separate observations
#I will delete second observation because it's only 0.486111111 kg and no count info

GSLsouth <- GSLsouth[-31057,] #be sure to only do once!

#try again
which(duplicated(GSLsouth[,c("haul_id","verbatim_name")])) #success!

#correcting order of columns and adding final column 
GSLsouth <- GSLsouth %>%
  mutate(verbatim_aphia_id = NA) %>%
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
GSLsouth_survey_code <- "GSL-S"

GSLsouth <- GSLsouth %>%
  mutate(
    taxa2 = str_squish(verbatim_name),
    taxa2 = str_remove_all(taxa2," spp.| sp.| spp| sp|NO "),
    taxa2 = str_to_sentence(str_to_lower(taxa2))
  )

# Get clean taxa
clean_auto <- clean_taxa(unique(GSLsouth$taxa2),
                         input_survey = GSLsouth_survey_code, save = F, output=NA)
#takes 1.8 minutes

#This leaves out the following species, all of which are inverts or 
#only ID to genus except for Cae cae
#Caelorinchus caelorinchus     (fish)                                                        
#Coelenterata                                                        
#Nereidae                                                            
#Rhynchocoela                                                        
#Lithothamnium                                        

cae_cae <- c("Caelorinchus caelorinchus", "398381", "1726",
             "Coelorinchus caelorhincus", "Animalia", "Chordata",
             "Actinopteri", "Gadiformes", "Macrouridae", "Coelorinchus", "Species",
             "GSL-S")

clean_auto_missing <- rbind(clean_auto, cae_cae)

#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA in GSL-South survey data ####
#--------------------------------------------------------------------------------------#

correct_taxa <- clean_auto_missing %>% 
  select(-survey)

clean_GSLsouth <- left_join(GSLsouth, correct_taxa, by=c("taxa2"="query")) %>% 
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
write_clean_data(data = clean_GSLsouth, survey = "GSL-S_v2", overwrite = T)


