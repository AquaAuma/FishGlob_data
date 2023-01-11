################################################################################
#### R code to clean trawl survey West Coast US Annual Survey (WCANN)
#### Public data Ocean Adapt
#### Contacts:  Aimee Keller	smartt@dnr.sc.gov,	Fisheries Research Surveys Supervisor
#####                     NOAA, NMFS, NWFSC, FRAM
#####         John Buchanan	john.buchanan@noaa.gov	Fisheries Biologist
#####                     Groundfish Ecology Program, Northwest Fisheries Science Center
#### Coding: Michelle Stuart, Dan Forrest, Zoë Kitchel November 2021
################################################################################

#--------------------------------------------------------------------------------------#
#### LOAD LIBRARIES AND FUNCTIONS ####
#--------------------------------------------------------------------------------------#

library(rfishbase) #needs R 4.0 or more recent
library(tidyverse)
library(lubridate)
library(googledrive)
library(taxize) # for getting correct species names
library(magrittr) # for names wrangling
library(PBSmapping)
library(readxl)

source("functions/clean_taxa.R")
source("functions/write_clean_data.R")
source("functions/apply_trimming_method1.R")
source("functions/apply_trimming_method2.R")
source("functions/flag_spp.R")
fishglob_data_columns <- read_excel("standard_formats/fishglob_data_columns.xlsx")

#Data for the West Coast US Annual Survey can be best accessed using the public
#Pinsky Lab Ocean Adapt Git Hub Repository.
#Contact malin.pinsky@rutgers.edu for questions or help accessing

#--------------------------------------------------------------------------------------#
#### PULL IN AND EDIT RAW DATA FILES ####
#--------------------------------------------------------------------------------------#

temp <- tempfile()
download.file(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wcann_catch.csv.zip", temp)

wcann_catch <- read_csv(unz(temp, "wcann_catch.csv"), col_types = cols(
  catch_id = col_integer(),
  common_name = col_character(),
  cpue_kg_per_ha_der = col_double(),
  cpue_numbers_per_ha_der = col_double(),
  date_yyyymmdd = col_integer(),
  depth_m = col_double(),
  latitude_dd = col_double(),
  longitude_dd = col_double(),
  pacfin_spid = col_character(),
  partition = col_character(),
  performance = col_character(),
  program = col_character(),
  project = col_character(),
  sampling_end_hhmmss = col_character(),
  sampling_start_hhmmss = col_character(),
  scientific_name = col_character(),
  station_code = col_double(),
  subsample_count = col_integer(),
  subsample_wt_kg = col_double(),
  total_catch_numbers = col_integer(),
  total_catch_wt_kg = col_double(),
  tow_end_timestamp = col_datetime(format = ""),
  tow_start_timestamp = col_datetime(format = ""),
  trawl_id = col_double(),
  vessel = col_character(),
  vessel_id = col_integer(),
  year = col_integer(),
  year_stn_invalid = col_integer()
))

wcann_haul <- read_csv(
  "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wcann_haul.csv",
                       col_types = cols(
  area_swept_ha_der = col_double(),
  date_yyyymmdd = col_integer(),
  depth_hi_prec_m = col_double(),
  invertebrate_weight_kg = col_double(),
  latitude_hi_prec_dd = col_double(),
  longitude_hi_prec_dd = col_double(),
  mean_seafloor_dep_position_type = col_character(),
  midtow_position_type = col_character(),
  nonspecific_organics_weight_kg = col_double(),
  performance = col_character(),
  program = col_character(),
  project = col_character(),
  sample_duration_hr_der = col_double(),
  sampling_end_hhmmss = col_character(),
  sampling_start_hhmmss = col_character(),
  station_code = col_double(),
  tow_end_timestamp = col_datetime(format = ""),
  tow_start_timestamp = col_datetime(format = ""),
  trawl_id = col_double(),
  vertebrate_weight_kg = col_double(),
  vessel = col_character(),
  vessel_id = col_integer(),
  year = col_integer(),
  year_stn_invalid = col_integer()
))

# It is ok to get warning message that missing column names filled in: 'X1' [1].

#--------------------------------------------------------------------------------------#
#### REFORMAT AND MERGE DATA FILES ####
#--------------------------------------------------------------------------------------#


wcann <- left_join(wcann_haul, wcann_catch, by = c(
  "trawl_id", "year", "date_yyyymmdd", "station_code",
   "performance","program","project","sampling_end_hhmmss",
    "sampling_start_hhmmss","tow_end_timestamp","tow_start_timestamp",
    "vessel","vessel_id","year_stn_invalid"))
wcann <- wcann %>% 
  mutate(
    # create haul_id
    haul_id = trawl_id,
    # Add "strata" (define by lat, long and depth bands) where needed # no need to use 
    #lon grids on west coast (so narrow)
    stratum = paste(floor(latitude_dd)+0.5, floor(depth_m/100)*100 + 50, sep= "-"), 
    # adjust for tow area # kg per km2 (hectare/100 = km2)
    area_swept = (area_swept_ha_der/100), #km^2
    wgt_cpue = total_catch_wt_kg/area_swept,
    num_cpue = total_catch_numbers/area_swept,
    #note that sample duration is already in hours
    wgt_h = total_catch_wt_kg/sample_duration_hr_der, 
    #note that sample duration is already in hours
    num_h = total_catch_numbers/sample_duration_hr_der, 
    date = ymd(date_yyyymmdd),
    month = month(date),
    day = day(date),
    quarter = case_when(month %in% c(1,2,3) ~ 1,
                        month %in% c(4,5,6) ~ 2,
                        month %in% c(7,8,9) ~ 3,
                        month %in% c(10,11,12) ~ 4),
    season = NA_character_,
  )


wcann <- wcann %>% 
  rename(latitude = latitude_dd, 
         longitude = longitude_dd, 
         depth = depth_m, 
         wgt = total_catch_wt_kg,
         num = total_catch_numbers,
         haul_dur = sample_duration_hr_der,
         spp = scientific_name,
         station = station_code) %>% 
  # remove non-fish
  filter(!grepl("Egg", partition), 
         !grepl("crushed", spp),
         #remove non satisfactory tows where target speed was not maintained
         performance == "Satisfactory"
         ) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp)
  ) %>%
  # add survey column and fill missing columns
  mutate(survey = "WCANN",
         source = "NOAA",
         timestamp = mdy("04/07/2021"),
         sbt = NA,
         sst = NA,
         country = "United States",
         continent = "n_america",
         sub_area = NA,
         stat_rec = NA,
         verbatim_name = spp,
         gear = NA) %>% 
  select(survey, haul_id, source, timestamp, country, sub_area, continent, stat_rec,
         station, stratum, year,
         month, day, quarter, season, latitude, longitude, haul_dur,
         area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)
#many rows with missing num_h, num_cpue, wgt_h, and wgt_cpue values 
#due to missing haul_dur

#sum duplicates
wcann <- wcann %>%
  group_by(survey, 
           source,timestamp,
           haul_id, country, sub_area, continent, stat_rec, station, stratum,
           year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept,
           gear, depth, sbt, sst,verbatim_name) %>%
  summarise(num = sum(num, na.rm = T),
            num_h = sum(num_h, na.rm = T),
            num_cpue = sum(num_cpue, na.rm = T),
            wgt = sum(wgt, na.rm = T),
            wgt_h = sum(wgt_h, na.rm = T),
            wgt_cpue = sum(wgt_cpue, na.rm = T)) %>% ungroup()

#check for duplicates, should not be any with more than 1 obs
#check for duplicates
count_wcann <- wcann %>%
  group_by(haul_id, verbatim_name) %>%
  mutate(count = n())

#none!

#which ones are duplicated?
unique_name_match <- count_wcann %>%
  group_by(verbatim_name) %>%
  filter(count>1) %>%
  distinct(verbatim_name)

unique_name_match
#empty

#now, I will sum over these duplicated verbatim names
#Porifera
#Bathyraja sp.
#Merluccius productus
#Nudibranchia
#Strongylocentrotus
#Pagurus
#Pennatulacea
#Ceramaster
#Neptunea
#Rossellinae
#Gorgonacea
#Colus
#Munidopsis
#Sebastes sp. (aleutianus / melanostictus)
#Buccinum
#Ophiacantha
#Glyptocephalus zachirus
#Oncorhynchus tshawytscha
#Antipatharia
#Urticina
#Stomphia
#Hormathiidae
#Halipteris
#Molpadia intermedia
#Sebastes sp. (miniatus / crocotulus)
#Hexactinosida
#Suberites


#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA FROM TAXA ANALYSIS ####
#--------------------------------------------------------------------------------------#

# Get WoRM's id for sourcing
wrm <- gnr_datasources() %>% 
  filter(title == "World Register of Marine Species") %>% 
  pull(id)

### Automatic cleaning
# Set Survey code
wcann_survey_code <- "WCANN"

wcann <- wcann %>% 
  mutate(
    taxa2 = str_squish(verbatim_name),
    taxa2 = str_remove_all(taxa2," spp.| sp.| spp| sp|NO "),
    taxa2 = str_to_sentence(str_to_lower(taxa2)))

# Get clean taxa
clean_auto <- clean_taxa(unique(wcann$taxa2), input_survey = wcann_survey_code,
                         fishbase=T)
# takes 4.5 mins

#This cuts out the following species, one should be added

#1 Nearchaster aciculosus
#2 Cheiraster dawsoni    
#3 Crangon communis      
#4 Cancer gracilis       
#5 Cancer anthonyi       
#6 Cancer branneri       
#7 Cyclopterinae (fish, but only to genus)

cyclop <- c("Cyclopterinae", NA, NA, "Cyclopterinae", "Animalia", "Chordata","Actinopteri",
            "Scorpaeniformes", "Cyclopteridae", "NA", "Family", "WCANN")

clean_auto.missing <- rbind(clean_auto, cyclop)


#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA in WCANN survey data ####
#--------------------------------------------------------------------------------------#

clean_taxa <- clean_auto.missing %>% 
  select(-survey)

clean_wcann <- left_join(wcann, clean_taxa, by=c("taxa2"="query")) %>% 
  filter(!is.na(taxa)) %>% # query does not indicate taxa entry that were 
  #removed in the cleaning procedure
  # so all NA taxa have to be removed from the surveys because: non-existing, 
  #non marine or non fish
  rename(accepted_name = taxa,
         aphia_id = worms_id) %>% 
  mutate(verbatim_aphia_id = NA,
         num_cpua = num_cpue,
         num_cpue = num_h,
         wgt_cpua = wgt_cpue,
         wgt_cpue = wgt_h,
         survey_unit = ifelse(survey %in% c("BITS","NS-IBTS","SWC-IBTS"),
                              paste0(survey,"-",quarter),survey),
         survey_unit = ifelse(survey %in% c("NEUS","SEUS","SCS","GMEX"),
                              paste0(survey,"-",season),survey_unit)) %>% 
  select(fishglob_data_columns$`Column name fishglob`)


#check for duplicates
count_clean_wcann <- clean_wcann %>%
  group_by(haul_id, accepted_name) %>%
  mutate(count = n())

#none!

#which ones are duplicated?
unique_name_match <- count_clean_wcann %>%
  group_by(verbatim_name, accepted_name) %>%
  filter(count>1) %>%
  distinct(verbatim_name, accepted_name)

unique_name_match
#not empty

#a few duplicates are maintained with different verbatim name
#and the same accepted names. Data users should decide if they want to sum over.
#currently, these are independent observations

#Sebastes or not
#verbatim name                              accepted name
#Sebastes                                  Sebastes     
#Sebastes sp. (miniatus / crocotulus)      Sebastes     
#Sebastes sp. (aleutianus / melanostictus) Sebastes 
# -------------------------------------------------------------------------------------#
#### SAVE DATABASE IN GOOGLE DRIVE ####
# -------------------------------------------------------------------------------------#

# Just run this routine should be good for all
write_clean_data(data = clean_wcann, survey = "WCANN", overwrite = T)



# -------------------------------------------------------------------------------------#
#### FAGS ####
# -------------------------------------------------------------------------------------#
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

######### Apply taxonomic flagging per region
#get vector of regions (here the survey column)
regions <- levels(as.factor(clean_wcann$survey))

#run flag_spp function in a loop
for (r in regions) {
  flag_spp(clean_wcann, r)
}

######### Apply trimming per survey_unit method 1
#apply trimming for hex size 7
dat_new_method1_hex7 <- apply_trimming_per_survey_unit_method1(clean_wcann, 7)

#apply trimming for hex size 8
dat_new_method1_hex8 <- apply_trimming_per_survey_unit_method1(clean_wcann, 8)

######### Apply trimming per survey_unit method 2
dat_new_method2 <- apply_trimming_per_survey_unit_method2(clean_wcann)


#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#
surveys <- sort(unique(clean_wcann$survey))
survey_units <- sort(unique(clean_wcann$survey_unit))
survey_std <- clean_wcann %>% 
  mutate(flag_taxa = NA_character_,
         flag_trimming_hex7_0 = NA_character_,
         flag_trimming_hex7_2 = NA_character_,
         flag_trimming_hex8_0 = NA_character_,
         flag_trimming_hex8_2 = NA_character_,
         flag_trimming_2 = NA_character_)

# integrate taxonomic flags
for(i in 1:length(surveys)){
  if(!surveys[i] %in% c("FALK","GSL-N","MRT","NZ-CHAT","SCS", "SWC-IBTS")){
    xx <- data.frame(read_delim(paste0("outputs/Flags/taxonomic_flagging/",
                                       surveys[i],"_flagspp.txt"),
                                delim=";", escape_double = FALSE, col_names = FALSE,
                                trim_ws = TRUE))
    xx <- as.vector(unlist(xx[1,]))
    
    survey_std <- survey_std %>% 
      mutate(flag_taxa = ifelse(survey == surveys[i] & accepted_name %in% xx,
                                "TRUE",flag_taxa))
    
    rm(xx)
  }
}

# integrate spatio-temporal flags
for(i in 1:length(survey_units)){
  
  if(!survey_units[i] %in% c("DFO-SOG","IS-TAU","SCS-FALL","WBLS")){
    
    hex_res7_0 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res7/",
                                  survey_units[i], "_hex_res_7_trimming_0_hauls_removed.csv"),
                           sep = ";")
    hex_res7_0 <- as.vector(hex_res7_0[,1])
    
    hex_res7_2 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res7/",
                                  survey_units[i], "_hex_res_7_trimming_02_hauls_removed.csv"),
                           sep = ";")
    hex_res7_2 <- as.vector(hex_res7_2[,1])
    
    hex_res8_0 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_0_hauls_removed.csv"),
                           sep= ";")
    hex_res8_0 <- as.vector(hex_res8_0[,1])
    
    hex_res8_2 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_02_hauls_removed.csv"),
                           sep = ";")
    hex_res8_2 <- as.vector(hex_res8_2[,1])
    
    trim_2 <- read.csv(paste0("outputs/Flags/trimming_method2/",
                              survey_units[i],"_hauls_removed.csv"))
    trim_2 <- as.vector(trim_2[,1])
    
    survey_std <- survey_std %>% 
      mutate(flag_trimming_hex7_0 = ifelse(survey_unit == survey_units[i] & haul_id %in% hex_res7_0,
                                           "TRUE",flag_trimming_hex7_0),
             flag_trimming_hex7_2 = ifelse(survey_unit == survey_units[i] & haul_id %in% hex_res7_2,
                                           "TRUE",flag_trimming_hex7_2),
             flag_trimming_hex8_0 = ifelse(survey_unit == survey_units[i] & haul_id %in% hex_res8_0,
                                           "TRUE",flag_trimming_hex8_0),
             flag_trimming_hex8_2 = ifelse(survey_unit == survey_units[i] & haul_id %in% hex_res8_2,
                                           "TRUE",flag_trimming_hex8_2),
             flag_trimming_2 = ifelse(survey_unit == survey_units[i] & haul_id %in% trim_2,
                                      "TRUE", flag_trimming_2)
      )
    rm(hex_res7_0, hex_res7_2, hex_res8_0, hex_res8_2, trim_2)
  }
}


# Just run this routine should be good for all
write_clean_data(data = survey_std, survey = "WCANN_std",
                 overwrite = T, rdata=TRUE)


