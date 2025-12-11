################################################################################
#### R code to clean trawl survey Gulf of Mexico
#### Public data Ocean Adapt
#### Contacts: Jeff Rester jrester@gsmfc.org Coordinator - Gulf States Marine Fisheries 
####                Commission - Habitat Focus Team - Gulf of Mexico Program
####           David Hanisko david.s.hanisko@noaa.gov Research Fisheries Biologist, 
####                National Marine Fisheries Service, Southeast Fisheries Science Center
#### Coding: Michelle Stuart, Dan Forrest, Zoë Kitchel November 2021
################################################################################
####Update
####Zoe Kitchel
#### May 4, 2024
####Following issue 47, need to update sum technique to remove duplicates
####Malin Pinsky
#### 10 December, 2025
####Following issue #46, updating the GMEX data joins to follow DISMAP
################################################################################
#Relevant Organizations
#Gulf States Marine Fisheries Commission: https://www.gsmfc.org/seamap-gomrs.php
#Southeast Area Monitoring and Assessment Program Reports: 
#https://www.fisheries.noaa.gov/southeast/funding-and-financial-services/
#southeast-area-monitoring-and-assessment-program-seamap

#Helpful reference document
#https://sedarweb.org/docs/wpapers/SEDAR7_DW1.pdf
#Many different survey events included in the files we pull in 
#Most consistent through time are Summer SEAMAP 1987-on and Fall SEAMAP 1988-on
#All other surveys are excluded in the following code

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
library(here)

source("functions/clean_taxa.R")
source("functions/write_clean_data.R")
source("functions/apply_trimming_method1.R")
source("functions/apply_trimming_method2.R")
source("functions/flag_spp.R")
fishglob_data_columns <- read_excel("standard_formats/fishglob_data_columns.xlsx")

# function to calculate convex hull area in km2
#developed from http://www.nceas.ucsb.edu/files/scicomp/GISSeminar/UseCases/CalculateConvexHull/CalculateConvexHullR.html
calcarea <- function(lon,lat){
  hullpts = chull(x=lon, y=lat) # find indices of vertices
  hullpts = c(hullpts,hullpts[1]) # close the loop
  lonlat <- data.frame(cbind(lon, lat))
  ps = appendPolys(NULL,mat=as.matrix(lonlat[hullpts,]),1,1,FALSE) # create a Polyset object
  attr(ps,"projection") = "LL" # set projection to lat/lon
  psUTM = convUL(ps, km=TRUE) # convert to UTM in km
  polygonArea = calcArea(psUTM,rollup=1)
  return(polygonArea$area)
}

#acts like sum(na.rm=T) but returns NA if all are NA
sumna <- function(x){
  if(!all(is.na(x))) return(sum(x, na.rm=T))
  if(all(is.na(x))) return(NA)
}


#-------------------------------------------------------------------------------#
#### READ IN RAW DATA FILES USING DISMAP METHODS
#### see https://github.com/nmfs-ost/DisMAP/blob/main/data_processing_rcode/code/Compile_Dismap_Current.R
#### Visit the [Gulf of Mexico]("https://seamap.gsmfc.org/") website
#### Click "Download" the SEAMAP Trawl/Plankton, Bottom Longline
#### Fill in the form ("Scientific Research", "Educational Institution", "Trawl/Plankton Data (CSV)"
#### Unzip the CSV in your downloads folder
#### then copy them into the data folder with the script below
#### (this folder is not tracked by git)
#-------------------------------------------------------------------------------#

# station table
gmex_station <- read_csv(here::here("data", "gmex_STAREC.csv"), col_types = cols(.default = col_character())) %>%
  dplyr::select('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'STAT_ZONE', 'DEPTH_SSTA', 'MO_DAY_YR', 'VESSEL_SPD', 'COMSTAT', 'TEMP_SSURF', 'TEMP_BOT')

# old code to read in. remove this when done editing.
# gmex_station <- read_csv(file = "gmex_station_raw.txt", col_types = cols(.default = col_character())) %>% dplyr::select('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN',
#                 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD',
#                 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR',
#                 'VESSEL_SPD', 'COMSTAT','TEMP_SSURF','TEMP_BOT')


# old step: remove oddly quoted characters
#gmex_station_clean <- str_replace_all(gmex_station_raw, "\\\\\"", "")

gmex_station <- type_convert(gmex_station, col_types = cols(
  STATIONID = col_integer(),
  CRUISEID = col_integer(),
  CRUISE_NO = col_integer(),
  P_STA_NO = col_character(),
  TIME_ZN = col_integer(),
  TIME_MIL = col_character(),
  S_LATD = col_integer(),
  S_LATM = col_double(),
  S_LOND = col_integer(),
  S_LONM = col_double(),
  E_LATD = col_integer(),
  E_LATM = col_double(),
  E_LOND = col_integer(),
  E_LONM = col_double(),
  DEPTH_SSTA = col_double(),
  STAT_ZONE = col_double(),
  MO_DAY_YR = col_date(format = "%d/%m/%Y"),
  VESSEL_SPD = col_double(),
  COMSTAT = col_character()
))

names(gmex_station)<-tolower(names(gmex_station))

# tow table
gmex_tow <-readr::read_delim(here::here("data","gmex_INVREC.csv"),
                             delim = ',', escape_backslash = T, escape_double = F)
gmex_tow<-type_convert(gmex_tow, col_types = cols(
  INVRECID = col_integer(),
  STATIONID = col_integer(),
  CRUISEID = col_integer(),
  VESSEL = col_integer(),
  CRUISE_NO = col_integer(),
  P_STA_NO = col_character(),
  GEAR_SIZE = col_integer(),
  GEAR_TYPE = col_character(),
  MESH_SIZE = col_double(),
  OP = col_character(),
  MIN_FISH = col_integer(),
  WBCOLOR = col_character(),
  BOT_TYPE = col_character(),
  BOT_REG = col_character(),
  TOT_LIVE = col_double(),
  FIN_CATCH = col_double(),
  CRUS_CATCH = col_double(),
  OTHR_CATCH = col_double(),
  T_SAMPLEWT = col_double(),
  T_SELECTWT = col_double(),
  FIN_SMP_WT = col_double(),
  FIN_SEL_WT = col_double(),
  CRU_SMP_WT = col_double(),
  CRU_SEL_WT = col_double(),
  OTH_SMP_WT = col_double(),
  OTH_SEL_WT = col_double(),
  COMBIO = col_character(),
  X28 = col_character()
))
gmex_tow <- gmex_tow %>%
  dplyr::select('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP') %>%
  filter(GEAR_TYPE=='ST') #ST = shrimp trawl (this is what OceanAdapt does too, preserves 90% of tows)


# biology table
gmex_bio <-readr::read_delim(here::here("data","gmex_BGSREC.csv"),
                             delim = ',', escape_backslash = T, escape_double = F)

gmex_bio <- type_convert(gmex_bio, cols(
  CRUISEID = col_integer(),
  STATIONID = col_integer(),
  VESSEL = col_integer(),
  CRUISE_NO = col_integer(),
  P_STA_NO = col_character(),
  GENUS_BGS = col_character(),
  SPEC_BGS = col_character(),
  BGSCODE = col_character(),
  BIO_BGS = col_integer(),
  SELECT_BGS = col_double()
))

# old code. remove when done.
# gmex_bio <- read.csv(unz(temp, "gmex_BGSREC.csv")) %>% 
#   dplyr::select('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO',
#                 'GENUS_BGS','CNT','CNTEXP', 'SPEC_BGS', 'BGSCODE', 'BIO_BGS', 'SELECT_BGS') %>%
#   # trim out young of year records (only useful for count data) and those with 
#   #UNKNOWN species
#   filter(BGSCODE != "T" | is.na(BGSCODE),
#          GENUS_BGS != "UNKNOWN" | is.na(GENUS_BGS))  %>%
#   # remove the few rows that are still duplicates
#   distinct()



# cruise table
gmex_cruise <-read_csv(here::here("data", "gmex_CRUISES.csv"), col_types = cols(.default = col_character())) %>%
  dplyr::select(CRUISEID, VESSEL, TITLE, SOURCE)

gmex_cruise <- type_convert(gmex_cruise, col_types = cols(CRUISEID = col_integer(), VESSEL = col_integer(), TITLE = col_character(), SOURCE = col_character()))
names(gmex_cruise)<-tolower(names(gmex_cruise))


# species table
gmex_spp <-read_csv(here::here("data","gmex_BCT_NFR_01182023.csv"))
problems(gmex_spp)
names(gmex_spp)<-tolower(names(gmex_spp))
gmex_spp<-dplyr::select(gmex_spp,biocode,ciu_biocode,taxon)

# old code. remove when done.
# gmex_spp <-read_csv(
#   "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_NEWBIOCODESBIG.csv",
#   col_types = cols(
#     Key1 = col_integer(),
#     TAXONOMIC = col_character(),
#     CODE = col_integer(),
#     TAXONSIZECODE = col_character(),
#     isactive = col_integer(),
#     common_name = col_character(),
#     tsn = col_integer(),
#     tsn_accepted = col_integer()
#   )) %>% 
#   dplyr::select(-tsn_accepted)


#--------------------------------------------------------------------------------------#
#### REFORMAT AND MERGE DATA FILES ####
#--------------------------------------------------------------------------------------#

##Resolve issues
#Issue 1: Proper way to Merge the Tow (invrec) and bio (bgsrec) tables
# The proper way to link the invrec table to the bgsrec is supposed to use the invrecid
# variable as the primary key. However, the bgsrec table has null invrecid for data collected
# under previous data collection systems. The invrec and bgsrec tables can be linked using
# the vessel, cruise_no and p_sta_no variables as a primary key. Unfortunately, there are
# a series stations where the Oregon II (Vessel 4 Cruise_No = 0284) towed standard
# shrimp trawls (ST) side by side (port/starboard) with experimental trawls (ES). Therefore,
# linking the invrec and bgsrec tabls based on the vessel, cruise_no and p_sta_no variables
# will lead to all catch records for both the shrimp and experimental trawls being linked
# to both trawls. The bgsrec also contains records for catches not associated with invrec table
# records. These are from reef fish cruises. The following codes creates a modified bgsrec table
# that updates the null invrecid for older data and performs some checks.

names(gmex_tow) <- tolower(names(gmex_tow))
names(gmex_bio) <- tolower(names(gmex_bio))
#create bgsrec_invrecid_fix
#get only stationid and invrecid from invrec table
get_stationid_invrecid <- gmex_tow %>% dplyr::select(stationid, invrecid) %>% rename(inv_invrecid = invrecid)

#extract bgsrec table records with missing invrecid and update based on stationid from get_stationid_invrecid
bgsrec_null_invrecid <- gmex_bio %>%
  dplyr::filter(is.na(invrecid)) %>%
  dplyr::left_join(get_stationid_invrecid, by = 'stationid') %>%
  dplyr::mutate(invrecid = inv_invrecid) %>% dplyr::select(-inv_invrecid)

#Extracts any remaining bgsrec table records with null invrecid. These should all be
#associated with reef fish cruises at this point.
bgsrec_null_check1 <- bgsrec_null_invrecid %>%
  dplyr::filter(is.na(invrecid))

#extract bgsrec table records with valid invrecid
bgsrec_with_invrecid <- gmex_bio %>%
  dplyr::filter(!is.na(invrecid))

#Stack bgsrec_null_invrec now updated with valid invrecid and bgsrec_with_invrecid
gmex_bio_mod <- bgsrec_null_invrecid %>%
  dplyr::bind_rows(bgsrec_with_invrecid) %>%
  #Remove null invrecid which should only include those in bgsrec_null_check1
  dplyr::filter(!is.na(invrecid)) %>%
  dplyr::arrange(bgsid)

#Check to make sure only records with invrecs are present - should have 0 rows
bgsrec_null_check2 <- gmex_bio_mod %>% filter(is.na(invrecid))

#drop unwanted data objects
rm(bgsrec_null_invrecid,bgsrec_null_check1,bgsrec_null_check2,bgsrec_with_invrecid,get_stationid_invrecid, gmex_bio)
# garbace collect to free up memory
gc()

#Issues 2: Taxonomic coding
# (3-1) The newbiocodesbig table does not fully contain all code/taxonomic names found in the bgsrec table:
# (3-2) the bgsrec table has a few instances of invalid bio_bgs (biocode) values; and
# (3-3) multiple code/taxonomic combinations may refer to the same organisms under different names. For example,
# 189040204/MONACANTHUS HISPIDUS, 189040305/STEPHANOLEPIS HISPIDA, 189040306/STEPHANOLEPIS HISPIDUS
# and 189040307/STEPHANOLEPIS HISPIDA (current) have all been used to identify Planehead Filefish due to
# changes in taxonomy. The bsgrec file reflects the code/taxonomic use at time of data ingest.
# The provided master biocode table (MBT) will allow translation of the vast majority cases where multiple
# code/taxonomic refer to the same organism. The process relies on the use of the biocode,
# ciu_biocode and taxon variables in the MBT. The MBT biocode variable in numeric form is equivalent to the
# code (character) variable in the newbiocodesbig and bio_bgs (character) variable in the bgsrec tables.
# Similarly the taxon variable in the MBT table is equivalent to taxonomic in the newbiocodesbig table.
# The MBT also has a rb_biocode (replaced by biocode) variable which is the numeric biocode that
# replaces a inactive (inactive = 1 variable) biocode, and allows me to track changes over time.
# Since multiple changes may have occurred, the ciu_biocode (currently in use biocode) value ties multiple records
# that are now inactive to the current active biocode. Inactive biocodes have the variable inactive set to zero.
# Using the example above, the ciu_biocode that ties together records of Planehead Filefish is 189040307.
# The following script updates bgsrec table code to ciu_biocode via the MBT table. The rb_biocode variable is not
# needed for this purpose.

# starting with our gmex_bio_mod from above
gmex_bio_utax1 <- gmex_bio_mod %>%
  #convert bgsrec table bio_bgs varialbe to numeric integer
  dplyr::mutate(bio_bgs = as.integer(bio_bgs)) %>%
  #rename bio_bgs to biocode to allow for easier manipulation with master biocode table (mbt)
  dplyr::rename(biocode = bio_bgs) %>%
  ### take care of Issue 3-2 ###
  # fix invalid zero code and make it the code (999999998) for unidentified specimen
  dplyr::mutate(biocode = ifelse(biocode == 0,999999998,biocode)) %>%
  # fix invalid unidentified fish code 100000001 to proper code
  dplyr::mutate(biocode = ifelse(biocode == 100000001,100000000,biocode)) %>%
  # fix invalid unidentified crustacean code 200000001 to proper code
  dplyr::mutate(biocode = ifelse(biocode == 200000001,200000000,biocode)) %>%
  # fix invalid unidentified crustacean code 300000001  and 300000001 to proper code
  dplyr::mutate(biocode = ifelse(biocode == 300000001,300000000,biocode)) %>%
  dplyr::mutate(biocode = ifelse(biocode == 300000002,300000000,biocode)) %>%
  ### take care of Issue 3-3 ###
  #update older inactive biocodes to those currently in use (ciu_biocode)
  dplyr::left_join(dplyr::select(gmex_spp,biocode,taxon,ciu_biocode), by = "biocode") %>%
  #rename taxon to bgs taxon to keep the original name associated with a biocode
  dplyr::rename(bgs_taxon = taxon) %>%
  #do a left join to bring in taxon associated with ciu_taxon
  dplyr::left_join(dplyr::select(gmex_spp,biocode,taxon), by = c("ciu_biocode" = "biocode"))

### Issue 3: Problematic Taxa with taxonomic issues or problematic separation in the field###
# Collapse taxa with known identification issues and collapse all sponge to single category
# Note this process needs to be implemented after the ciu_biocode update as the statements
# rely on the ciu_biocode. The statements undergo a review with each updated version of the
# MBT
gmex_bio_utax2 <- gmex_bio_utax1 %>%
  #Take care of squid and species complexes...
  #Update the squid genus Loligo and all species under genus Doryteuthis to the genus Doryteuthis
  mutate(ciu_biocode = ifelse(ciu_biocode %in% c(347020200,347021001,347021002,347021003),347021000,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(347021000),'DORYTEUTHIS SP',taxon)) %>%
  # #Update batfish species to Halieutichthys
  mutate(ciu_biocode = ifelse(ciu_biocode >= 195050401 & ciu_biocode <= 195050405,195050400,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(195050400),'HALIEUTICHTHYS SP',taxon)) %>%
  #Update all jellfishy in the genus Aurelia to the genus Aurelia
  mutate(ciu_biocode = ifelse(ciu_biocode >= 618010101 & ciu_biocode <= 618010105,618010100,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(618010100),'AURELIA',taxon)) %>%
  #Update all lionfishes species to the genus Pterois
  mutate(ciu_biocode = ifelse(ciu_biocode %in% c(168011901,168011902),168011900,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(168011900),'PTEROIS',taxon)) %>%
  #smoothhounds (Mustelus) Managed as species complex, our ids are OK now but in the past assumptions made %>%
  mutate(ciu_biocode = ifelse(ciu_biocode %in% c(108031101,108031102,108031103,108031104),108031100,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(108031100),'MUSTELUS SP',taxon)) %>%
  #lump all sponge identifications to Porifera
  mutate(ciu_biocode = ifelse(ciu_biocode >= 613000000 & ciu_biocode < 616000000,613000000,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(613000000),'PORIFERA',taxon)) %>%
  #handle out of order Porifera  Demospngiae and Agelas and Agelas and Agelasidae in coral numbers
  mutate(ciu_biocode = ifelse(ciu_biocode %in% c(999997000,999997020,617170000,617170100),613000000,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(613000000),'PORIFERA',taxon)) %>%
  #Collapse all shrimp species in Rimnapenaeus as they are not consistently seperated in the field
  mutate(ciu_biocode = ifelse(ciu_biocode %in% c(228012001,228012002),228012000,ciu_biocode)) %>%
  mutate(taxon = ifelse(ciu_biocode %in% c(228012000),'RIMAPENAEUS',taxon)) %>%
  #Astropecten species have changed, distribution overlap with major east west differences
  mutate(biocode = ifelse(biocode >= 691010101 & biocode <= 691010112,691010100,biocode)) %>%
  mutate(taxon = ifelse(biocode %in% c(691010100),'ASTROPECTEN',taxon))

## MERGE the corrected catch/tow/species information from above with cruise information, but only for shrimp trawl tows (ST)
gmex <- left_join(gmex_bio_utax2, gmex_tow, by = c("cruiseid", "stationid","vessel", "cruise_no", "p_sta_no", "invrecid")) %>%
  # add station location and related data
  left_join(gmex_station, by = c("cruiseid", "stationid", "cruise_no", "p_sta_no")) %>%
  # add cruise title
  left_join(gmex_cruise, by = c("cruiseid", "vessel")) %>%
  #filter out YOY (denoted by BSGCODE=T) since they are useful for counts but not weights
  filter(bgscode != "T"| is.na(bgscode))

# merge tow information with catch data, but only for shrimp trawl tows (ST)
# gmex <- left_join(gmex_bio, gmex_tow, by = c("STATIONID", "CRUISE_NO", "P_STA_NO")) %>% 
#   # add station location and related data
#   left_join(gmex_station, by = c("CRUISEID", "STATIONID", "CRUISE_NO", "P_STA_NO")) %>% 
#   # add scientific name
#   left_join(gmex_spp, by = "BIO_BGS") %>% 
#   # add cruise title
#   left_join(gmex_cruise, by = c("CRUISEID", "VESSEL"))


gmex <- gmex %>%
  # Trim to high quality SEAMAP summer trawls, based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas
  filter(grepl("Summer", title) &
           gear_size == 40 &
           mesh_size == 1.63 &
           # OP has no letter value
           !grepl("[A-Z]", op)) %>%
  mutate(
    # Create a unique haulid
    haulid = paste(formatC(vessel, width=3, flag=0), formatC(cruise_no, width=3, flag=0), formatC(p_sta_no, width=5, flag=0, format='d'), sep='-'),
    # Extract year where needed
    year = year(mo_day_yr),
    # Calculate decimal lat and lon, depth in m, where needed
    s_latd = ifelse(s_latd == 0, NA, s_latd),
    s_lond = ifelse(s_lond == 0, NA, s_lond),
    e_latd = ifelse(e_latd == 0, NA, e_latd),
    e_lond = ifelse(e_lond == 0, NA, e_lond),
    lat = rowMeans(cbind(s_latd + s_latm/60, e_latd + e_latm/60), na.rm=T),
    lon = -rowMeans(cbind(s_lond + s_lonm/60, e_lond + e_lonm/60), na.rm=T),
  )

#add stratum code defined by STAT_ZONE and depth bands (note depth in recorded as m, and depth bands based on 0-20 fathoms
# and 21-60 fathoms))
gmex$depth_zone<-ifelse(gmex$depth_ssta<=36.576, "20",
                        ifelse(gmex$depth_ssta>36.576, "60", NA))
gmex<-gmex %>%
  mutate(stratum = paste(stat_zone, depth_zone, sep= "-"))


# old code. remove when done.
# gmex <- gmex %>% 
#   # Trim to high quality SEAMAP summer trawls (1987-on) and SEAMAP fall trawls (1988-on)
#   #based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas, but including fall 
#   #Keeps Fall SEAMAP groundfish Survey,Fall SEAMAP Groundfish Survey,
#   #Fall SEAMAP Groundfish Suvey, Summer SEAMAP Groundfish Survey, Summer SEAMAP 
#   #Groundfish Suvey 
#   filter(
#     ((grepl("Summer", TITLE) & year(as.Date(MO_DAY_YR)) >= 1987) |
#        (grepl("Fall", TITLE) & year(as.Date(MO_DAY_YR)) >= 1988)) &
#       GEAR_SIZE == 40 & 
#       MESH_SIZE == 1.63 &
#       # OP has no letter value
#       !grepl("[A-Z]", OP)) %>% 
#   mutate(
#     # Create a unique haulid
#     haulid = paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE_NO, width=3, flag=0),
#                    formatC(P_STA_NO, width=5, flag=0, format='d'), S_LATD, S_LOND,sep=''), 
#     # Extract year where needed
#     year = year(MO_DAY_YR),
#     month = month(MO_DAY_YR),
#     day = day(MO_DAY_YR),
#     quarter = case_when(month %in% c(1,2,3) ~ 1,
#                         month %in% c(4,5,6) ~ 2,
#                         month %in% c(7,8,9) ~ 3,
#                         month %in% c(10,11,12) ~ 4),
#     season = ifelse(
#       grepl("Summer", TITLE), "Summer",
#       ifelse(grepl("Fall", TITLE), "Fall", NA
#              
#       )),
#     # Calculate decimal lat and lon, depth in m, where needed
#     S_LATD = ifelse(S_LATD == 0, NA, S_LATD), 
#     S_LOND = ifelse(S_LOND == 0, NA, S_LOND), 
#     E_LATD = ifelse(E_LATD == 0, NA, E_LATD), 
#     E_LOND = ifelse(E_LOND == 0, NA, E_LOND),
#     latitude = rowMeans(cbind(S_LATD + S_LATM/60, E_LATD + E_LATM/60), na.rm=T), 
#     longitude = -rowMeans(cbind(S_LOND + S_LONM/60, E_LOND + E_LONM/60), na.rm=T), 
#     # convert fathoms to meters
#     depth = DEPTH_SSTA * 1.8288, 
#     # Add "strata" (define by lat, lon and depth bands) where needed
#     # degree bins, # degree bins, # 100 m bins
#     stratum = paste(floor(latitude)+0.5, floor(longitude)+0.5, 
#                     floor(depth/100)*100 + 50, sep= "-")
#   )


# # fix speed
# Trim out or fix speed and duration records
# trim out tows of 0, >60, or unknown minutes
gmex <- gmex %>%
  filter(min_fish <= 60 & min_fish  > 0 & !is.na(min_fish )) %>%
  # fix typo according to Jeff Rester: 30 = 3
  mutate(vessel_spd = ifelse(vessel_spd == 30, 3, vessel_spd)) %>%
  # trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)
  filter(vessel_spd <= 5 & vessel_spd > 0  & !is.na(vessel_spd))


gmex_strats <- gmex %>%
  group_by(stratum) %>%
  summarise(stratumarea = calcarea(lon, lat))
gmex <- left_join(gmex, gmex_strats, by = "stratum")

# while comsat is still present
# Remove a tow when paired tows exist (same lat/lon/year but different haulid, only Gulf of Mexico)
# identify duplicate tows at same year/lat/lon
dups <- gmex %>%
  group_by(year, lat, lon) %>%
  filter(n() > 1) %>%
  group_by(haulid) %>%
  filter(n() == 1)

# remove the identified tows from the dataset
gmex <- gmex %>%
  filter(!haulid %in% dups$haulid & !grepl("PORT", comstat))

gmex <- gmex %>%
  rename(sub_area = source,
         gear = gear_type,
         haul_id = haulid,
         station = stationid,
         verbatim_name = taxonomic,
         spp = taxon,
         depth = depth_ssta,
         num = cnt_exp,
         haul_dur.min = min_fish,
  ) %>%
  # adjust for area towed
  mutate(
    # kg per 10000m2. calc area trawled in m2: knots * 1.8 km/hr/knot * 1000 m/km * minutes * 1 hr/60 min * width of gear in feet * 0.3 m/ft # biomass per standard tow
    wtcpue = 10000*select_bgs/(vessel_spd * 1.85200 * 1000 * min_fish / 60 * gear_size * 0.3048),
    num_cpue = 1000000*num/(VESSEL_SPD * 1.85200 * 1000 * haul_dur.min / 60 * GEAR_SIZE * 0.3048),
    wgt_h = 60*wgt/haul_dur.min,
    num_h  = 60*num/haul_dur.min,
    # add survey column
    survey = "GMEX",
    country = "United States",
    continent = "n_america",
    stat_rec = NA,
    verbatim_aphia_id = NA,
    #haul duration in hours is haul duration minutes * 1 hour/60 minutes
    haul_dur = haul_dur.min/60
  ) %>%
  # remove non-fish
  filter(
    spp != '' | !is.na(spp),
    # remove unidentified spp
    !spp %in% c('UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')
  ) %>%
  group_by(haul_id, stratum, stratumarea, year, lat, lon, depth, spp) %>%
  #sum wtcpue for duplicates (all columns the same except for BGSID which 
  #we don't pull in, and doesn't  have any significance other than telling us that 
  #these are indeed independent observations. we're not sure why this occurs
  #in the raw data files, but it was the recommended technique by Jeff in 2012)
  summarise(wtcpue = sumna(wtcpue)) %>%
  # add region column
  mutate(region = "Gulf of Mexico") %>%
  # note this doesn't retain sst or sbt columns
  dplyr::select(survey, haul_id, country, sub_area, continent, stat_rec, station, stratum,
                year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept,
                gear, depth, verbatim_name, num, num_h, num_cpue, 
                wgt, wgt_h, wgt_cpue, verbatim_aphia_id) %>%
  ungroup()

# old code. remove when done.
# gmex <- gmex %>% 
#   group_by(haulid, stratum, year, latitude, longitude, depth, BIO_BGS, SOURCE, 
#            MIN_FISH, GEAR_TYPE, SPEC_BGS,GENUS_BGS,
#            STATIONID, TEMP_BOT, TEMP_SSURF, TAXONOMIC, VESSEL_SPD, GEAR_SIZE, 
#            month, day, quarter, season) %>% 
#   summarise(SELECT_BGS = my_sum(SELECT_BGS), #sum weights across duplicates
#             CNTEXP = my_sum(CNTEXP)) #sum counts across duplicates

# gmex <- gmex %>% 
#   rename(sub_area = SOURCE,
#          haul_dur.min = MIN_FISH,
#          gear = GEAR_TYPE, 
#          haul_id = haulid,
#          station = STATIONID,
#          sbt = TEMP_BOT,
#          sst = TEMP_SSURF,
#          verbatim_name = TAXONOMIC,
#          num = CNTEXP,
#          wgt = SELECT_BGS
#          ) %>% 
#   mutate(
#   # adjust for area towed
# # kg per 1,000,000m2 (1km^2). calc area trawled in m2:
#     #     knots * 1.8 km/hr/knot * 1000 m/km * minutes *
#     #       1 hr/60 min * width of gear in feet * 0.3 m/ft # biomass per standard tow
#     wgt_cpue = 1000000*wgt/
#       (VESSEL_SPD * 1.85200 * 1000 * haul_dur.min / 60 * GEAR_SIZE * 0.3048), 
# # count per 1,000,000m2 (1km^2). calc area trawled in m2: knots * 
# #                  1.8 km/hr/knot * 1000 m/km * minutes * 1 hr/60 min *
# #                  width of gear in feet * 0.3 m/ft # biomass per standard tow
#     num_cpue = 1000000*num/
#       (VESSEL_SPD * 1.85200 * 1000 * haul_dur.min / 60 * GEAR_SIZE * 0.3048),
#    #area_swept in km^2: knots * 1.8 km/hr/knot * minutes * 
# #                  1 hr/60 min * width of gear in feet * 0.0003 km/ft
#    area_swept = VESSEL_SPD * 1.85200 * haul_dur.min / 60 * GEAR_SIZE * 0.0003048,
#    #kg per hour: 60 minutes/hour * kg / minutes fished
#    wgt_h = 60*wgt/haul_dur.min,
#    #count per hour:  60 minutes/hour * abundance/minutes fished
#    num_h  = 60*num/haul_dur.min
#   ) %>% 
#   # remove non-fish
#   filter(
#     verbatim_name != '' | !is.na(verbatim_name),
#     # remove unidentified verbatim_name
#     !verbatim_name %in% 
#       c('UNID CRUSTA', 'UNID OTHER', 'UNID.FISH',
#           'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01',
#           'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')
#   ) %>% 
#   mutate(
#   # adjust verbatim_name names
#     verbatim_name = ifelse(GENUS_BGS == 'PELAGIA' &
#                              SPEC_BGS == 'NOCTUL', 'PELAGIA NOCTILUCA', verbatim_name), 
#     BIO_BGS = ifelse(verbatim_name == "PELAGIA NOCTILUCA", 618030201, BIO_BGS), 
#     verbatim_name = ifelse(GENUS_BGS == 'MURICAN' &
#                              SPEC_BGS == 'FULVEN', 'MURICANTHUS FULVESCENS', verbatim_name), 
#     BIO_BGS = ifelse(verbatim_name == "MURICANTHUS FULVESCENS", 308011501, BIO_BGS), 
#     verbatim_name = ifelse(grepl("APLYSIA", verbatim_name), "APLYSIA", verbatim_name), 
#     verbatim_name = ifelse(grepl("AURELIA", verbatim_name), "AURELIA", verbatim_name), 
#     verbatim_name = ifelse(grepl("BOTHUS", verbatim_name), "BOTHUS", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "CLYPEASTER", verbatim_name), "CLYPEASTER", verbatim_name), 
#     verbatim_name = ifelse(grepl("CONUS", verbatim_name), "CONUS", verbatim_name), 
#     verbatim_name = ifelse(grepl("CYNOSCION", verbatim_name), "CYNOSCION", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "ECHINASTER", verbatim_name), "ECHINASTER", verbatim_name),
#     verbatim_name = ifelse(grepl(
#       "OPISTOGNATHUS", verbatim_name), "OPISTOGNATHUS", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "OPSANUS", verbatim_name), "OPSANUS", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "ROSSIA", verbatim_name), "ROSSIA", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "SOLENOCERA", verbatim_name), "SOLENOCERA", verbatim_name), 
#     verbatim_name = ifelse(grepl(
#       "TRACHYPENEUS", verbatim_name), "TRACHYPENEUS", verbatim_name)
#   ) %>% 
#   # add survey column
#   mutate(survey = "GMEX",
#          country = "United States",
#          continent = "n_america",
#          stat_rec = NA,
#          verbatim_aphia_id = NA,
#          #haul duration in hours is haul duration minutes * 1 hour/60 minutes
#          haul_dur = haul_dur.min/60
#   ) %>% 
#   ungroup() %>%
#   dplyr::select(survey, haul_id, country, sub_area, continent, stat_rec, station, stratum,
#          year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept,
#          gear, depth, sbt, sst, verbatim_name, num, num_h, num_cpue,
#          wgt, wgt_h, wgt_cpue, verbatim_aphia_id)


#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA FROM TAXA ANALYSIS ####
# MALIN START HERE December 2025
#--------------------------------------------------------------------------------------#

# Get WoRM's id for sourcing
wrm <- gna_data_sources() %>% 
  filter(title == "World Register of Marine Species") %>% 
  pull(id)

### Automatic cleaning
# Set Survey code
gmex_survey_code <- "GMEX"

gmex <- gmex %>% 
  mutate(
    taxa2 = str_squish(verbatim_name),
    taxa2 = str_remove_all(taxa2," spp.| sp.| spp| sp|NO "),
    taxa2 = str_to_sentence(str_to_lower(taxa2))
  )

# Get clean taxa
clean_auto <- clean_taxa(unique(gmex$taxa2),
                         input_survey = gmex_survey_code,
                         save = F, output=NA, fishbase=T) # takes 2 mins. 8 December 2025 (Malin Pinsky)

# [1] "Returned 813 taxa and dropped 1156. Misspelled taxa: 258; No alphia id found: 120; Non-fish classes: 1036; Non-marine taxa: 1 All taxa assessed =FALSE"
# Time difference of -1.986862 mins. (8 December 2025, Malin Pinsky)
           

#all invertebrates except for Astroscopus y-graecum

#add new row for this species
ast_ygr <- c("Astroscopus y-graecum", 159252,3704,
             "Astroscopus y-graecum","Animalia","Chordata",
             "Actinopteri","Perciformes","Uranoscopidae","Astroscopus","Species","GMEX")

clean_auto.missing <- rbind(clean_auto, ast_ygr)

#--------------------------------------------------------------------------------------#
#### INTEGRATE CLEAN TAXA in GMEX survey data ####
#--------------------------------------------------------------------------------------#
clean_taxa <- clean_auto.missing %>% 
  dplyr::select(-survey) %>% 
  filter(!(query == "Astroscopus y-graecum" & is.na(SpecCode)))

clean_gmex <- left_join(gmex, clean_taxa, by=c("taxa2"="query")) %>% 
  filter(!is.na(taxa)) %>% 
  # query does not indicate taxa entry that were removed in the cleaning procedure
  # so all NA taxa have to be removed from the surveys because:
        #non-existing, non marine or non fish
  rename(accepted_name = taxa,
         aphia_id = worms_id) %>% 
  mutate(verbatim_aphia_id = NA,
         source = "NOAA",
         timestamp = lubridate::my("03/2021"),
         num_cpua = num_cpue,
         num_cpue = num_h,
         wgt_cpua = wgt_cpue,
         wgt_cpue = wgt_h,
         survey_unit = ifelse(survey %in% c("BITS","NS-IBTS","SWC-IBTS"),
                              paste0(survey,"-",quarter),survey),
         survey_unit = ifelse(survey %in% c("NEUS","SEUS","SCS","GMEX"),
                              paste0(survey,"-",season),survey_unit)) %>% 
  dplyr::select(fishglob_data_columns$`Column name fishglob`)


#check for duplicates
count_clean_gmex <- clean_gmex %>%
  group_by(haul_id, accepted_name) %>%
  mutate(count = n())

#which ones are duplicated?
unique_name_match <- count_clean_gmex %>%
  group_by(accepted_name, verbatim_name) %>%
  filter(count>1) %>%
  distinct(accepted_name, verbatim_name)

#explanations for duplications of haulid x species
  #Etropus crossotus and Etropus intermedius both fix to Etropus crossotus
  #Monacanthus hispidus, Monacanthus setifer, and Stephanolepis hispida
      #all fix to Stephanolepis hispida
  #Ophidion beani and Ophidion holbrooki both fix to Ophidion holbrookii
  #Anthias tenuis and Anthias tenuis and woodsi both fix to Choranthias tenuis
  #Multiple genuses resolve together (Cynoscion, Bothus, Opsanus)

#User decisions with what to do with repeats due to taxonomic classifications
#depend on goals of data use, and therefore are maintained in FishGlob data product



########## A. Fredston, August 2025: resolving issue #49 where haul_id value is a numeric, see https://github.com/AquaAuma/FishGlob_data/issues/49 
class(clean_gmex$haul_id) 
head(clean_gmex$haul_id) 

clean_gmex_fixed_haul_id <- clean_gmex |> 
  mutate(haul_id = paste0("id", haul_id))
class(clean_gmex_fixed_haul_id$haul_id)
head(clean_gmex_fixed_haul_id$haul_id)
###########




# -------------------------------------------------------------------------------------#
#### SAVE DATABASE IN GOOGLE DRIVE ####
# -------------------------------------------------------------------------------------#

# Just run this routine should be good for all
write_clean_data(data = clean_gmex, survey = "GMEX", overwrite = T, csv = T)



# -------------------------------------------------------------------------------------#
#### FLAGS ####
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
regions <- levels(as.factor(clean_gmex$survey))

#run flag_spp function in a loop
for (r in regions) {
  flag_spp(clean_gmex, r)
}

######### Apply trimming per survey_unit method 1
#apply trimming for hex size 7
dat_new_method1_hex7 <- apply_trimming_per_survey_unit_method1(clean_gmex, 7)

#apply trimming for hex size 8
dat_new_method1_hex8 <- apply_trimming_per_survey_unit_method1(clean_gmex, 8)

######### Apply trimming per survey_unit method 2
dat_new_method2 <- apply_trimming_per_survey_unit_method2(clean_gmex)


#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#
surveys <- sort(unique(clean_gmex$survey))
survey_units <- sort(unique(clean_gmex$survey_unit))
survey_std <- clean_gmex %>% 
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
                           sep = ";", colClasses=c(haul_id = "character"))
    hex_res7_0 <- as.vector(hex_res7_0[,1])
    
    hex_res7_2 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res7/",
                                  survey_units[i], "_hex_res_7_trimming_02_hauls_removed.csv"),
                           sep = ";", colClasses=c(haul_id = "character"))
    hex_res7_2 <- as.vector(hex_res7_2[,1])
    
    hex_res8_0 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_0_hauls_removed.csv"),
                           sep= ";", colClasses=c(haul_id = "character"))
    hex_res8_0 <- as.vector(hex_res8_0[,1])
    
    hex_res8_2 <- read.csv(paste0("outputs/Flags/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_02_hauls_removed.csv"),
                           sep = ";", colClasses=c(haul_id = "character"))
    hex_res8_2 <- as.vector(hex_res8_2[,1])
    
    trim_2 <- read.csv(paste0("outputs/Flags/trimming_method2/",
                              survey_units[i],"_hauls_removed.csv"), colClasses=c(haul_id_removed = "character"))
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

# verify that the flagging worked. these values should match the respective _stats_hauls.csv files in outputs/Flags/trimming_methods1 and 2
survey_std |>
  group_by(survey_unit) |>
  distinct(haul_id, flag_trimming_hex7_0, flag_trimming_hex7_2, flag_trimming_hex8_0, flag_trimming_hex8_2, flag_trimming_2) |>
  summarize(hex7_0 = sum(!is.na(flag_trimming_hex7_0)),
            hex7_2 = sum(!is.na(flag_trimming_hex7_2)),
            hex8_0 = sum(!is.na(flag_trimming_hex8_0)),
            hex8_2 = sum(!is.na(flag_trimming_hex8_2)),
            trim_2 = sum(!is.na(flag_trimming_2)))



########## A. Fredston, August 2025: resolving issue #49 where haul_id value is a numeric, see https://github.com/AquaAuma/FishGlob_data/issues/49 
class(survey_std$haul_id) 
head(survey_std$haul_id) 

survey_std_fixed_haul_id <- survey_std |> 
  mutate(haul_id = paste0("id", haul_id))
class(survey_std_fixed_haul_id$haul_id)
head(survey_std_fixed_haul_id$haul_id)
###########


# Just run this routine should be good for all
write_clean_data(data = survey_std_fixed_haul_id, survey = "GMEX_std",
                 overwrite = T, rdata=TRUE)
