## ---- oceanadapt

# OceanAdapt requires the following verisons of packages. These versions are based on the last successful date that the script ran.  This will install these versions on your machine, proceed with caution.  The dates on the following lines can be updated if the script successfully runs with different versions on a subsequent date.
library(devtools)
install_version("tidyverse", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("lubridate", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("PBSmapping", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("data.table", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("gridExtra", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("questionr", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("geosphere", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")
install_version("here", repos = "https://mran.revolutionanalytics.com/snapshot/2019-12-05/")

# Load required packages 
library(tidyverse)
library(lubridate)
library(PBSmapping) 
library(gridExtra) 
library(questionr) 
library(geosphere)
library(here)
library(dplyr)
library(data.table) 

# If running from R instead of RStudio, please set the working directory to the folder containing this script before running this script.
# This script is designed to run within the following directory structure:
# Directory 1 contains:
# 1. compile.R script - this script
# 2. data_raw directory - folder containing all raw data files
# 3. R directory - folder containing scripts used in the making of this script

# The zip file you downloaded created this directory structure for you.

# a note on species name adjustment #### 
# At some point during certain surveys it was realized that what was believed to be one species was actually a different species or more than one species.  Species have been lumped together as a genus in those instances.

# Answer the following questions using all caps TRUE or FALSE to direct the actions of the script =====================================

# 1. Some strata and years have very little data, should they be removed and saved as fltr data? #DEFAULT: TRUE. 
HQ_DATA_ONLY <- TRUE

# 2. View plots of removed strata for HQ_DATA. #OPTIONAL, DEFAULT:FALSE
# It takes a while to generate these plots.
HQ_PLOTS <- FALSE

# 3. Remove ai,ebs,gmex,goa,neus,seus,wcann,wctri, scot. Keep `dat`. #DEFAULT: FALSE 
REMOVE_REGION_DATASETS <- FALSE

# 4. Create graphs based on the data similar to those shown on the website and outputs them to pdf. #DEFAULT:FALSE
PLOT_CHARTS <- FALSE
# This used to be called OPTIONAL_PLOT_CHARTS, do I need to change it back?

# 5. If you would like to write out the clean data, would you prefer it in Rdata or CSV form?  Note the CSV's are much larger than the Rdata files. #DEFAULT:TRUE, FALSE generates CSV's instead of Rdata.
PREFER_RDATA <- TRUE

# 6. Output the clean full master data frame. #DEFAULT:FALSE
WRITE_MASTER_DAT <- TRUE
# This used to be called OPTIONAL_OUTPUT_DAT_MASTER_TABLE, do I need to change the name back?

# 7. Output the clean trimmed data frame. #DEFAULT:FALSE
WRITE_TRIMMED_DAT <- TRUE

# 7. Generate dat.exploded table. #OPTIONAL, DEFAULT:TRUE
DAT_EXPLODED <- TRUE

# 9. Output the dat.exploded table #DEFAULT:FALSE
WRITE_DAT_EXPLODED <- FALSE

# 10. Output the BY_SPECIES, BY_REGION, and BY_NATIONAL tables. #DEFAULT:FALSE
WRITE_BY_TABLES <- TRUE


# Workspace setup ---------------------------------------------------------
print("Workspace setup")

# This script works best when the repository is downloaded from github, 
# especially when that repository is loaded as a project into RStudio.

# The working directory is assumed to be the OceanAdapt directory of this repository.
# library(tidyverse)# use ggplot2, tibble, readr, dplyr, stringr, purrr


# Functions ===========================================================
print("Functions")

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

sumna <- function(x){
  #acts like sum(na.rm=T) but returns NA if all are NA
  if(!all(is.na(x))) return(sum(x, na.rm=T))
  if(all(is.na(x))) return(NA)
}

meanna = function(x){
  if(!all(is.na(x))) return(mean(x, na.rm=T))
  if(all(is.na(x))) return(NA)
}

# weighted mean for use with summarize(). values in col 1, weights in col 2
wgtmean = function(x, na.rm=FALSE) {questionr::wtd.mean(x=x[,1], weights=x[,2], na.rm=na.rm)}

wgtse = function(x, na.rm=TRUE){ 
  if(sum(!is.na(x[,1]) & !is.na(x[,2]))>1){
    if(na.rm){
      return(sqrt(wtd.var(x=x[,1], weights=x[,2], na.rm=TRUE, normwt=TRUE))/sqrt(sum(!is.na(x[,1] & !is.na(x[,2])))))
    } else {
      return(sqrt(wtd.var(x=x[,1], weights=x[,2], na.rm=FALSE, normwt=TRUE))/sqrt(length(x))) # may choke on wtd.var without removing NAs
    }
  } else {
    return(NA) # NA if vector doesn't have at least 2 values
  }
}

se <- function(x) sd(x)/sqrt(length(x)) # assumes no NAs

lunique = function(x) length(unique(x)) # number of unique values in a vector

present_every_year <- function(dat, ...){
  presyr <- dat %>% 
    filter(wtcpue > 0) %>% 
    group_by(...) %>% 
    summarise(pres = n())
  return(presyr)
}

num_year_present <- function(presyr, ...){
  presyrsum <- presyr %>% 
    filter(pres > 0) %>% 
    group_by(...) %>% 
    summarise(presyr = n()) 
  return(presyrsum)
}

max_year_surv <- function(presyrsum, ...){
  maxyrs <- presyrsum %>% 
    group_by(...) %>% 
    summarise(maxyrs = max(presyr))
  return(maxyrs)
  
}

explode0 <- function(x, by=c("region")){
  # x <- copy(x)
  stopifnot(is.data.table(x))
  
  # print(x[1])
  
  # x <- as.data.table(x)
  # x <- as.data.table(trimmed_dat)[region=="Scotian Shelf Summer"]
  # setkey(x, haulid, stratum, year, lat, lon, stratumarea, depth)
  # group the data by these columns
  setorder(x, haulid, stratum, year, lat, lon, stratumarea, depth)
  
  # pull out all of the unique spp
  u.spp <- x[,as.character(unique(spp))]
  # pull out all of the unique common names
  u.cmmn <- x[,common[!duplicated(as.character(spp))]]
  
  # pull out these location related columns and sort by haul_id and year
  x.loc <- x[,list(haulid, year, stratum, stratumarea, lat, lon, depth)]
  setkey(x.loc, haulid, year)
  
  # attatch all spp to all locations
  x.skele <- x.loc[,list(spp=u.spp, common=u.cmmn), by=eval(colnames(x.loc))]
  setkey(x.skele, haulid, year, spp)
  x.skele <- unique(x.skele)
  setcolorder(x.skele, c("haulid","year","spp", "common", "stratum", "stratumarea","lat","lon","depth"))
  
  # pull in multiple observations of the same species 
  x.spp.dat <- x[,list(haulid, year, spp, wtcpue)]
  setkey(x.spp.dat, haulid, year, spp)
  x.spp.dat <- unique(x.spp.dat)
  
  out <- x.spp.dat[x.skele, allow.cartesian = TRUE]
  
  out$wtcpue[is.na(out$wtcpue)] <- 0
  
  out
}

#convert factors to numeric

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Reformat string - first letter uppercase
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#add one to odd numbers
oddtoeven <- function(x) {
  ifelse(x %% 2 == 1,x+1,x)
}

#add one to even numbers
eventoodd <- function(x) {
  ifelse(x %% 2 == x+1,1,x)
}


# Compile AI =====================================================
print("Compile AI")

## Special fix
#there is a comment that contains a comma in the 2014-2018 file that causes the delimiters to read incorrectly.  Fix that here::here:
aiURL <- "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/ai2014_2018.csv"

temp <- read_lines(aiURL)
# replace the string that causes the problem
temp_fixed <- stringr::str_replace_all(temp, "Stone et al., 2011", "Stone et al. 2011")
# read the result in as a csv
temp_csv <- read_csv(temp_fixed)
## End special fix

ai83_00 <- "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/ai1983_2000.csv"
ai02_12 <- "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/ai2002_2012.csv"


files <- as.list(c(ai83_00, ai02_12))

# exclude the strata file and the raw 2014-2016 data file which has been fixed in temp_csv
#files <- files[-c(grep("strata", files),grep("2014", files))]

# combine all of the data files into one table
ai_data <- files %>% 
  # read in all of the csv's in the files list
  map_dfr(read_csv) %>%
  # add in the data fixed above
  rbind(temp_csv) %>% 
  # remove any data rows that have headers as data rows
  filter(LATITUDE != "LATITUDE", !is.na(LATITUDE)) %>% 
  mutate(stratum = as.integer(STRATUM)) %>% 
  # remove unused columns
  #select(-STATION, -DATETIME, -NUMCPUE, -SID, -BOT_TEMP, -SURF_TEMP, -STRATUM) %>% 
  # remove any extra white space from around spp and common names
  mutate(COMMON = str_trim(COMMON), 
         SCIENTIFIC = str_trim(SCIENTIFIC))

# The warning of 13 parsing failures is pointing to a row in the middle of the data set that contains headers instead of the numbers expected, this row is removed by the filter above.

aistrat <- "https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/ai_strata.csv"

ai_strata <- read_csv(aistrat, col_types = cols(NPFMCArea = col_character(),
                                                                                SubareaDescription = col_character(),
                                                                                StratumCode = col_integer(),
                                                                                DepthIntervalm = col_character(),
                                                                                Areakm2 = col_integer()
))  %>% 
  #select(StratumCode, Areakm2) %>% 
  mutate(stratum = StratumCode)


ai <- left_join(ai_data, ai_strata, by = "stratum")



# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(ai, is.na(Areakm2))) == 0)

# the following chunk of code reformats and fixes this region's data
ai <- ai %>%
  mutate(
    # Create a unique haulid
    haulid = paste(formatC(VESSEL, width=3, flag=0), CRUISE, formatC(HAUL, width=3, flag=0), LONGITUDE, LATITUDE, sep=''),    
    wtcpue = ifelse(WTCPUE == "-9999", NA, WTCPUE)) %>% 
  rename(year = YEAR, 
         latitude = LATITUDE, 
         longitude = LONGITUDE, 
         depth = BOT_DEPTH, 
         spp = SCIENTIFIC, 
         area_swept = Areakm2,
         station = STATION,
         sbt = BOT_TEMP,
         sst = SURF_TEMP,
         num_cpue = NUMCPUE,
         wgt_cpue = WTCPUE) %>% 
  #convert date to month and day columns 
  mutate(datetime = mdy_hm(DATETIME),
         month = month(datetime),
         day = day(datetime),
         quarter = case_when(month %in% c(1,2,3) ~ 1,
                             month %in% c(4,5,6) ~ 2,
                             month %in% c(7,8,9) ~ 3,
                             month %in% c(10,11,12) ~ 4),
         season = 'NA',
         haul_dur = NA,
         gear = NA,
         num = NA,
         num_h = NA,
         wgt = NA,
         wgt_h = NA,
  ) %>%
  # remove non-fish
  filter(
    spp != '' & 
      !grepl("egg", spp)) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    spp = ifelse(grepl("Myoxocephalus", spp ) & !grepl("scorpius", spp), "Myoxocephalus sp.", spp),
    spp = ifelse(grepl("Bathyraja", spp) & !grepl("panthera", spp), 'Bathyraja sp.', spp)
  ) %>% 
  type_convert(col_types = cols(
    latitude = col_double(),
    longitude = col_double(),
    station = col_character(),
    year = col_integer(),
    DATETIME = col_character(),
    wgt_cpue = col_double(),
    num_cpue = col_double(),
    COMMON = col_character(),
    spp = col_character(),
    SID = col_integer(),
    depth = col_integer(),
    sbt = col_double(),
    sst = col_double(),
    VESSEL = col_integer(),
    CRUISE = col_integer(),
    HAUL = col_integer(),
    haulid = col_character()
  ))  %>% 
  #group_by(haulid, stratum, stratumarea, year, latitude, longitude, depth, spp) %>% 
  #  summarise(wtcpue = sumna(wtcpue)) %>% 
  mutate(survey = "AI",
         country = "United States",
         sub_area = NA,
         continent = "n_america",
         stat_rec = NA,
         verbatim_name = spp) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)  %>%  #keep SID?
  #select(survey, haulid, year, latitude, longitude, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()

if (HQ_DATA_ONLY == TRUE){
  
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  # plot the strata by year
  
  p1 <- ai %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- ai %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- ai %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>% 
    filter(count >= 13)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- ai %>% 
    filter(stratum %in% test$stratum)
  nrow(ai) - nrow(test2)
  # percent that will be lost
  print((nrow(ai) - nrow(test2))/nrow(ai))
  # 0% of rows are removed
  ai_fltr <- ai %>% 
    filter(stratum %in% test$stratum)
  
  # plot the results after editing
  p3 <- ai_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- ai_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "ai_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}# clean up
rm(ai_data, ai_strata, files, temp_fixed, temp_csv)

# Compile EBS ============================================================
print("Compile EBS")
files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs1982_1984.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs1985_1989.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs1990_1994.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs1995_1999.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs2000_2004.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs2005_2008.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs2009_2012.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs2013_2016.csv",
             "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs2017_2019.csv")


# exclude the strata file
#files <- files[-grep("strata", files)]

# combine all of the data files into one table
ebs_data <- files %>% 
  # read in all of the csv's in the files list
  map_dfr(read_csv) %>%
  # remove any data rows that have headers as data rows
  filter(LATITUDE != "LATITUDE", !is.na(LATITUDE)) %>% 
  mutate(stratum = as.integer(STRATUM))  %>% 
  # remove unused columns
  #select(-STATION, -DATETIME, -NUMCPUE, -SID, -BOT_TEMP, -SURF_TEMP, -STRATUM) %>% 
  # remove any extra white space from around spp and common names
  mutate(COMMON = str_trim(COMMON), 
         SCIENTIFIC = str_trim(SCIENTIFIC))

# import the strata data
ebsstrat <- "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/ebs_strata.csv"
ebs_strata <- read_csv(ebsstrat, col_types = cols(
  SubareaDescription = col_character(),
  StratumCode = col_integer(),
  Areakm2 = col_integer()
)) %>% 
  #select(StratumCode, Areakm2) %>% 
  rename(stratum = StratumCode)

ebs <- left_join(ebs_data, ebs_strata, by = "stratum")

# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(ebs, is.na(Areakm2))) == 0)

ebs <- ebs %>%
  mutate(
    # Create a unique haulid
    haulid = paste(formatC(VESSEL, width=3, flag=0), CRUISE, formatC(HAUL, width=3, flag=0), LONGITUDE, LATITUDE, sep=''),    
    wtcpue = ifelse(WTCPUE == "-9999", NA, WTCPUE)) %>% 
  rename(year = YEAR, 
         latitude = LATITUDE, 
         longitude = LONGITUDE, 
         depth = BOT_DEPTH, 
         spp = SCIENTIFIC, 
         area_swept = Areakm2,
         station = STATION,
         sbt = BOT_TEMP,
         sst = SURF_TEMP,
         num_cpue = NUMCPUE,
         wgt_cpue = WTCPUE) %>% 
  #convert date to month and day columns 
  mutate(datetime = mdy_hm(DATETIME),
         month = month(datetime),
         day = day(datetime),
         quarter = case_when(month %in% c(1,2,3) ~ 1,
                             month %in% c(4,5,6) ~ 2,
                             month %in% c(7,8,9) ~ 3,
                             month %in% c(10,11,12) ~ 4),
         season = 'NA',
         haul_dur = NA,
         gear = NA,
         num = NA,
         num_h = NA,
         wgt = NA,
         wgt_h = NA,
  ) %>%
  # remove non-fish
  filter(
    spp != '' & 
      !grepl("egg", spp)) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    spp = ifelse(grepl("Myoxocephalus", spp ) & !grepl("scorpius", spp), "Myoxocephalus sp.", spp),
    spp = ifelse(grepl("Bathyraja", spp) & !grepl("panthera", spp), 'Bathyraja sp.', spp)
  ) %>% 
  type_convert(col_types = cols(
    latitude = col_double(),
    longitude = col_double(),
    station = col_character(),
    year = col_integer(),
    DATETIME = col_character(),
    wgt_cpue = col_double(),
    num_cpue = col_double(),
    COMMON = col_character(),
    spp = col_character(),
    SID = col_integer(),
    depth = col_integer(),
    sbt = col_double(),
    sst = col_double(),
    VESSEL = col_integer(),
    CRUISE = col_integer(),
    HAUL = col_integer(),
    haulid = col_character()
  ))  %>% 
  #group_by(haulid, stratum, stratumarea, year, latitude, longitude, depth, spp) %>% 
  #  summarise(wtcpue = sumna(wtcpue)) %>% 
  mutate(survey = "EBS",
         country = "United States",
         sub_area = NA,
         continent = "n_america",
         stat_rec = NA,
         verbatim_name = spp) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name) %>% #keep SID?
  #select(survey, haulid, year, latitude, longitude, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  p1 <- ebs %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- ebs %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- ebs %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n())  %>% 
    filter(count >= 36)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- ebs %>% 
    filter(stratum %in% test$stratum)
  nrow(ebs) - nrow(test2)
  # percent that will be lost
  print((nrow(ebs) - nrow(test2))/nrow(ebs))
  # 4.7% of rows are removed
  ebs_fltr <- ebs %>% 
    filter(stratum %in% test$stratum)
  
  p3 <- ebs_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year))) +
    geom_jitter()
  
  p4 <- ebs_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "ebs_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}
# clean up
rm(files, ebs_data, ebs_strata)


# Compile GOA =============================================================
print("Compile GOA")

files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa1984_1987.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa1990_1999.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa2001_2005.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa2007_2013.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa2015_2019.csv")


# exclude the 2 strata files; the 1 and 2 elements
#files <- files[-grep("strata", files)]

# combine all of the data files into one table
goa_data <- files %>% 
  # read in all of the csv's in the files list
  map_dfr(read_csv) %>%
  # remove any data rows that have headers as data rows
  filter(LATITUDE != "LATITUDE", !is.na(LATITUDE)) %>% 
  mutate(stratum = as.integer(STRATUM)) #%>% 
  # remove unused columns
  #select(-STATION, -DATETIME, -NUMCPUE, -SID, -BOT_TEMP, -SURF_TEMP, -STRATUM)

# import the strata data
file <- "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/goa_strata.csv"

goa_strata <- file %>% 
  # read in all of the csv's in the files list
  map_dfr(read_csv) %>% 
  #select(StratumCode, Areakm2) %>% 
  distinct() %>% 
  rename(stratum = StratumCode)

goa <- left_join(goa_data, goa_strata, by = "stratum")

# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(goa, is.na(Areakm2))) == 0)


goa <- goa %>%
  mutate(
    # Create a unique haulid
    haulid = paste(formatC(VESSEL, width=3, flag=0), CRUISE, formatC(HAUL, width=3, flag=0), LONGITUDE, LATITUDE, sep=''),    
    wtcpue = ifelse(WTCPUE == "-9999", NA, WTCPUE)) %>% 
  rename(year = YEAR, 
         latitude = LATITUDE, 
         longitude = LONGITUDE, 
         depth = BOT_DEPTH, 
         spp = SCIENTIFIC, 
         area_swept = Areakm2,
         station = STATION,
         sbt = BOT_TEMP,
         sst = SURF_TEMP,
         num_cpue = NUMCPUE,
         wgt_cpue = WTCPUE) %>% 
  #convert date to month and day columns 
  mutate(datetime = mdy_hm(DATETIME),
         month = month(datetime),
         day = day(datetime),
         quarter = case_when(month %in% c(1,2,3) ~ 1,
                             month %in% c(4,5,6) ~ 2,
                             month %in% c(7,8,9) ~ 3,
                             month %in% c(10,11,12) ~ 4),
         season = 'NA',
         haul_dur = NA,
         gear = NA,
         num = NA,
         num_h = NA,
         wgt = NA,
         wgt_h = NA,
  ) %>%
  # remove non-fish
  filter(
    spp != '' & 
      !grepl("egg", spp)) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    spp = ifelse(grepl("Myoxocephalus", spp ) & !grepl("scorpius", spp), "Myoxocephalus sp.", spp),
    spp = ifelse(grepl("Bathyraja", spp) & !grepl("panthera", spp), 'Bathyraja sp.', spp)
  ) %>% 
  type_convert(col_types = cols(
    latitude = col_double(),
    longitude = col_double(),
    station = col_character(),
    year = col_integer(),
    DATETIME = col_character(),
    wgt_cpue = col_double(),
    num_cpue = col_double(),
    COMMON = col_character(),
    spp = col_character(),
    SID = col_integer(),
    depth = col_integer(),
    sbt = col_double(),
    sst = col_double(),
    VESSEL = col_integer(),
    CRUISE = col_integer(),
    HAUL = col_integer(),
    haulid = col_character()
  ))  %>% 
  #group_by(haulid, stratum, stratumarea, year, latitude, longitude, depth, spp) %>% 
#  summarise(wtcpue = sumna(wtcpue)) %>% 
  mutate(survey = "GOA",
         country = "United States",
         sub_area = NA,
         continent = "n_america",
         stat_rec = NA,
         verbatim_name = spp) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name) %>% #keep SID?
  #select(survey, haulid, year, latitude, longitude, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  p1 <- goa %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- goa %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  # for GOA in 2018, 2001 missed 27 strata and will be removed, stratum 50 is
  # missing from 3 years but will be kept, 410, 420, 430, 440, 450 are missing 
  #from 3 years but will be kept, 510 and higher are missing from 7 or more years
  # of data and will be removed
  test <- goa %>%
    filter(year != 2001) %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n())  %>%
    filter(count >= 14)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- goa %>% 
    filter(stratum %in% test$stratum)
  nrow(goa) - nrow(test2)
  # percent that will be lost
  print ((nrow(goa) - nrow(test2))/nrow(goa))
  # 4% of rows are removed
  goa_fltr <- goa %>% 
    filter(stratum %in% test$stratum) %>%
    filter(year != 2001)
  
  p3 <-  goa_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- goa_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "goa_hq_dat_removed.png"))
    
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}
rm(files, goa_data, goa_strata)


# Compile WCTRI ===========================================================
print("Compile WCTRI")

wctri_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wctri_catch.csv", col_types = cols(
  CRUISEJOIN = col_integer(),
  HAULJOIN = col_integer(),
  CATCHJOIN = col_integer(),
  REGION = col_character(),
  VESSEL = col_integer(),
  CRUISE = col_integer(),
  HAUL = col_integer(),
  SPECIES_CODE = col_integer(),
  WEIGHT = col_double(),
  NUMBER_FISH = col_integer(),
  SUBSAMPLE_CODE = col_character(),
  VOUCHER = col_character(),
  AUDITJOIN = col_integer()
)) #%>% 
  #select(CRUISEJOIN, HAULJOIN, VESSEL, CRUISE, HAUL, SPECIES_CODE, WEIGHT, NUMBER_FISH, REGION)

wctri_haul <- read_csv("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/wctri_haul.csv", col_types = 
                         cols(
                           CRUISEJOIN = col_integer(),
                           HAULJOIN = col_integer(),
                           REGION = col_character(),
                           VESSEL = col_integer(),
                           CRUISE = col_integer(),
                           HAUL = col_integer(),
                           HAUL_TYPE = col_integer(),
                           PERFORMANCE = col_double(),
                           START_TIME = col_character(),
                           DURATION = col_double(),
                           DISTANCE_FISHED = col_double(),
                           NET_WIDTH = col_double(),
                           NET_MEASURED = col_character(),
                           NET_HEIGHT = col_double(),
                           STRATUM = col_integer(),
                           START_LATITUDE = col_double(),
                           END_LATITUDE = col_double(),
                           START_LONGITUDE = col_double(),
                           END_LONGITUDE = col_double(),
                           STATIONID = col_character(),
                           GEAR_DEPTH = col_integer(),
                           BOTTOM_DEPTH = col_integer(),
                           BOTTOM_TYPE = col_integer(),
                           SURFACE_TEMPERATURE = col_double(),
                           GEAR_TEMPERATURE = col_double(),
                           WIRE_LENGTH = col_integer(),
                           GEAR = col_integer(),
                           ACCESSORIES = col_integer(),
                           SUBSAMPLE = col_integer(),
                           AUDITJOIN = col_integer()
                         )) #%>% 
  #select(CRUISEJOIN, HAULJOIN, VESSEL, CRUISE, HAUL, HAUL_TYPE, PERFORMANCE, START_TIME, DURATION, DISTANCE_FISHED, NET_WIDTH, STRATUM, START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID, BOTTOM_DEPTH, SURFACE_TEMPERATURE)

wctri_species <- read_csv("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/wctri_species.csv", col_types = cols(
  SPECIES_CODE = col_integer(),
  SPECIES_NAME = col_character(),
  COMMON_NAME = col_character(),
  REVISION = col_character(),
  BS = col_character(),
  GOA = col_character(),
  WC = col_character(),
  AUDITJOIN = col_integer()
)) #%>% 
  #select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME)

# Add haul info to catch data
wctri <- left_join(wctri_catch, wctri_haul, by = c("CRUISEJOIN", "HAULJOIN", "VESSEL", "CRUISE", "HAUL"))
#  add species names
wctri <- left_join(wctri, wctri_species, by = "SPECIES_CODE")


wctri <- wctri %>% 
  # trim to standard hauls and good performance
  filter(HAUL_TYPE == 3 & PERFORMANCE == 0) %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(VESSEL, width=3, flag=0), CRUISE, formatC(HAUL, width=3, flag=0), START_LONGITUDE, START_LATITUDE, sep=''),
    # Extract year where needed
    year = substr(CRUISE, 1, 4),
    month = substr(CRUISE, 5,6),
    day = NA,
    quarter = case_when(month %in% c(1,2,3) ~ 1,
                        month %in% c(4,5,6) ~ 2,
                        month %in% c(7,8,9) ~ 3,
                        month %in% c(10,11,12) ~ 4),
    season = NA_character_,
    haul_dur = DURATION*60, #hours to minutes
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(START_LATITUDE)+0.5, floor(BOTTOM_DEPTH/100)*100 + 50, sep= "-"), 
    area_swept = DISTANCE_FISHED*(NET_WIDTH/1000),
    # adjust for tow area # weight per km (1000 m2)	
    wgt_cpue = WEIGHT/area_swept,
    #wgt_cpue = (WEIGHT*10000)/(DISTANCE_FISHED*10*NET_WIDTH),
    wgt_h = WEIGHT/DURATION, #kg/hour
    num_h = NUMBER_FISH/DURATION, # ind/hour
    num_cpue = NUMBER_FISH/area_swept #ind/km2
  )

# Calculate stratum area where needed (use convex hull approach)
wctri_strats <- wctri %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(START_LONGITUDE, START_LATITUDE))

wctri <- left_join(wctri, wctri_strats, by = "stratum")

wctri <- wctri %>% 
  rename(
    svvessel = VESSEL,
    latitude = START_LATITUDE, 
    longitude = START_LONGITUDE,
    depth = BOTTOM_DEPTH, 
    spp = SPECIES_NAME,
    sst = SURFACE_TEMPERATURE,
    sbt = NA,
    num = NUMBER_FISH,
    gear = GEAR,
    station = STATIONID,
    verbatim_name = SPECIES_NAME
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add survey column
  mutate(survey = "WCTRI",
         country = "United States",
         continent = "n_america",
         sub_area = NA,
         stat_rec = NA) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
        num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)  %>% 
  ungroup()

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  
  p1 <- wctri %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- wctri %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- wctri %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 10)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- wctri %>% 
    filter(stratum %in% test$stratum)
  nrow(wctri) - nrow(test2)
  # percent that will be lost
  print((nrow(wctri) - nrow(test2))/nrow(wctri))
  # 23% of rows are removed
  wctri_fltr <- wctri %>% 
    filter(stratum %in% test$stratum)
  
  p3 <- wctri_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- wctri_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "wctri_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}

rm(wctri_catch, wctri_haul, wctri_species, wctri_strats)

# Compile WCANN ===========================================================
print("Compile WCANN")

temp <- tempfile()
download.file("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wcann_catch.csv.zip", temp)

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
)) #%>% 
  #select("trawl_id","year","longitude_dd","latitude_dd","depth_m","scientific_name","total_catch_wt_kg","cpue_kg_per_ha_der",
        # "cpue_numbers_per_ha_der","date_yyyymmdd","station_code", "partition")

wcann_haul <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/wcann_haul.csv", col_types = cols(
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
)) #%>% 
  #select("trawl_id","year","longitude_hi_prec_dd","latitude_hi_prec_dd","depth_hi_prec_m","area_swept_ha_der")
# It is ok to get warning message that missing column names filled in: 'X1' [1].

wcann <- left_join(wcann_haul, wcann_catch, by = c("trawl_id", "year", "date_yyyymmdd", "station_code"))
wcann <- wcann %>% 
  mutate(
    # create haulid
    haulid = trawl_id,
    # Add "strata" (define by lat, lon and depth bands) where needed # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(latitude_dd)+0.5, floor(depth_m/100)*100 + 50, sep= "-"), 
    # adjust for tow area # kg per km2 (hectare/100 = km2)
    area_swept = (area_swept_ha_der/100),
    wgt_cpue = total_catch_wt_kg/(area_swept_ha_der/100),
    num_cpue = total_catch_numbers/(area_swept_ha_der/100),
    wgt_h = total_catch_wt_kg/sample_duration_hr_der,
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

wcann_strats <- wcann %>% 
  filter(!is.na(longitude_dd)) %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(longitude_dd, latitude_dd), na.rm = T)

wcann <- left_join(wcann, wcann_strats, by = "stratum")

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
         !grepl("crushed", spp)) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
    spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp)
  ) %>%
  # add survey column and fill missing columns
  mutate(survey = "WCANN",
         sbt = NA,
         sst = NA,
         country = "United States",
         continent = "n_america",
         sub_area = NA,
         stat_rec = NA,
         verbatim_name = spp,
         gear = NA) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)
  #many rows with missing num_h, num_cpue, wgt_h, and wgt_cpue values due to missing haul_dur
  
#%>% 
  #group_by(haulid, stratum, area_swept, year, latitude, longitude, depth, verbatim_name) %>% 
  #summarise(wgt_cpue = sumna(wgt_cpue)) %>% 
  #ungroup()


if (HQ_DATA_ONLY == TRUE){
  # keep the same footprint as wctri
  # how many rows of data will be lost?
  nrow(wcann) - nrow(filter(wcann, stratum %in% wctri$stratum))
  # percent that will be lost - 61% !
  (nrow(wcann) - nrow(filter(wcann, stratum %in% wctri$stratum)))/nrow(wcann)
  
  wcann_fltr <- wcann %>% 
    filter(stratum %in% wcann$stratum)
  
  # see what these data look like - pretty solid
  p1 <- wcann_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- wcann_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "wcann_hq_dat_removed.png"))
    rm(temp)
  }
  rm(p1, p2)
}

# cleanup
rm(wcann_catch, wcann_haul, wcann_strats)

# Compile GMEX ===========================================================
print("Compile GMEX")

gmex_station_raw <- read_lines("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_STAREC.csv")
# remove oddly quoted characters
#gmex_station_clean <- str_replace_all(gmex_station_raw, "\\\\\\\"", "\\\"\\\"")
gmex_station_clean <- str_replace_all(gmex_station_raw, "\\\\\"", "")
#gmex_station_clean <- gsub('\"', "", gmex_station_clean)

gmex_station <- read_csv(gmex_station_clean, col_types = cols(.default = col_character())) %>% 
  select('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', 'VESSEL_SPD', 'COMSTAT')

problems <- problems(gmex_station) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)

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
  MO_DAY_YR = col_date(format = ""),
  VESSEL_SPD = col_double(),
  COMSTAT = col_character()
))


gmex_tow <-read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_INVREC.csv", col_types = cols(
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
  select('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP') %>%
  filter(GEAR_TYPE=='ST')

problems <- problems(gmex_tow) %>% 
  filter(!is.na(col)) 
stopifnot(nrow(problems) == 2)
# 2 problems are that there are weird delimiters in the note column COMBIO, ignoring for now.

gmex_spp <-read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_NEWBIOCODESBIG.csv", col_types = cols(
  Key1 = col_integer(),
  TAXONOMIC = col_character(),
  CODE = col_integer(),
  TAXONSIZECODE = col_character(),
  isactive = col_integer(),
  common_name = col_character(),
  tsn = col_integer(),
  tsn_accepted = col_integer(),
  X9 = col_character()
)) %>% 
  select(-X9, -tsn_accepted)

# problems should be 0 obs
problems <- problems(gmex_spp) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)
gmex_cruise <-read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_CRUISES.csv", col_types = cols(.default = col_character())) %>% 
  select(CRUISEID, VESSEL, TITLE, SOURCE)

# problems should be 0 obs
problems <- problems(gmex_cruise) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)
gmex_cruise <- type_convert(gmex_cruise, col_types = cols(CRUISEID = col_integer(), VESSEL = col_integer(), TITLE = col_character()))

temp <- tempfile()
download.file("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/gmex_BGSREC.csv.zip", temp)
gmex_bio <-read_csv(unz(temp, "gmex_BGSREC.csv"), col_types = cols(.default = col_character())) %>% 
  select('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'GENUS_BGS', 'SPEC_BGS', 'BGSCODE', 'BIO_BGS', 'SELECT_BGS') %>%
  # trim out young of year records (only useful for count data) and those with UNKNOWN species
  filter(BGSCODE != "T" | is.na(BGSCODE),
         GENUS_BGS != "UNKNOWN" | is.na(GENUS_BGS))  %>%
  # remove the few rows that are still duplicates
  distinct()

# problems should be 0 obs
problems <- problems(gmex_bio) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)

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

# make two combined records where 2 different species share the same species code
newspp <- tibble(
  Key1 = c(503,5770), 
  TAXONOMIC = c('ANTHIAS TENUIS AND WOODSI', 'MOLLUSCA AND UNID.OTHER #01'), 
  CODE = c(170026003, 300000000), 
  TAXONSIZECODE = NA, 
  isactive = -1, 
  common_name = c('threadnose and swallowtail bass', 'molluscs or unknown'), 
  tsn = NA) 

# remove the duplicates that were just combined  
gmex_spp <- gmex_spp %>% 
  distinct(CODE, .keep_all = T)
# add the combined records on to the end. trim out extra columns from gmexspp
gmex_spp <- rbind(gmex_spp, newspp) %>% 
  select(CODE, TAXONOMIC) %>% 
  rename(BIO_BGS = CODE)

# merge tow information with catch data, but only for shrimp trawl tows (ST)
gmex <- left_join(gmex_bio, gmex_tow, by = c("STATIONID", "CRUISE_NO", "P_STA_NO")) %>% 
  # add station location and related data
  left_join(gmex_station, by = c("CRUISEID", "STATIONID", "CRUISE_NO", "P_STA_NO")) %>% 
  # add scientific name
  left_join(gmex_spp, by = "BIO_BGS") %>% 
  # add cruise title
  left_join(gmex_cruise, by = c("CRUISEID", "VESSEL"))


gmex <- gmex %>% 
  # Trim to high quality SEAMAP summer trawls, based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas
  filter(grepl("Summer", TITLE) & 
           GEAR_SIZE == 40 & 
           MESH_SIZE == 1.63 &
           # OP has no letter value
           !grepl("[A-Z]", OP)) %>% 
  mutate(
    # Create a unique haulid
    haulid = paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE_NO, width=3, flag=0), formatC(P_STA_NO, width=5, flag=0, format='d'), S_LATD, S_LOND,sep=''), 
    # Extract year where needed
    year = year(MO_DAY_YR),
    month = month(MO_DAY_YR),
    day = day(MO_DAY_YR),
    quarter = case_when(month %in% c(1,2,3) ~ 1,
                        month %in% c(4,5,6) ~ 2,
                        month %in% c(7,8,9) ~ 3,
                        month %in% c(10,11,12) ~ 4),
    season = NA_character_,
    # Calculate decimal lat and lon, depth in m, where needed
    S_LATD = ifelse(S_LATD == 0, NA, S_LATD), 
    S_LOND = ifelse(S_LOND == 0, NA, S_LOND), 
    E_LATD = ifelse(E_LATD == 0, NA, E_LATD), 
    E_LOND = ifelse(E_LOND == 0, NA, E_LOND),
    latitude = rowMeans(cbind(S_LATD + S_LATM/60, E_LATD + E_LATM/60), na.rm=T), 
    longitude = -rowMeans(cbind(S_LOND + S_LONM/60, E_LOND + E_LONM/60), na.rm=T), 
    # convert fathoms to meters
    depth = DEPTH_SSTA * 1.8288, 
    # Add "strata" (define by lat, lon and depth bands) where needed
    # degree bins, # degree bins, # 100 m bins
    stratum = paste(floor(latitude)+0.5, floor(longitude)+0.5, floor(depth/100)*100 + 50, sep= "-")
  )

# fix speed
# Trim out or fix speed and duration records
# trim out tows of 0, >60, or unknown minutes
gmex <- gmex %>% 
  filter(MIN_FISH <= 60 & MIN_FISH > 0 & !is.na(MIN_FISH)) %>% 
  # fix typo according to Jeff Rester: 30 = 3	
  mutate(VESSEL_SPD = ifelse(VESSEL_SPD == 30, 3, VESSEL_SPD)) %>% 
  # trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)
  filter(VESSEL_SPD <= 5 & VESSEL_SPD > 0  & !is.na(VESSEL_SPD))

gmex_strats <- gmex %>%
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(longitude, latitude))
gmex <- left_join(gmex, gmex_strats, by = "stratum")

# while comsat is still present
# Remove a tow when paired tows exist (same lat/lon/year but different haulid, only Gulf of Mexico)
# identify duplicate tows at same year/lat/lon
dups <- gmex %>%
  group_by(year, latitude, longitude) %>%
  filter(n() > 1) %>%
  group_by(haulid) %>%
  filter(n() == 1)

# remove the identified tows from the dataset
gmex <- gmex %>%
  filter(!haulid %in% dups$haulid & !grepl("PORT", COMSTAT))

gmex <- gmex %>% 
  rename(spp = TAXONOMIC,
         sub_area = SOURCE) %>% 
  # adjust for area towed
  mutate(
    # kg per 10000m2. calc area trawled in m2: knots * 1.8 km/hr/knot * 1000 m/km * minutes * 1 hr/60 min * width of gear in feet * 0.3 m/ft # biomass per standard tow
    wtcpue = 10000*SELECT_BGS/(VESSEL_SPD * 1.85200 * 1000 * MIN_FISH / 60 * GEAR_SIZE * 0.3048) 
  ) %>% 
  # remove non-fish
  filter(
    spp != '' | !is.na(spp),
    # remove unidentified spp
    !spp %in% c('UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')
  ) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(GENUS_BGS == 'PELAGIA' & SPEC_BGS == 'NOCTUL', 'PELAGIA NOCTILUCA', spp), 
    BIO_BGS = ifelse(spp == "PELAGIA NOCTILUCA", 618030201, BIO_BGS), 
    spp = ifelse(GENUS_BGS == 'MURICAN' & SPEC_BGS == 'FULVEN', 'MURICANTHUS FULVESCENS', spp), 
    BIO_BGS = ifelse(spp == "MURICANTHUS FULVESCENS", 308011501, BIO_BGS), 
    spp = ifelse(grepl("APLYSIA", spp), "APLYSIA", spp), 
    spp = ifelse(grepl("AURELIA", spp), "AURELIA", spp), 
    spp = ifelse(grepl("BOTHUS", spp), "BOTHUS", spp), 
    spp = ifelse(grepl("CLYPEASTER", spp), "CLYPEASTER", spp), 
    spp = ifelse(grepl("CONUS", spp), "CONUS", spp), 
    spp = ifelse(grepl("CYNOSCION", spp), "CYNOSCION", spp), 
    spp = ifelse(grepl("ECHINASTER", spp), "ECHINASTER", spp),
    spp = ifelse(grepl("OPISTOGNATHUS", spp), "OPISTOGNATHUS", spp), 
    spp = ifelse(grepl("OPSANUS", spp), "OPSANUS", spp), 
    spp = ifelse(grepl("ROSSIA", spp), "ROSSIA", spp), 
    spp = ifelse(grepl("SOLENOCERA", spp), "SOLENOCERA", spp), 
    spp = ifelse(grepl("TRACHYPENEUS", spp), "TRACHYPENEUS", spp)
  ) %>% 
  # add survey column
  mutate(survey = "GMEX",
         country = "United States",
         continent = "n_america",
         stat_rec = NA,
         ) %>% 
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
          num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)
  # group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  # summarise(wtcpue = sumna(wtcpue)) %>% 
  # select(survey, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  # ungroup()
  # 

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  p1 <- gmex %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- gmex %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  
  test <- gmex %>% 
    #filter(year >= 2008) %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 23) 
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- gmex %>% 
    filter(stratum %in% test$stratum)
  nrow(gmex) - nrow(test2)
  # percent that will be lost
  print((nrow(gmex) - nrow(test2))/nrow(gmex))
  # lose 18.24% of rows
  
  
  
  gmex_fltr <- gmex %>%
    filter(stratum %in% test$stratum) 
  #filter(year >= 2008) 
  
  p3 <- gmex_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- gmex_fltr%>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "gmex_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}
rm(gmex_bio, gmex_cruise, gmex_spp, gmex_station, gmex_tow, newspp, problems, gmex_station_raw, gmex_station_clean, gmex_strats, dups)

# Compile Northeast US ===========================================================
print("Compile NEUS")

#load conversion factors
NEFSC_conv <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/NEFSC_conversion_factors.csv", col_types = "_ddddddd")
NEFSC_conv <- data.table::as.data.table(NEFSC_conv)
#Bigelow >2008 Vessel Conversion
#Use Bigelow conversions for Pisces as well (PC)
#Tables 56-58 from Miller et al. 2010 Biomass estimators
big_fall <- data.table::data.table(svspp = c('012', '022', '024', '027', '028', 
                                             '031', '033', '034', '073', '076', 
                                             '106', '107', '109', '121', '135', 
                                             '136', '141', '143', '145', '149', 
                                             '155', '164', '171', '181', '193', 
                                             '197', '502', '512', '015', '023', '026', 
                                             '032', '072', '074', '077', '078', 
                                             '102', '103', '104', '105', '108', 
                                             '131', '163', '301', '313', '401', 
                                             '503'),
                                   season = c(rep('fall', 47)),
                                   rhoW = c(1.082, 3.661, 6.189, 4.45, 3.626, 1.403, 1.1, 2.12,
                                            1.58, 2.088, 2.086, 3.257, 12.199, 0.868, 0.665, 1.125,
                                            2.827, 1.347, 1.994, 1.535, 1.191, 1.354, 3.259, 0.22,
                                            3.912, 8.062, 1.409, 2.075, 1.21,
                                            2.174, 8.814, 1.95, 4.349, 1.489, 3, 2.405, 1.692,
                                            2.141, 2.151, 2.402, 1.901, 1.808, 2.771, 1.375, 2.479,
                                            3.151, 1.186))

big_spring <- data.table::data.table(svspp = c('012', '022', '024', '027', '028', 
                                               '031', '033', '034', '073', '076', 
                                               '106', '107', '109', '121', '135', 
                                               '136', '141', '143', '145', '149', 
                                               '155', '164', '171', '181', '193', 
                                               '197', '502', '512', '015', '023', 
                                               '026', '032', '072', '074', '077', 
                                               '078', '102', '103', '104', '105', 
                                               '108', '131', '163', '301', '313', 
                                               '401', '503'),
                                     season = c(rep('spring', 47)),
                                     rhoW = c(1.082, 3.661, 6.189, 4.45, 3.626, 1.403, 1.1, 2.12,
                                              1.58, 2.088, 2.086, 3.257, 12.199, 0.868, 0.665, 1.125,
                                              2.827, 1.347, 1.994, 1.535, 1.191, 1.354, 3.259, 0.22,
                                              3.912, 8.062, 1.409, 2.075, 1.166, 3.718, 2.786, 5.394,
                                              4.591, 0.878, 3.712, 3.483, 2.092, 3.066, 3.05, 2.244,
                                              3.069, 2.356, 2.986, 1.272, 3.864, 1.85, 2.861))


#read strata file
neus_strata <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_strata.csv", col_types = cols(.default = col_character())) %>%
  select(stratum, stratum_area) %>% 
  mutate(stratum = as.double(stratum)) %>%
  distinct()
#read and clean spp file
neus_spp_raw <- read_lines("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_spp.csv")
neus_spp_raw <- str_replace_all(neus_spp_raw, 'SQUID, CUTTLEFISH, AND OCTOPOD UNCL', 'Squid/Cuttlefish/Octopod (unclear)')
neus_spp_raw <- str_replace_all(neus_spp_raw, 'SEA STAR, BRITTLE STAR, AND BASKETSTAR UNCL', 'Sea Star/Brittle Star/Basket Star (unclear)')
neus_spp_raw <- str_replace_all(neus_spp_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_spp_raw <- str_replace_all(neus_spp_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_spp_clean <- str_replace_all(neus_spp_raw, 'SHRIMP \\(PINK,BROWN,WHITE\\)', 'Shrimp \\(pink/brown/white\\)')
neus_spp<- read_csv(neus_spp_clean, col_types = cols(.default = col_character()))

rm(neus_spp_clean, neus_spp_raw)


#NEUS Fall
neus_catch_raw <- read_lines("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_fall_svcat.csv")
# remove comma
neus_catch_raw <- str_replace_all(neus_catch_raw, 'SQUID, CUTTLEFISH, AND OCTOPOD UNCL', 'Squid/Cuttlefish/Octopod (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'SEA STAR, BRITTLE STAR, AND BASKETSTAR UNCL', 'Sea Star/Brittle Star/Basket Star (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_catch_clean <- str_replace_all(neus_catch_raw, 'SHRIMP \\(PINK,BROWN,WHITE\\)', 'Shrimp \\(pink/brown/white\\)')
neus_fall_catch <- read_csv(neus_catch_clean, col_types = cols(.default = col_character())) #%>% 
  #select('CRUISE6','STRATUM','TOW','STATION','ID','LOGGED_SPECIES_NAME','SVSPP','CATCHSEX','EXPCATCHNUM','EXPCATCHWT')

neus_fall_haul <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_fall_svsta.csv", col_types = cols(.default = col_character())) #%>% 
  #select("CRUISE6","STRATUM", "ID", "AREA","EST_YEAR","AVGDEPTH", "DECDEG_BEGLAT","DECDEG_BEGLON", "SVVESSEL")
drops <- c("CRUISE6","STRATUM")
neus_fall <- left_join(neus_fall_catch, neus_fall_haul[ , !(names(neus_fall_haul) %in% drops)], by = "ID")
neus_fall <- left_join(neus_fall, neus_spp, by = "SVSPP")

neus_fall <- neus_fall %>%
  rename(year = EST_YEAR,
         month = EST_MONTH,
         day = EST_DAY,
         lat = DECDEG_BEGLAT, 
         lon = DECDEG_BEGLON, 
         depth = AVGDEPTH,
         stratum = STRATUM,
         haulid = ID,
         spp = SCINAME,
         wgt = EXPCATCHWT,
         num = EXPCATCHNUM,
         station = STATION,
         sst = SURFTEMP,
         sbt = BOTTEMP) 
neus_fall <- neus_fall %>%
  mutate(stratum = as.double(stratum),
         latitude = as.double(lat),
         longitude = as.double(lon),
         depth = as.double(depth),
         wgt = as.double(wgt),
         year = as.double(year),
         haul_dur = as.numeric(TOWDUR),
         quarter = case_when(month %in% c(1,2,3) ~ 1,
                             month %in% c(4,5,6) ~ 2,
                             month %in% c(7,8,9) ~ 3,
                             month %in% c(10,11,12) ~ 4),
         season = "Fall",
         SVSPP = as.double(SVSPP),
         area = AREA/1000000) # I think this is incorrect because AREA here refers to a code, not a value


#apply fall conversion factors
setDT(neus_fall)

dcf.spp <- NEFSC_conv[DCF_WT > 0, SVSPP]

#test for changes due to conversion with "before" and "after"
#before <- neus_fall[year < 1985 & SVSPP %in% dcf.spp, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]

for(i in 1:length(dcf.spp)){
  neus_fall[year < 1985 & SVSPP == dcf.spp[i], wgt  := wgt  * NEFSC_conv[SVSPP == dcf.spp[i], DCF_WT]]
}

#after <- neus_fall[year < 1985 & SVSPP %in% dcf.spp, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]

#before <- neus_fall[SVVESSEL == 'DE' & SVSPP %in% vcf.spp, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]

vcf.spp <- NEFSC_conv[VCF_WT > 0, SVSPP]
for(i in 1:length(dcf.spp)){
  neus_fall[SVVESSEL == 'DE' & SVSPP == vcf.spp[i], wgt  := wgt * NEFSC_conv[SVSPP == vcf.spp[i], VCF_WT]]
}

#after<- neus_fall[SVVESSEL == 'DE' & SVSPP %in% vcf.spp, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]

spp_fall <- big_fall[season == 'fall', svspp]

#before <- neus_fall[SVVESSEL %in% c('HB', 'PC') & SVSPP %in% spp_fall, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]
for(i in 1:length(big_fall$svspp)){
  neus_fall[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp_fall[i], wgt  := wgt  / big_fall[i, rhoW]]
}  

#after <- neus_fall[SVVESSEL %in% c('HB', 'PC')  & SVSPP %in% spp_fall, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]

neus_fall <- as.data.frame(neus_fall)

# sum different sexes of same spp together
neus_fall <- neus_fall %>% 
  group_by(year, latitude, longitude, depth, haulid, CRUISE6, STATION, stratum, spp) %>% 
  mutate(wgt_cpue = sum(wgt)) %>%
  ungroup()
#neus_fall <- ungroup(neus_fall)

#join with strata
neus_fall <- left_join(neus_fall, neus_strata, by = "stratum")
neus_fall <- filter(neus_fall, !is.na(stratum_area))
neus_fall <- neus_fall %>%
  rename(stratumarea = stratum_area) %>%
  mutate(stratumarea = as.double(stratumarea)* 3.429904) #convert square nautical miles to square kilometers
neus_fall$survey <- "NEUS-F"

neus_fall<- neus_fall %>%
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name)



# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(neus_fall, is.na(stratumarea))) == 0)

rm(neus_catch_clean, neus_catch_raw, neus_fall_catch, neus_fall_haul)


#NEUS Spring
neus_catch_raw <- read_lines("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_spring_svcat.csv")
# remove comma
neus_catch_raw <- str_replace_all(neus_catch_raw, 'SQUID, CUTTLEFISH, AND OCTOPOD UNCL', 'Squid/Cuttlefish/Octopod (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'SEA STAR, BRITTLE STAR, AND BASKETSTAR UNCL', 'Sea Star/Brittle Star/Basket Star (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_catch_raw <- str_replace_all(neus_catch_raw, 'MOON SNAIL, SHARK EYE, AND BABY-EAR UNCL', 'Moon Snail/shark eye/baby-ear (unclear)')
neus_catch_clean <- str_replace_all(neus_catch_raw, 'SHRIMP \\(PINK,BROWN,WHITE\\)', 'Shrimp \\(pink/brown/white\\)')
neus_spring_catch <- read_csv(neus_catch_clean, col_types = cols(.default = col_character())) %>% 
  select('CRUISE6','STRATUM','TOW','STATION','ID','LOGGED_SPECIES_NAME','SVSPP','CATCHSEX','EXPCATCHNUM','EXPCATCHWT')
rm(neus_catch_clean, neus_catch_raw)

neus_spring_haul <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/neus_spring_svsta.csv", col_types = cols(.default = col_character())) %>% 
  select("CRUISE6","STRATUM", "ID", "AREA","EST_YEAR","AVGDEPTH", "DECDEG_BEGLAT","DECDEG_BEGLON", "SVVESSEL")
drops <- c("CRUISE6","STRATUM")
neus_spring <- left_join(neus_spring_catch, neus_spring_haul[ , !(names(neus_spring_haul) %in% drops)], by = "ID")
neus_spring <- left_join(neus_spring, neus_spp, by = "SVSPP")


rm(neus_spring_catch, neus_spring_haul)
neus_spring <- neus_spring %>%
  rename(year = EST_YEAR,
         lat = DECDEG_BEGLAT, 
         lon = DECDEG_BEGLON, 
         depth = AVGDEPTH,
         stratum = STRATUM,
         haulid = ID,
         spp = SCINAME,
         wtcpue = EXPCATCHWT) 
neus_spring <- neus_spring %>%
  mutate(stratum = as.double(stratum),
         lat = as.double(lat),
         lon = as.double(lon),
         depth = as.double(depth),
         wtcpue = as.double(wtcpue))


#apply spring conversion factors
setDT(neus_spring)

dcf.spp <- NEFSC_conv[DCF_WT > 0, SVSPP]

for(i in 1:length(dcf.spp)){
  neus_spring[year < 1985 & SVSPP == dcf.spp[i], 
              wtcpue := wtcpue * NEFSC_conv[SVSPP == dcf.spp[i], DCF_WT]]
}

gcf.spp <- NEFSC_conv[GCF_WT > 0, SVSPP]
for(i in 1:length(gcf.spp)){
  neus_spring[year > 1972 & year < 1982 & SVSPP == gcf.spp[i],
              wtcpue := wtcpue / NEFSC_conv[SVSPP == gcf.spp[i], GCF_WT]]
}

vcf.spp <- NEFSC_conv[VCF_WT > 0, SVSPP]
for(i in 1:length(dcf.spp)){
  neus_spring[SVVESSEL == 'DE' & SVSPP == vcf.spp[i], wtcpue := wtcpue* NEFSC_conv[SVSPP == vcf.spp[i], VCF_WT]]
}

spp_spring <- big_spring[season == 'spring', svspp]
#before <- neus_spring[SVVESSEL %in% c('HB', 'PC') & SVSPP %in% spp_spring, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]
for(i in 1:length(big_spring$svspp)){
  neus_spring[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp_spring[i], wtcpue := wtcpue / big_spring[i, rhoW]]
}  

#after <- neus_spring[SVVESSEL %in% c('HB', 'PC')  & SVSPP %in% spp_spring, .(mean_wtcpue=mean(wtcpue)), by=SVSPP][order(SVSPP)]


neus_spring <- as.data.frame(neus_spring)

# sum different sexes of same spp together
neus_spring <- neus_spring %>% 
  group_by(year, lat, lon, depth, haulid, CRUISE6, STATION, stratum, spp) %>% 
  summarise(wtcpue = sum(wtcpue)) 
neus_spring <- ungroup(neus_spring)

#join with strata
neus_spring <- left_join(neus_spring, neus_strata, by = "stratum")
neus_spring <- filter(neus_spring, !is.na(stratum_area))

# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(neus_spring, is.na(stratum_area))) == 0)
neus_spring <- neus_spring %>%
  rename(stratumarea = stratum_area) %>%
  mutate(stratumarea = as.double(stratumarea)* 3.429904)#convert square nautical miles to square kilometers

neus_spring$survey <- "NEUS-S"
neus_spring <- neus_spring %>%
  select(survey, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)

# are there any strata in the data that are not in the strata file?
stopifnot(nrow(filter(neus_fall, is.na(stratumarea))) == 0)



# NEUS Fall ====

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  
  p1 <- neus_fall %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year))) +
    geom_jitter()
  
  p2 <- neus_fall %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- neus_fall %>% 
    filter(year != 2017, year >= 1972) %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 45)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- neus_fall %>% 
    filter(year != 2017, year > 1973) %>% 
    filter(stratum %in% test$stratum)
  nrow(neus_fall) - nrow(test2)
  # percent that will be lost
  (nrow(neus_fall) - nrow(test2))/nrow(neus_fall)
  # 60% is too much, by removing bad years we get rid of 9%, which is not so bad.
  # When bad strata are removed after bad years we only lose 37%
  
  neus_fall_fltr <- neus_fall %>%
    filter(year != 2017, year > 1973) %>% 
    filter(stratum %in% test$stratum) 
  
  p3 <- neus_fall_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- neus_fall_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "neusF_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, test2, p1, p2, p3, p4)
}

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  p1 <-neus_spring %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p2 <- neus_spring %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  
  test <- neus_spring %>% 
    filter(year > 1973) %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 45)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- neus_spring %>% 
    filter(stratum %in% test$stratum)
  nrow(neus_spring) - nrow(test2)
  # percent that will be lost
  (nrow(neus_spring) - nrow(test2))/nrow(neus_spring)
  #23%
  
  test <- neus_spring %>% 
    filter(year != 2020,year != 2014, year != 1975, year > 1973) %>%
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 42)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- neus_spring %>% 
    filter(stratum %in% test$stratum)
  nrow(neus_spring) - nrow(test2)
  # percent that will be lost
  (nrow(neus_spring) - nrow(test2))/nrow(neus_spring)
  # When bad strata are removed after bad years we only lose 20.1%
  
  neus_spring_fltr <- neus_spring %>%
    filter(year != 2020,year != 2014, year != 1975, year > 1973) %>% 
    filter(stratum %in% test$stratum) 
  
  p3 <- neus_spring_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- neus_spring_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "neusS_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, p1, p2, p3, p4)
}

rm(neus_spp, neus_strata, big_fall, big_spring, NEFSC_conv)

# Compile SEUS ===========================================================
print("Compile SEUS")
# turns everything into a character so import as character anyway
temp <- tempfile()
download.file("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/seus_catch.csv.zip", temp)

seus_catch <- read_csv(unz(temp, "seus_catch.csv"), col_types = cols(.default = col_character())) %>% 
  # remove symbols
  mutate_all(list(~str_replace(., "=", ""))) %>% 
  mutate_all(list(~str_replace(., '"', ''))) %>% 
  mutate_all(list(~str_replace(., '\"', ''))) 

# The 9 parsing failures are due to the metadata at the end of the file that does not fit into the data columns

# problems should have 0 obs
problems <- problems(seus_catch) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)

# convert the columns to their correct formats
seus_catch <- type_convert(seus_catch, col_types = cols(
  PROJECTNAME = col_character(),
  PROJECTAGENCY = col_character(),
  DATE = col_character(),
  EVENTNAME = col_character(),
  COLLECTIONNUMBER = col_character(),
  VESSELNAME = col_character(),
  GEARNAME = col_character(),
  GEARCODE = col_character(),
  SPECIESCODE = col_character(),
  MRRI_CODE = col_character(),
  SPECIESSCIENTIFICNAME = col_character(),
  SPECIESCOMMONNAME = col_character(),
  NUMBERTOTAL = col_integer(),
  SPECIESTOTALWEIGHT = col_double(),
  SPECIESSUBWEIGHT = col_double(),
  SPECIESWGTPROCESSED = col_character(),
  WEIGHTMETHODDESC = col_character(),
  ORGWTUNITS = col_character(),
  EFFORT = col_character(),
  CATCHSUBSAMPLED = col_logical(),
  CATCHWEIGHT = col_double(),
  CATCHSUBWEIGHT = col_double(),
  TIMESTART = col_character(),
  DURATION = col_integer(),
  TOWTYPETEXT = col_character(),
  LOCATION = col_character(),
  REGION = col_character(),
  DEPTHZONE = col_character(),
  ACCSPGRIDCODE = col_character(),
  STATIONCODE = col_character(),
  EVENTTYPEDESCRIPTION = col_character(),
  TEMPSURFACE = col_double(),
  TEMPBOTTOM = col_double(),
  SALINITYSURFACE = col_double(),
  SALINITYBOTTOM = col_double(),
  SDO = col_character(),
  BDO = col_character(),
  TEMPAIR = col_double(),
  LATITUDESTART = col_double(),
  LATITUDEEND = col_double(),
  LONGITUDESTART = col_double(),
  LONGITUDEEND = col_double(),
  SPECSTATUSDESCRIPTION = col_character(),
  LASTUPDATED = col_character()
))

seus_haul <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/seus_haul.csv", col_types = cols(.default = col_character())) %>% 
  distinct(EVENTNAME, DEPTHSTART)  %>% 
  # remove symbols
  mutate_all(list(~str_replace(., "=", ""))) %>% 
  mutate_all(list(~str_replace(., '"', ''))) %>% 
  mutate_all(list(~str_replace(., '"', '')))

# problems should have 0 obs
problems <- problems(seus_haul) %>% 
  filter(!is.na(col))
stopifnot(nrow(problems) == 0)

seus_haul <- type_convert(seus_haul, col_types = cols(
  EVENTNAME = col_character(),
  DEPTHSTART = col_integer()
))

seus <- left_join(seus_catch, seus_haul, by = "EVENTNAME")

# contains strata areas
seus_strata <- read_csv("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/seus_strata.csv", col_types = cols(
  STRATA = col_integer(),
  STRATAHECTARE = col_double()
))


#Create STRATA column
seus <- seus %>% 
  mutate(STRATA = as.numeric(str_sub(STATIONCODE, 1, 2))) %>% 
  # Drop OUTER depth zone because it was only sampled for 10 years
  filter(DEPTHZONE != "OUTER")

#add STRATAHECTARE to main file 
seus <- left_join(seus, seus_strata, by = "STRATA") 

#Create a 'SEASON' column using 'MONTH' as a criteria
seus <- seus %>% 
  mutate(DATE = as.Date(DATE, "%m-%d-%Y"), 
         MONTH = month(DATE)) %>%
  # create season column
  mutate(SEASON = NA, 
         SEASON = ifelse(MONTH >= 1 & MONTH <= 3, "winter", SEASON), 
         SEASON = ifelse(MONTH >= 4 & MONTH <= 6, "spring", SEASON),
         SEASON = ifelse(MONTH >= 7 & MONTH <= 8, "summer", SEASON),
         #September EVENTS were grouped with summer, should be fall because all
         #hauls made in late-September during fall-survey
         SEASON = ifelse(MONTH >= 9 & MONTH <= 12, "fall", SEASON))  

# find rows where weight wasn't provided for a species
misswt <- seus %>% 
  filter(is.na(SPECIESTOTALWEIGHT)) %>% 
  select(SPECIESCODE, SPECIESSCIENTIFICNAME) %>% 
  distinct()

# calculate the mean weight for those species
meanwt <- seus %>% 
  filter(SPECIESCODE %in% misswt$SPECIESCODE) %>% 
  group_by(SPECIESCODE) %>% 
  summarise(mean_wt = mean(SPECIESTOTALWEIGHT, na.rm = T))

# rows that need to be changed
change <- seus %>%
  filter(is.na(SPECIESTOTALWEIGHT))

# remove those rows from SEUS
seus <- anti_join(seus, change)

# change the rows
change <- change %>% 
  select(-SPECIESTOTALWEIGHT)

# update the column values
change <- left_join(change, meanwt, by = "SPECIESCODE") %>% 
  rename(SPECIESTOTALWEIGHT = mean_wt)

# rejoin to the data
seus <- rbind(seus, change)


#Data entry error fixes for lat/lon coordinates
seus <- seus %>%
  mutate(
    # longitudes of less than -360 (like -700), do not exist.  This is a missing decimal.
    LONGITUDESTART = ifelse(LONGITUDESTART < -360, LONGITUDESTART/10, LONGITUDESTART), 
    LONGITUDEEND = ifelse(LONGITUDEEND < -360, LONGITUDEEND/10, LONGITUDEEND), 
    # latitudes of more than 100 are outside the range of this survey.  This is a missing decimal.
    LATITUDESTART = ifelse(LATITUDESTART > 100, LATITUDESTART/10, LATITUDESTART), 
    LATITUDEEND = ifelse(LATITUDEEND  > 100, LATITUDEEND/10, LATITUDEEND)
  )

# calculate trawl distance in order to calculate effort
# create a matrix of starting positions
start <- as.matrix(seus[,c("LONGITUDESTART", "LATITUDESTART")], nrow = nrow(seus), ncol = 2)
# create a matrix of ending positions
end <- as.matrix(seus[,c("LONGITUDEEND", "LATITUDEEND")], nrow = nrow(seus), ncol = 2)
# add distance to seus table
seus <- seus %>%
  mutate(distance_m = geosphere::distHaversine(p1 = start, p2 = end),
         distance_km = distance_m / 1000.0, 
         distance_mi = distance_m / 1609.344) %>% 
  # calculate effort = mean area swept
  # EFFORT = 0 where the boat didn't move, distance_m = 0
  mutate(EFFORT = (13.5 * distance_m)/10000, 
         # Create a unique haulid
         haulid = EVENTNAME, 
         # Extract year where needed
         year = substr(EVENTNAME, 1,4)
  ) %>% 
  rename(
    stratum = STRATA, 
    lat = LATITUDESTART, 
    lon = LONGITUDESTART, 
    depth = DEPTHSTART, 
    spp = SPECIESSCIENTIFICNAME, 
    stratumarea = STRATAHECTARE)

seus$year <- as.integer(seus$year)

#In seus there are two 'COLLECTIONNUMBERS' per 'EVENTNAME', with no exceptions; EFFORT is always the same for each COLLECTIONNUMBER
# We sum the two tows in seus
biomass <- seus %>% 
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, SEASON, spp, EFFORT) %>% 
  summarise(biomass = sum(SPECIESTOTALWEIGHT)) %>% 
  mutate(wtcpue = biomass/(EFFORT*2))

seus <- left_join(seus, biomass, by = c("haulid", "stratum", "stratumarea", "year", "lat", "lon", "depth", "SEASON", "spp", "EFFORT"))
# double check that column numbers haven't changed by more than 2.  

seus <- seus %>% 
  # remove non-fish
  filter(
    !spp %in% c('MISCELLANEOUS INVERTEBRATES','XANTHIDAE','MICROPANOPE NUTTINGI','ALGAE','DYSPANOPEUS SAYI', 'PSEUDOMEDAEUS AGASSIZII')
  ) %>% 
  # adjust spp names
  mutate(
    spp = ifelse(grepl("ANCHOA", spp), "ANCHOA", spp), 
    spp = ifelse(grepl("LIBINIA", spp), "LIBINIA", spp)
  )  %>% 
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp, SEASON) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add temporary region column that will be converted to seasonal
  mutate(survey = "SEUS") %>% 
  select(survey, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue, SEASON) %>% 
  ungroup()

#remove infinite wtcpue values (where effort was 0, causes wtcpue to be inf)
seus <- seus[!is.infinite(seus$wtcpue),]

# now that lines have been removed from the main data set, can split out seasons
# SEUS spring ====
#Separate the the spring season and convert to dataframe
seusSPRING <- seus %>% 
  filter(SEASON == "spring") %>% 
  select(-SEASON) %>% 
  mutate(region = "Southeast US Spring")

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  
  p1 <- seusSPRING %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year))) +
    geom_jitter()
  
  p2 <- seusSPRING %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- seusSPRING %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n()) %>%
    filter(count >= 29)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- seusSPRING %>% 
    filter(stratum %in% test$stratum)
  nrow(seusSPRING) - nrow(test2)
  # percent that will be lost
  print((nrow(seusSPRING) - nrow(test2))/nrow(seusSPRING))
  # 6% are removed
  
  seusSPRING_fltr <- seusSPRING %>%
    filter(stratum %in% test$stratum) 
  
  p3 <- seusSPRING_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- seusSPRING_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "seusSPR_hq_dat_removed.png"))
    rm(temp)
  }
  rm(test, p1, p2, p3, p4)
}

# SEUS summer ====
#Separate the summer season and convert to dataframe
seusSUMMER <- seus %>% 
  filter(SEASON == "summer") %>% 
  select(-SEASON) %>% 
  mutate(region = "Southeast US Summer")

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  #no need to filter, but rename dataset for consistency
  seusSUMMER_fltr <- seusSUMMER
  p1 <- seusSUMMER_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year))) +
    geom_jitter()
  
  p2 <- seusSUMMER_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "seusSUM_hq_dat_removed.png"))
    rm(temp)
  }
  rm(p1, p2)
}
# no missing data

# SEUS fall ====
seusFALL <- seus %>% 
  filter(SEASON == "fall") %>% 
  select(-SEASON) %>% 
  mutate(region = "Southeast US Fall")


# how many rows will be lost if only stratum trawled ever year are kept?
if (HQ_DATA_ONLY == TRUE){
  test <- seusFALL %>%
    filter(year != 1986, year != 1978, year != 2018,  year != 2019) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 6)
  
  test2 <- seusFALL %>% 
    filter(year != 1986, year != 1978, year != 2018,  year != 2019) %>%
    filter(stratum %in% test$stratum)
  nrow(seusFALL) - nrow(test2)
  # percent that will be lost
  print((nrow(seusFALL) - nrow(test2))/nrow(seusFALL))
  # 5.1% are removed
  
  p1 <- seusFALL %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year))) +
    geom_jitter()
  
  p2 <- seusFALL %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  seusFALL_fltr <- seusFALL  %>%
    filter(year != 1986, year != 1978, year != 2018,  year != 2019) %>%
    filter(stratum %in% test$stratum)
  
  # plot the results after editing
  p3 <- seusFALL_fltr %>% 
    select(stratum, year) %>% 
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- seusFALL_fltr %>%
    select(lat, lon) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "seusFALL_hq_dat_removed.png"))
    rm(temp)
  }
}
#clean up
rm(test, test2, p1, p2, p3, p4)

rm(seus_catch, seus_haul, seus_strata, end, start, meanwt, misswt, biomass, problems, change, seus)

# Compile Maritimes (Scotian Shelf) ---------------------------------------------------

spp_files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_FALL_SPP.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SPRING__SPP.csv",
              "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SUMMER_SPP.csv")

#spp_files <- as.list(dir(pattern = "_SPP", path = "data_raw", full.names = T))
mar_spp <- spp_files %>% 
  map_dfr(~ read_csv(.x, col_types = cols(
    SPEC = col_character()
  )))

mar_spp <- mar_spp %>%
  rename(spp = SPEC,
         SPEC = CODE) 

mission_files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_FALL_MISSION.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SPRING_MISSION.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SUMMER_MISSION.csv")
#mission_files <- as.list(dir(pattern = "_MISSION", path = "data_raw", full.names = T))
mar_missions <- mission_files %>% 
  map_dfr(~ read_csv(.x, col_types = cols(
    .default = col_double(),
    MISSION = col_character(),
    VESEL = col_character(),
    SEASON = col_character()
  )))

info_files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_FALL_INF.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SPRING__INF.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SUMMER_INF.csv")
#info_files <- as.list(dir(pattern = "_INF", path = "data_raw", full.names = T))
mar_info <- info_files %>% 
  map_dfr(~ read_csv(.x, col_types = cols(
    .default = col_double(),
    MISSION = col_character(),
    SDATE = col_character(),
    GEARDESC = col_character(),
    STRAT = col_character() 
  )))

catch_files <- list("https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_FALL_CATCH.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SPRING__CATCH.csv",
                  "https://raw.githubusercontent.com/pinskylab/OceanAdapt/master/data_raw/MAR_SUMMER_CATCH.csv")
#catch_files <- as.list(dir(pattern = "_CATCH", path = "data_raw", full.names = T))
mar_catch <- catch_files %>% 
  map_dfr(~ read_csv(.x, col_types = cols(
    .default = col_double(),
    MISSION = col_character()
  )))

mar <- left_join(mar_catch, mar_missions, by = "MISSION") 

mar <- mar %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(MISSION, width=3, flag=0), formatC(SETNO, width=3, flag=0))) 

mar_info <- mar_info %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(MISSION, width=3, flag=0), formatC(SETNO, width=3, flag=0))) 

drops <- c("MISSION","SETNO")
mar_info <- mar_info[ , !(names(mar_info) %in% drops)]

mar <- left_join(mar, mar_info, by = "haulid")
mar <- left_join(mar, mar_spp, by = "SPEC")
mar$survey <- "SCS"

names(mar) <- tolower(names(mar))


mar <- mar %>% 
  # convert mission to haul_id
  rename(wtcpue = totwgt, 
         lat = slat, 
         lon = slong, 
         stratum = strat)

# calculate stratum area for each stratum
mar <- mar %>% 
  group_by(stratum) %>% 
  filter(stratum != 'NA') %>%
  mutate(stratumarea = calcarea(lon, lat)) %>% 
  ungroup()


# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- mar %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>% 
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)

# combine the wtcpue for each species by haul
mar <- mar %>% 
  group_by(haulid, stratum, stratumarea, year, season, lat, lon, depth, spp, survey) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  ungroup() %>% 
  # remove extra columns
  select(survey, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue, season)

rm(mar_catch, mar_info, mar_missions, mar_spp, mission_files, info_files, spp_files, catch_files)

mar$spp <- firstup(mar$spp)

# Maritimes Fall ====
marFall <- mar %>% 
  ungroup() %>% 
  filter(season == "FALL") %>% 
  select(-season) %>% 
  mutate(region = "Maritimes Fall")


# Maritimes Spring ====
marSpring <- mar %>% 
  ungroup() %>% 
  filter(season == "SPRING") %>% 
  select(-season) %>% 
  mutate(region = "Maritimes Spring")

# # plot the strata by year
# p1 <- marSpring %>%
#   select(stratum, year) %>%
#   ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
#   geom_jitter()
# p2 <- marSpring %>%
#   select(lat, lon) %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_jitter()
# 
# grid.arrange(p1, p2, nrow = 2)

# Maritimes Summer ====
marSummer <- mar %>% 
  ungroup() %>% 
  filter(season == "SUMMER") %>% 
  select(-season) %>% 
  mutate(region = "Maritimes Summer")

# # plot the strata by year
# p1 <- marSummer %>%
#   select(stratum, year) %>%
#   ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
#   geom_jitter()
# p2 <- marSummer %>%
#   select(lat, lon) %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_jitter()
# 
# grid.arrange(p1, p2, nrow = 2)



# test <- mar %>%
#   filter(region != "4VSW")
# 
# # plot the strata by year without 4VSW
# p1 <- test %>%
#   select(stratum, year) %>%
#   ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
#   geom_jitter()
# p2 <- mar %>%
#   select(lat, lon) %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_jitter()
# 
# grid.arrange(p1, p2, nrow = 2)

# only consistent methodology and coverage occurred in Summer 
# overwrite mar with only Summer data
mar <- marSummer

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  # plot the strata by year
  p1 <- mar %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  p2 <- mar %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  # find strata sampled every year
  annual_strata <- mar %>%
    filter(year != 2018) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 25)
  
  # find strata sampled every year
  annual_strata_old <- mar %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) 
  
  sum(length(unique(annual_strata_old$count)) - length(unique(annual_strata$count)))
  # how many rows will be lost if only stratum trawled ever year are kept?
  test <- mar %>%
    filter(year!= 2018) %>%
    filter(stratum %in% annual_strata$stratum)
  nrow(mar) - nrow(test)
  # percent that will be lost
  print((nrow(mar) - nrow(test))/nrow(mar))
  # 7.6% are removed
  
  mar_fltr <- mar  %>%
    filter(year != 2018) %>%
    filter(stratum %in% annual_strata$stratum)
  
  p3 <- mar_fltr %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- mar_fltr %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2, p3, p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "mar_hq_dat_removed.png"))
  }
}


# Compile Canadian Pacific ---------------------------------------------------
print("Compile CPAC")

#Queen Charlotte Sound

QCS_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/QCS_catch.csv", col_types = cols(
  Survey.Year = col_integer(),
  Trip.identifier = col_integer(),
  Set.number = col_integer(),
  ITIS.TSN = col_integer(),
  Species.code = col_character(),
  Scientific.name = col_character(),
  English.common.name = col_character(),
  French.common.name = col_character(),
  LSID = col_character(),
  Catch.weight..kg. = col_double(),
  Catch.count..pieces. = col_integer()
)) %>% 
  select(Trip.identifier, Set.number,Survey.Year, ITIS.TSN, Species.code, Scientific.name, English.common.name, Catch.weight..kg.)

QCS_effort <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/QCS_effort.csv", col_types = 
                         cols(
                           Survey.Year = col_integer(),
                           Trip.identifier = col_integer(),
                           Vessel.name = col_character(),
                           Trip.start.date = col_character(),
                           Trip.end.date = col_character(),
                           GMA = col_character(),
                           PFMA = col_character(),
                           Set.number = col_integer(),
                           Set.date = col_character(),
                           Start.latitude = col_double(),
                           Start.longitude = col_double(),
                           End.latitude = col_double(),
                           End.longitude = col_double(),
                           Bottom.depth..m. = col_double(),
                           Tow.duration..min. = col_integer(),
                           Distance.towed..m. = col_double(),
                           Vessel.speed..m.min. = col_double(),
                           Trawl.door.spread..m. = col_double(),
                           Trawl.mouth.opening.height..m. = col_double()
                         )) %>% 
  select(Trip.identifier, Set.number,Survey.Year,Trip.start.date,Trip.end.date, GMA, PFMA,Set.date, Start.latitude,Start.longitude, End.latitude, End.longitude, Bottom.depth..m., Tow.duration..min.,Distance.towed..m., Trawl.door.spread..m., Trawl.mouth.opening.height..m. )

QCS <- left_join(QCS_catch, QCS_effort, by = c("Trip.identifier", "Set.number","Survey.Year"))



QCS <- QCS %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(Trip.identifier, width=3, flag=0), formatC(Set.number, width=3, flag=0), sep= "-"), 
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(Start.latitude), floor(Start.longitude),floor(Bottom.depth..m./100)*100, sep= "-"), 
    # catch weight (kg.) per tow	
    wtcpue = (Catch.weight..kg.)#/(Distance.towed..m.*Trawl.door.spread..m.)
  )


# Calculate stratum area where needed (use convex hull approach)
QCS_strats <- QCS  %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(Start.longitude, Start.latitude))

QCS <- left_join(QCS, QCS_strats, by = "stratum")

QCS <- QCS %>% 
  rename(
    lat = Start.latitude, 
    lon = Start.longitude,
    depth = Bottom.depth..m., 
    spp = Scientific.name,
    year = Survey.Year
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add sub_area column
  mutate(sub_area = "QCS") %>% 
  select(sub_area, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()


# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- QCS %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>%
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)


# combine the wtcpue for each species by haul
QCS <- QCS %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>%
  summarise(wtcpue = sumna(wtcpue)) %>%
  ungroup() %>%
  # remove extra columns
  select(haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)

#test = setcolorder(scot, c('region', 'haulid', 'year', 'lat', 'lon', 'stratum', 'stratumarea', 'depth', 'spp', 'wtcpue'))
test <- QCS %>%
  filter(stratumarea > 0)


#West Coast Vancouver Island


WCV_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/WCV_catch.csv", col_types = cols(
  Survey.Year = col_integer(),
  Trip.identifier = col_integer(),
  Set.number = col_integer(),
  ITIS.TSN = col_integer(),
  Species.code = col_character(),
  Scientific.name = col_character(),
  English.common.name = col_character(),
  French.common.name = col_character(),
  LSID = col_character(),
  Catch.weight..kg. = col_double(),
  Catch.count..pieces. = col_integer()
)) %>% 
  select(Trip.identifier, Set.number,Survey.Year, ITIS.TSN, Species.code, Scientific.name, English.common.name, Catch.weight..kg.)

WCV_effort <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/WCV_effort.csv", col_types = 
                         cols(
                           Survey.Year = col_integer(),
                           Trip.identifier = col_integer(),
                           Vessel.name = col_character(),
                           Trip.start.date = col_character(),
                           Trip.end.date = col_character(),
                           GMA = col_character(),
                           PFMA = col_character(),
                           Set.number = col_integer(),
                           Set.date = col_character(),
                           Start.latitude = col_double(),
                           Start.longitude = col_double(),
                           End.latitude = col_double(),
                           End.longitude = col_double(),
                           Bottom.depth..m. = col_double(),
                           Tow.duration..min. = col_integer(),
                           Distance.towed..m. = col_double(),
                           Vessel.speed..m.min. = col_double(),
                           Trawl.door.spread..m. = col_double(),
                           Trawl.mouth.opening.height..m. = col_double()
                         )) %>% 
  select(Trip.identifier, Set.number,Survey.Year,Trip.start.date,Trip.end.date, GMA, PFMA,Set.date, Start.latitude,Start.longitude, End.latitude, End.longitude, Bottom.depth..m., Tow.duration..min.,Distance.towed..m., Trawl.door.spread..m., Trawl.mouth.opening.height..m. )


WCV <- left_join(WCV_catch, WCV_effort, by = c("Trip.identifier", "Set.number","Survey.Year"))



WCV <- WCV %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(Trip.identifier, width=3, flag=0), formatC(Set.number, width=3, flag=0), sep= "-"), 
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(Start.latitude), floor(Start.longitude),floor(Bottom.depth..m./100)*100, sep= "-"), 
    # catch weight (kg.) per tow	
    wtcpue = (Catch.weight..kg.)#/(Distance.towed..m.*Trawl.door.spread..m.)
  )

# Calculate stratum area where needed (use convex hull approach)
WCV_strats <- WCV  %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(Start.longitude, Start.latitude))

WCV <- left_join(WCV, WCV_strats, by = "stratum")

WCV <- WCV %>% 
  rename(
    lat = Start.latitude, 
    lon = Start.longitude,
    depth = Bottom.depth..m., 
    spp = Scientific.name,
    year = Survey.Year
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add sub_area column
  mutate(sub_area = "WCVI") %>% 
  select(sub_area, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()



# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- WCV %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>%
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)


# combine the wtcpue for each species by haul
WCV <- WCV %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>%
  summarise(wtcpue = sumna(wtcpue)) %>%
  ungroup() %>%
  # remove extra columns
  select(haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)

#test = setcolorder(scot, c('region', 'haulid', 'year', 'lat', 'lon', 'stratum', 'stratumarea', 'depth', 'spp', 'wtcpue'))
test <- WCV %>%
  filter(stratumarea > 0)


#West Coast Haida Guai


WCHG_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/WCHG_catch.csv", col_types = cols(
  Survey.Year = col_integer(),
  Trip.identifier = col_integer(),
  Set.number = col_integer(),
  ITIS.TSN = col_integer(),
  Species.code = col_character(),
  Scientific.name = col_character(),
  English.common.name = col_character(),
  French.common.name = col_character(),
  LSID = col_character(),
  Catch.weight..kg. = col_double(),
  Catch.count..pieces. = col_integer()
)) %>% 
  select(Trip.identifier, Set.number,Survey.Year, ITIS.TSN, Species.code, Scientific.name, English.common.name, Catch.weight..kg.)

WCHG_effort <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/WCHG_effort.csv", col_types = 
                          cols(
                            Survey.Year = col_integer(),
                            Trip.identifier = col_integer(),
                            Vessel.name = col_character(),
                            Trip.start.date = col_character(),
                            Trip.end.date = col_character(),
                            GMA = col_character(),
                            PFMA = col_character(),
                            Set.number = col_integer(),
                            Set.date = col_character(),
                            Start.latitude = col_double(),
                            Start.longitude = col_double(),
                            End.latitude = col_double(),
                            End.longitude = col_double(),
                            Bottom.depth..m. = col_double(),
                            Tow.duration..min. = col_integer(),
                            Distance.towed..m. = col_double(),
                            Vessel.speed..m.min. = col_double(),
                            Trawl.door.spread..m. = col_double(),
                            Trawl.mouth.opening.height..m. = col_double()
                          )) %>% 
  select(Trip.identifier, Set.number,Survey.Year,Trip.start.date,Trip.end.date, GMA, PFMA,Set.date, Start.latitude,Start.longitude, End.latitude, End.longitude, Bottom.depth..m., Tow.duration..min.,Distance.towed..m., Trawl.door.spread..m., Trawl.mouth.opening.height..m. )


WCHG <- left_join(WCHG_catch, WCHG_effort, by = c("Trip.identifier", "Set.number","Survey.Year"))



WCHG <- WCHG %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(Trip.identifier, width=3, flag=0), formatC(Set.number, width=3, flag=0), sep= "-"), 
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(Start.latitude), floor(Start.longitude),floor(Bottom.depth..m./100)*100, sep= "-"), 
    # catch weight (kg.) per tow	
    wtcpue = (Catch.weight..kg.)#/(Distance.towed..m.*Trawl.door.spread..m.)
  )

# Calculate stratum area where needed (use convex hull approach)
WCHG_strats <- WCHG  %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(Start.longitude, Start.latitude))

WCHG <- left_join(WCHG, WCHG_strats, by = "stratum")

WCHG <- WCHG %>% 
  rename(
    lat = Start.latitude, 
    lon = Start.longitude,
    depth = Bottom.depth..m., 
    spp = Scientific.name,
    year = Survey.Year
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add sub_area column
  mutate(sub_area = "WCHG") %>% 
  select(sub_area, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()



# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- WCHG %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>% 
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)


# combine the wtcpue for each species by haul
WCHG <- WCHG %>% 
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  ungroup() %>% 
  # remove extra columns
  select(haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)


#Hecate Strait


HS_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/HS_catch.csv", col_types = cols(
  Survey.Year = col_integer(),
  Trip.identifier = col_integer(),
  Set.number = col_integer(),
  ITIS.TSN = col_integer(),
  Species.code = col_character(),
  Scientific.name = col_character(),
  English.common.name = col_character(),
  French.common.name = col_character(),
  LSID = col_character(),
  Catch.weight..kg. = col_double(),
  Catch.count..pieces. = col_integer()
)) %>% 
  select(Trip.identifier, Set.number,Survey.Year, ITIS.TSN, Species.code, Scientific.name, English.common.name, Catch.weight..kg.)

HS_effort <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/HS_effort.csv", col_types = 
                        cols(
                          Survey.Year = col_integer(),
                          Trip.identifier = col_integer(),
                          Vessel.name = col_character(),
                          Trip.start.date = col_character(),
                          Trip.end.date = col_character(),
                          GMA = col_character(),
                          PFMA = col_character(),
                          Set.number = col_integer(),
                          Set.date = col_character(),
                          Start.latitude = col_double(),
                          Start.longitude = col_double(),
                          End.latitude = col_double(),
                          End.longitude = col_double(),
                          Bottom.depth..m. = col_double(),
                          Tow.duration..min. = col_integer(),
                          Distance.towed..m. = col_double(),
                          Vessel.speed..m.min. = col_double(),
                          Trawl.door.spread..m. = col_double(),
                          Trawl.mouth.opening.height..m. = col_double()
                        )) %>% 
  select(Trip.identifier, Set.number,Survey.Year,Trip.start.date,Trip.end.date, GMA, PFMA,Set.date, Start.latitude,Start.longitude, End.latitude, End.longitude, Bottom.depth..m., Tow.duration..min.,Distance.towed..m., Trawl.door.spread..m., Trawl.mouth.opening.height..m. )


HS <- left_join(HS_catch, HS_effort, by = c("Trip.identifier", "Set.number","Survey.Year"))



HS <- HS %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(Trip.identifier, width=3, flag=0), formatC(Set.number, width=3, flag=0), sep= "-"), 
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(Start.latitude), floor(Start.longitude),floor(Bottom.depth..m./100)*100, sep= "-"), 
    # catch weight (kg.) per tow	
    wtcpue = (Catch.weight..kg.)#/(Distance.towed..m.*Trawl.door.spread..m.)
  )

# Calculate stratum area where needed (use convex hull approach)
HS_strats <- HS  %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(Start.longitude,Start.latitude))

HS <- left_join(HS, HS_strats, by = "stratum")

HS <- HS %>% 
  rename(
    lat = Start.latitude, 
    lon = Start.longitude,
    depth = Bottom.depth..m., 
    spp = Scientific.name,
    year = Survey.Year
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add sub_area column
  mutate(sub_area = "HS") %>% 
  select(sub_area, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()



# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- HS %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>% 
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)


# combine the wtcpue for each species by haul
HS <- HS %>% 
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  ungroup() %>% 
  # remove extra columns
  select(haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)


#Strait of Georgia


SOG_catch <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/SOG_catch.csv", col_types = cols(
  Survey.Year = col_integer(),
  Trip.identifier = col_integer(),
  Set.number = col_integer(),
  ITIS.TSN = col_integer(),
  Species.code = col_character(),
  Scientific.name = col_character(),
  English.common.name = col_character(),
  French.common.name = col_character(),
  LSID = col_character(),
  Catch.weight..kg. = col_double(),
  Catch.count..pieces. = col_integer()
)) %>% 
  select(Trip.identifier, Set.number,Survey.Year, ITIS.TSN, Species.code, Scientific.name, English.common.name, Catch.weight..kg.)

SOG_effort <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/SOG_effort.csv", col_types = 
                         cols(
                           Survey.Year = col_integer(),
                           Trip.identifier = col_integer(),
                           Vessel.name = col_character(),
                           Trip.start.date = col_character(),
                           Trip.end.date = col_character(),
                           GMA = col_character(),
                           PFMA = col_character(),
                           Set.number = col_integer(),
                           Set.date = col_character(),
                           Start.latitude = col_double(),
                           Start.longitude = col_double(),
                           End.latitude = col_double(),
                           End.longitude = col_double(),
                           Bottom.depth..m. = col_double(),
                           Tow.duration..min. = col_integer(),
                           Distance.towed..m. = col_double(),
                           Vessel.speed..m.min. = col_double(),
                           Trawl.door.spread..m. = col_double(),
                           Trawl.mouth.opening.height..m. = col_double()
                         )) %>% 
  select(Trip.identifier, Set.number,Survey.Year,Trip.start.date,Trip.end.date, GMA, PFMA,Set.date, Start.latitude,Start.longitude, End.latitude, End.longitude, Bottom.depth..m., Tow.duration..min.,Distance.towed..m., Trawl.door.spread..m., Trawl.mouth.opening.height..m. )


SOG <- left_join(SOG_catch, SOG_effort, by = c("Trip.identifier", "Set.number","Survey.Year"))



SOG <- SOG %>% 
  # Create a unique haulid
  mutate(
    haulid = paste(formatC(Trip.identifier, width=3, flag=0), formatC(Set.number, width=3, flag=0), sep= "-"), 
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(Start.latitude), floor(Start.longitude),floor(Bottom.depth..m./100)*100, sep= "-"), 
    # catch weight (kg.) per tow	
    wtcpue = (Catch.weight..kg.)#/(Distance.towed..m.*Trawl.door.spread..m.)
  )

# Calculate stratum area where needed (use convex hull approach)
SOG_strats <- SOG  %>% 
  group_by(stratum) %>% 
  summarise(stratumarea = calcarea(Start.longitude, Start.latitude))

SOG <- left_join(SOG, SOG_strats, by = "stratum")

SOG <- SOG %>% 
  rename(
    lat = Start.latitude, 
    lon = Start.longitude,
    depth = Bottom.depth..m., 
    spp = Scientific.name,
    year = Survey.Year
  ) %>% 
  filter(
    spp != "" & 
      !grepl("egg", spp)
  ) %>% 
  # adjust spp names
  mutate(spp = ifelse(grepl("Lepidopsetta", spp), "Lepidopsetta sp.", spp),
         spp = ifelse(grepl("Bathyraja", spp), 'Bathyraja sp.', spp), 
         spp = ifelse(grepl("Squalus", spp), 'Squalus suckleyi', spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add sub_area column
  mutate(sub_area = "SOG") %>% 
  select(sub_area, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()



# Does the spp column contain any eggs or non-organism notes? As of 2019, nothing stuck out as needing to be removed
test <- SOG %>%
  select(spp) %>%
  filter(!is.na(spp)) %>%
  distinct() %>%
  mutate(spp = as.factor(spp)) %>% 
  filter(grepl("egg", spp) & grepl("", spp))
stopifnot(nrow(test)==0)


# combine the wtcpue for each species by haul
SOG <- SOG %>% 
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  ungroup() %>% 
  # remove extra columns
  select(haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue)



#combine canadian pacific
CPAC <- rbind(QCS, WCV, WCHG, HS, SOG)

#test = setcolorder(scot, c('region', 'haulid', 'year', 'lat', 'lon', 'stratum', 'stratumarea', 'depth', 'spp', 'wtcpue'))
test <- CPAC %>% 
  filter(stratumarea > 0)

#CPAC$year <- as.integer(CPAC$year)


# how many rows will be lost if only stratum trawled ever year are kept?
test2 <- CPAC %>%
  filter(stratum %in% test$stratum)
nrow(CPAC) - nrow(test2)
# percent that will be lost
print((nrow(CPAC) - nrow(test2))/nrow(CPAC))
# 0.9% of rows are removed
test2 <- CPAC %>%
  filter(stratum %in% test$stratum)


CPAC$survey <- 'CPAC'

CPAC <- CPAC %>%
  select(region, everything())

CPAC$spp <- firstup(CPAC$spp)


if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  # plot the strata by year
  p1 <- CPAC %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  p2 <- CPAC %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  CPAC_fltr <- CPAC
  
  # regroup year bins
  # update this when adding a new year!
  # The following line will place data into two year bins
  # bin names (e.g., 2015) refer to the stated year and the one following (e.g., 2015 = 2015-2016)
  # This maintains year as a numeric variable and facilitates all other analyses
  CPAC_fltr$year <- oddtoeven(CPAC_fltr$year)-1
  
  
  # find strata sampled every year
  annual_strata <- CPAC_fltr %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 3)
  
  # find strata sampled every year
  annual_strata_old <- CPAC %>%
    #filter(year != 1986, year != 1978) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n())
  
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test <- CPAC_fltr %>%
    #filter(year != 1986, year != 1978, year!= 2018) %>%
    filter(stratum %in% annual_strata$stratum)
  nrow(CPAC) - nrow(test)
  # percent that will be lost
  print((nrow(CPAC) - nrow(test))/nrow(CPAC))
  # 3.03% are removed
  
  #how much additional data will be lost if we remove years 2003-2004?
  test <- CPAC_fltr%>%
    filter(year != 2003,year != 2019 ) %>%
    filter(stratum %in% annual_strata$stratum)
  nrow(CPAC) - nrow(test)
  # percent that will be lost
  print((nrow(CPAC) - nrow(test))/nrow(CPAC))
  # 18.3% are removed
  
  CPAC_fltr <- CPAC_fltr  %>%
    filter(year != 2003,year != 2019 )  %>%
    filter(stratum %in% test$stratum)
  
  p3 <- CPAC_fltr %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- CPAC_fltr %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2,p3,p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "cpac_hq_dat_removed.png"))
  }
}


rm(SOG, SOG_catch, SOG_effort, SOG_strats, QCS, QCS_catch, QCS_effort, QCS_strats, 
   HS, HS_catch, HS_effort, HS_strats, WCHG, WCHG_catch, WCHG_effort, WCHG_strats,
   WCV, WCV_catch, WCV_effort, WCV_strats)

# Compile Canadian Gulf of Saint Lawrence South ---------------------------------------------------
print("Compile GSL South")

#GSL South

GSLsouth <- read_csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLsouth.csv")

GSLsouth$haulid <- paste(GSLsouth$year,GSLsouth$month,GSLsouth$day,GSLsouth$start.hour,GSLsouth$start.minute, GSLsouth$longitude, GSLsouth$latitude, sep="-")

GSLsouth <- GSLsouth %>% 
  # Create a unique haulid
  mutate(
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    stratum = paste(floor(latitude), floor(longitude), sep= "-"), 
    #stratum_area = NA,
    wgt_cpue = weight.caught,
    num_cpue = number.caught,
    sub_area = NA,
    wgt = NA,
    num = NA,
    num_h = NA,
    wgt_h = NA,
    depth = NA, #No depth data available - fill with NA
    station = NA,
    sst = NA,
    sbt = NA,
    season = NA_character_,
    haul_dur = NA,
    area_swept = NA,
    gear = gear.str,
    country = "Canada",
    continent = "n_america",
    stat_rec = NA,
    verbatim_name = latin.name,
    quarter = case_when(month %in% c(1,2,3) ~ 1,
                        month %in% c(4,5,6) ~ 2,
                        month %in% c(7,8,9) ~ 3,
                        month %in% c(10,11,12) ~ 4),
  )
#don't need stratum_area

# # Calculate stratum area where needed (use convex hull approach)
# GSLsouth_strats <- GSLsouth  %>% 
#   group_by(stratum) %>% 
#   summarise(stratum_area = calcarea(longitude, latitude))
# 
# GSLsouth <- left_join(GSLsouth, GSLsouth_strats, by = "stratum")


#GSLsouth$latin.name <- firstup(GSLsouth$latin.name)
GSLsouth <- GSLsouth %>%
  #filter(
    # remove unidentified spp and non-species
    #spp != "" | !is.na(spp), 
    #!grepl("EGG", spp), 
    #!grepl("UNIDENTIFIED", spp)) %>%
  group_by(haulid, stratum, year, latitude, longitude, depth, verbatim_name) %>% 
  mutate(wgt_cpue = sumna(wgt_cpue),
         num_cpue = sumna(num_cpue),
         survey = "GSL-S") %>% 
  # add survey column
  select(survey, haulid, country, sub_area, continent, stat_rec, station, stratum, year, month, day, quarter, season, latitude, longitude, haul_dur, area_swept, gear, depth, sbt, sst,
         num, num_h, num_cpue, wgt, wgt_h, wgt_cpue, verbatim_name) %>% 
  ungroup()





if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  # plot the strata by year
  p1 <- GSLsouth %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  p2 <- GSLsouth %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test <- GSLsouth %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 29)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- GSLsouth %>%
    filter(stratum %in% test$stratum)
  nrow(GSLsouth) - nrow(test2)
  # percent that will be lost
  print((nrow(GSLsouth) - nrow(test2))/nrow(GSLsouth))
  # 1.2% of rows are removed
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  # and remove first year with very low coverage
  test <- GSLsouth %>%
    filter(year != 1970) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count > 37)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- GSLsouth %>%
    filter(stratum %in% test$stratum)
  nrow(GSLsouth) - nrow(test2)
  # percent that will be lost
  print((nrow(GSLsouth) - nrow(test2))/nrow(GSLsouth))
  # 5.6% of rows removed
  
  
  test3 <- GSLsouth %>%
    filter(year >= 1985)
  #filter(year != 1984,year != 1983,year != 1982,year != 1981,year != 1980,year != 1979)
  
  # how many rows will be lost if only years with all strata are kept?
  test4 <- GSLsouth %>%
    filter(year %in% test3$year)
  nrow(GSLsouth) - nrow(test4)
  # percent that will be lost
  print((nrow(GSLsouth) - nrow(test4))/nrow(GSLsouth))
  # 5.3% of rows are removed
  
  #how many rows will be lost if both years with low coverage and strata with low coverage are dropped?
  test5 <- GSLsouth %>%
    filter(year >= 1985) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 28)
  
  test6 <- GSLsouth %>%
    filter(stratum %in% test5$stratum) %>%
    filter(year >= 1985)
  
  nrow(GSLsouth) - nrow(test6)
  # percent that will be lost
  print((nrow(GSLsouth) - nrow(test6))/nrow(GSLsouth))
  # 7.4% of rows are removed
  
  
  
  # GSLsouth <- GSLsouth  %>%
  #   #filter(year != 1986, year != 1978, year != 2018) %>%
  #   filter(stratum %in% test$stratum) %>%
  #   filter(year >= 1985)
  
  #Filter spatially and first year with very low coverage 
  GSLsouth_fltr <- GSLsouth %>%
    filter(year != 1970) %>%
    filter(stratum %in% test$stratum)
  
  p3 <- GSLsouth_fltr %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- GSLsouth_fltr %>%
    select(lat, lon) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2,p3,p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "GSLsouth_hq_dat_removed.png"))
  }
}


rm(GSLsouth_strats) 

# Compile Canadian Gulf of Saint Lawrence North ---------------------------------------------------
print("Compile GSL North")

#GSL North Sentinel

GSLnor_sent <- read.csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_sentinel.csv")

#GSL North Gadus

GSLnor_gad <- read.csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_gadus.csv")

#GSL North Hammond

GSLnor_ham <- read.csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_hammond.csv")

#GSL North Needler

GSLnor_need <- read.csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_needler.csv")

#GSL North Teleost

GSLnor_tel <- read.csv("https://github.com/pinskylab/OceanAdapt/raw/master/data_raw/GSLnorth_teleost.csv")

#Bind all datasets

GSLnor <- plyr::rbind.fill(GSLnor_sent, GSLnor_gad, GSLnor_ham, GSLnor_need, GSLnor_tel)
GSLnor$lat <-as.numeric(as.character(GSLnor$Latit_Deb))
GSLnor$lon <-as.numeric(as.character(GSLnor$Longit_Deb))
GSLnor$depth <-as.numeric(as.character(GSLnor$Prof_Max))
GSLnor$Dist_Towed <-as.numeric(GSLnor$Dist_Chalute_Position)
GSLnor$Pds_Capture <- as.double(GSLnor$Pds_Capture)
GSLnor$Date <-as.Date(GSLnor$Date_Deb_Trait)
GSLnor$year <- as.integer(year(GSLnor$Date))
GSLnor$spp <- trimws(as.character(GSLnor$Nom_Scient_Esp), which = "right")



#GSLnor$haulid <- paste(GSLnor$No_Releve,GSLnor$Trait,GSLnor$Date_Deb_Trait,GSLnor$Hre_Deb, sep="-")

GSLnor <- GSLnor[!is.na(GSLnor$lat),]
GSLnor <- GSLnor[!is.na(GSLnor$depth),]

GSLnor <- GSLnor %>%
  # Create a unique haulid
  mutate(
    haulid = paste(GSLnor$No_Releve,GSLnor$Trait,GSLnor$Date_Deb_Trait,GSLnor$Hre_Deb, sep="-"),
    # Add "strata" (define by lat, lon and depth bands) where needed # degree bins # 100 m bins # no need to use lon grids on west coast (so narrow)
    #stratum = paste(floor(lat), floor(lon),floor(depth)*100, sep= "-"),
    stratum = paste(floor(lat), floor(lon),plyr::round_any(GSLnor$depth, 100), sep= "-"),
    #weight of catch (kg.) per tow
    wtcpue = (Pds_Capture)#/(Dist_Towed *12.497)
  )


# Calculate stratum area where needed (use convex hull approach)
GSLnor_strats <- GSLnor  %>%
  group_by(stratum) %>%
  summarise(stratumarea = calcarea(lon,lat)) %>%
  ungroup()


GSLnor <- left_join(GSLnor, GSLnor_strats, by = "stratum")

GSLnor <- GSLnor %>%
  filter(
    # remove unidentified spp and non-species
    spp != "" | !is.na(spp), 
    !grepl("EGG", spp), 
    !grepl("UNIDENTIFIED", spp)) %>%
  group_by(haulid, stratum, stratumarea, year, lat, lon, depth, spp) %>% 
  summarise(wtcpue = sumna(wtcpue)) %>% 
  # add survey column
  mutate(survey = "GSL-N") %>% 
  select(survey, haulid, year, lat, lon, stratum, stratumarea, depth, spp, wtcpue) %>% 
  ungroup()

if (HQ_DATA_ONLY == TRUE){
  # look at the graph and make sure decisions to keep or eliminate data make sense
  
  # plot the strata by year
  p1 <- GSLnor %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  p2 <- GSLnor %>%
    select(lon, lat) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  test <- GSLnor %>%
    filter(year != 1979, year != 1980,year != 1981) %>% 
    select(stratum, year) %>% 
    distinct() %>% 
    group_by(stratum) %>% 
    summarise(count = n())  %>%
    filter(count >= 29)
  
  # how many rows will be lost if only stratum trawled ever year are kept?
  test2 <- GSLnor %>% 
    filter(stratum %in% test$stratum)
  nrow(GSLnor) - nrow(test2)
  # percent that will be lost
  print ((nrow(GSLnor) - nrow(test2))/nrow(GSLnor))
  # 10.5% of rows are removed
  
  # find strata sampled every year
  annual_strata <- GSLnor %>%
    filter(year != 1979, year != 1980,year != 1981) %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) %>%
    filter(count >= 29)
  
  # find strata sampled every year
  annual_strata_old <- GSLnor %>%
    select(stratum, year) %>%
    distinct() %>%
    group_by(stratum) %>%
    summarise(count = n()) 
  
  sum(length(unique(annual_strata_old$count)) - length(unique(annual_strata$count)))
  # how many rows will be lost if only stratum trawled ever year are kept?
  # test <- GSLnor %>%
  #   filter(year != 1979, year != 1980,year != 1981) %>%
  #   select(stratum, year) %>%
  #   distinct() %>%
  #   group_by(stratum) %>%
  #   summarise(count = n()) #%>%
  #   #filter(count <=34)
  # 
  # nrow(GSLnor) - nrow(test)
  # # percent that will be lost
  # print((nrow(GSLnor) - nrow(test))/nrow(GSLnor))
  # # 5.6% are removed
  # #
  
  GSLnor_fltr <- GSLnor  %>%
    filter(year != 1979, year != 1980,year != 1981) %>%
    filter(stratum %in% annual_strata$stratum)
  
  p3 <- GSLnor_fltr %>%
    select(stratum, year) %>%
    ggplot(aes(x = as.factor(stratum), y = as.factor(year)))   +
    geom_jitter()
  
  p4 <- GSLnor_fltr %>%
    select(lon, lat) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_jitter()
  
  if (HQ_PLOTS == TRUE){
    temp <- grid.arrange(p1, p2,p3,p4, nrow = 2)
    ggsave(plot = temp, filename = here::here("plots", "GSLnorth_hq_dat_removed.png"))
  }
}

rm(GSLnor_gad, GSLnor_ham, GSLnor_need, GSLnor_sent, GSLnor_tel, GSLnor_strats)


# # Compile Canadian Central and Arctic ---------------------------------------------------
# print("Compile CCA")
# 
# CCA <- read.csv(here::here("data_raw", "CCA.csv"))
# 
# CCA %>% 
#   rename(
#     lat = decimalLatitude,
#     lon = decimalLongitude,
#     depth = maximumDepthInMeters,
#     spp = scientificName
#   )

# Compile TAX ===========================================================
print("Compile TAX")
tax <- read_csv(here::here("data_raw", "spptaxonomy.csv"), col_types = cols(
  taxon = col_character(),
  species = col_character(),
  genus = col_character(),
  family = col_character(),
  order = col_character(),
  class = col_character(),
  superclass = col_character(),
  subphylum = col_character(),
  phylum = col_character(),
  kingdom = col_character(),
  name = col_character(),
  common = col_character()
)) %>% 
  select(taxon, name, common)


# if(isTRUE(WRITE_MASTER_DAT)){
#   save(ai, CPAC, ebs, gmex, goa, GSLnor, GSLsouth, mar, neus_fall, neus_spring, seusFALL, seusSPRING, seusSUMMER, tax, wcann, wctri, file = here("data_clean", "individual-regions.rds"))
# }
# if(isTRUE(WRITE_MASTER_DAT)){
#   save(ai_fltr, CPAC_fltr, ebs_fltr, gmex_fltr, goa_fltr, GSLnor_fltr, GSLsouth_fltr, mar_fltr, neus_fall_fltr, neus_spring_fltr, seusFALL_fltr, seusSPRING_fltr, seusSUMMER_fltr, tax, wcann_fltr, wctri_fltr, file = here("data_clean", "individual-regions-fltr.rds"))
# }

if(isTRUE(WRITE_MASTER_DAT)){
  save(ai, CPAC, ebs, gmex, goa, GSLnor, GSLsouth, mar, neus_fall, neus_spring, seusFALL, seusSPRING, seusSUMMER, tax, wcann, wctri, file = gzfile(here("data_clean", "individual-regions.rds.gz")))
}
if(isTRUE(WRITE_MASTER_DAT)){
  save(ai_fltr, CPAC_fltr, ebs_fltr, gmex_fltr, goa_fltr, GSLnor_fltr, GSLsouth_fltr, mar_fltr, neus_fall_fltr, neus_spring_fltr, seusFALL_fltr, seusSPRING_fltr, seusSUMMER_fltr, tax, wcann_fltr, wctri_fltr, file = gzfile(here("data_clean", "individual-regions-fltr.rds.gz")))
}


# Master Data Set ===========================================================
print("Join into Master Data Set")
dat <- rbind(ai, CPAC, ebs, gmex, goa, GSLnor, GSLsouth, mar, neus_fall, neus_spring, seusFALL, seusSPRING, seusSUMMER, wcann, wctri) %>% 
  # Remove NA values in wtcpue
  filter(!is.na(wtcpue))

# add a case sensitive spp and common name
dat <- left_join(dat, tax, by = c("spp" = "taxon")) %>% 
  select(region, haulid, year, lat, lon, stratum, stratumarea, depth, name, common, wtcpue) %>% 
  distinct() %>% 
  rename(spp = name)


if(isTRUE(REMOVE_REGION_DATASETS)) {
  rm(ai, CPAC, ebs, gmex, goa, GSLnor, GSLsouth, mar, neus_fall, neus_spring, seusFALL, seusSPRING, seusSUMMER, wcann, wctri, tax)
}

if(isTRUE(WRITE_MASTER_DAT)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(dat, file = here::here("data_clean", "all-regions-full.rds"))
  }else{
    write_csv(dat, file = gzfile(here::here("data_clean", "all-regions-full.csv.gz")))
  }
}
# 
# if(isTRUE(WRITE_MASTER_DAT)){
#   if(isTRUE(PREFER_RDATA)){
#     saveRDS(dat, file = gzfile(here::here("data_clean", "all-regions-full.rds.gz")))
#   }else{
#     write_csv(dat, gzfile(here::here("data_clean", "all-regions-full.csv.gz")))
#   }
# }


dat_fltr <- rbind(ai_fltr, CPAC_fltr, ebs_fltr, gmex_fltr, goa_fltr, GSLnor_fltr, GSLsouth_fltr, mar_fltr, neus_fall_fltr, neus_spring_fltr, seusFALL_fltr, seusSPRING_fltr, seusSUMMER_fltr, wcann_fltr, wctri_fltr) %>% 
  # Remove NA values in wtcpue
  filter(!is.na(wtcpue))

# add a case sensitive spp and common name
dat_fltr <- left_join(dat_fltr, tax, by = c("spp" = "taxon")) %>% 
  select(region, haulid, year, lat, lon, stratum, stratumarea, depth, name, common, wtcpue) %>% 
  distinct() %>% 
  rename(spp = name)

# check for errors in name matching
if(sum(dat_fltr$spp == 'NA') > 0 | sum(is.na(dat_fltr$spp)) > 0){
  warning('>>create_master_table(): Did not match on some taxon [Variable: `tax`] names.')
}

if(isTRUE(REMOVE_REGION_DATASETS)) {
  rm(ai_fltr, CPAC_fltr, ebs_fltr, gmex_fltr, goa_fltr, GSLnor_fltr, GSLsouth_fltr, mar_fltr, neus_fall_fltr, neus_spring_fltr, seusFALL_fltr, seusSPRING_fltr, seusSUMMER_fltr, wcann_fltr, wctri_fltr, tax)
}

if(isTRUE(WRITE_MASTER_DAT)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(dat_fltr, file = here::here("data_clean", "all-regions-full-fltr.rds"))
  }else{
    write_csv(dat_fltr, gzfile(here::here("data_clean", "all-regions-full-fltr.csv.gz")))
  }
}

# if(isTRUE(WRITE_MASTER_DAT)){
#   if(isTRUE(PREFER_RDATA)){
#     saveRDS(dat_fltr, file = gzfile(here::here("data_clean", "all-regions-full-fltr.rds.gz")))
#   }else{
#     write_csv(dat_fltr, gzfile(here::here("data_clean", "all-regions-full-fltr.csv.gz")))
#   }
# }

###STOP#####
#if this is the first time running compile.R for this year's annual update, now open add-spp-to-taxonomy.Rmd and run this script.
#Once you have produced new "flagspp" files, refer to these and compare those in the files with those in exclude_spp
#If nothing changed, continue script as is
#If new species have appeared in flagspp, contact data providers

# At this point, we have a compiled `dat` master table on which we can begin our analysis.

# If you have not cleared the regional datasets {By setting REMOVE_REGION_DATASETS=FALSE at the top}, 
#you are free to do analysis on those sets individually as well.

##FEEL FREE TO ADD, MODIFY, OR DELETE ANYTHING BELOW THIS LINE
# Trim species ===========================================================
print("Trim species")

#FULL DATA

# Find a standard set of species (present at least 3/4 of the years in a region)
# this result differs from the original code because it does not include any species that have a pres value of 0.  It does, however, include species for which the common name is NA.
presyr <- present_every_year(dat, region, spp, common, year) 

# years in which spp was present
presyrsum <- num_year_present(presyr, region, spp, common)

# max num years of survey in each region
maxyrs <- max_year_surv(presyrsum, region)

# merge in max years
presyrsum <- left_join(presyrsum, maxyrs, by = "region")

# retain all spp present at least 3/4 of the available years in a survey
spplist <- presyrsum %>% 
  filter(presyr >= (maxyrs * 3/4)) %>% 
  select(region, spp, common)
#any flagged species in spplist?

#remove flagged spp
temp = list.files(here("~/OceanAdapt/OceanAdapt/spp_QAQC/exclude_spp/"), pattern="*.csv")
myfiles = lapply(here("spp_QAQC/exclude_spp",temp), read.csv)
myfiles[[6]] <- NULL #removes empty item for GSLnor (no flagged spp)
myfiles[[6]] <- NULL #removes empty item for GSLsouth (no flagged spp)
myfiles[[13]] <- NULL #removes ineffective check for wctri
names(myfiles[[4]]) <- names(myfiles[[1]]) #adjusts column names of Gulf of Mexico (after removing spp. in add-spp-to-taxonomy.Rmd)
excludespp <- do.call(rbind, myfiles)
names(excludespp)[1] <- "spp"
#Add a true/false flag to the tax_added csv for flagged species
test <- merge(spplist, excludespp)
length(test$exclude[test$exclude == TRUE])
#8 flagged species in the list, 7 listed as exclude (all in Gulf of Alaska) 
test <- filter(test, exclude == TRUE)

spplist$exclude <- match(paste(spplist$region,spplist$spp), paste(test$region,test$spp))
#test = the number of flagged spp
test <- max(na.rm(spplist$exclude))
#overwrite count with TRUE/FALSE
spplist$exclude <- !is.na(spplist$exclude)
#exclude 7 "TRUE" species in list
spplist <- filter(spplist, exclude == FALSE)


# Trim dat to these species (for a given region, spp pair in spplist, in dat, keep only rows that match that region, spp pairing)
trimmed_dat <- dat %>% 
  filter(paste(region, spp) %in% paste(spplist$region, spplist$spp)) %>% 
  # some spp have whitespace - this should potentially be moved up to NEUS section
  mutate(
    spp = ifelse(grepl("LIMANDA FERRUGINEA", spp), "LIMANDA FERRUGINEA", spp),
    spp = ifelse(grepl("PSEUDOPLEURONECTES AMERICANUS", spp), "PSEUDOPLEURONECTES AMERICANUS", spp))

rm (maxyrs, presyr, presyrsum)

if(isTRUE(WRITE_TRIMMED_DAT)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(trimmed_dat, file = here::here("data_clean", "all-regions-trimmed.rds"))
  }else{
    write_csv(trimmed_dat, gzfile(here::here("data_clean", "all-regions-trimmed.csv.gz")))
  }
}


# are there any spp in trimmed_dat that are not in the taxonomy file?
test <- anti_join(select(trimmed_dat, spp, common), spplist, by = "spp") %>% 
  distinct()

# if test contains more than 0 obs, use the add-spp-to-taxonomy.R script to add new taxa to the spptaxonomy.csv and go back to "Compile Tax".
rm(test)

## FILTERED DATA

# Trim dat to these species (for a given region, spp pair in spplist, in dat, keep only rows that match that region, spp pairing)
trimmed_dat_fltr <- dat_fltr %>% 
  filter(paste(region, spp) %in% paste(spplist$region, spplist$spp)) %>% 
  # some spp have whitespace - this should potentially be moved up to NEUS section
  mutate(
    spp = ifelse(grepl("LIMANDA FERRUGINEA", spp), "LIMANDA FERRUGINEA", spp),
    spp = ifelse(grepl("PSEUDOPLEURONECTES AMERICANUS", spp), "PSEUDOPLEURONECTES AMERICANUS", spp))

#rm(spplist)

if(isTRUE(WRITE_TRIMMED_DAT)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(trimmed_dat, file = here::here("data_clean", "all-regions-trimmed-fltr.rds"))
  }else{
    write_csv(trimmed_dat, gzfile(here::here("data_clean", "all-regions-trimmed-fltr.csv.gz")))
  }
}

# are there any spp in trimmed_dat that are not in the taxonomy file?
test <- anti_join(select(trimmed_dat_fltr, spp, common), spplist, by = "spp") %>% 
  distinct()

# if test contains more than 0 obs, use the add-spp-to-taxonomy.R script to add new taxa to the spptaxonomy.csv and go back to "Compile Tax".
rm(test)


# BY_SPECIES_DATA ===========================================================
print("By species data")
# Calculate mean position through time for species 
## Calculate mean latitude and depth of each species by year within each survey/region
### mean lat/lon/depth for each stratum
dat_strat <- trimmed_dat_fltr %>% 
  select(stratum, region, lat, lon, depth, stratumarea, haulid) %>% 
  distinct(region, stratum, haulid, .keep_all = T) %>% 
  group_by(stratum, region) %>% 
  summarise(lat = meanna(lat), 
            lon = meanna(lon), 
            depth = meanna(depth), 
            stratumarea = meanna(stratumarea))

### mean wtcpue in each stratum/yr/spp (new code includes more lines because it
### includes rows that do not have a common name)
dat_strat_yr <- trimmed_dat_fltr %>% 
  group_by(region, spp, common, stratum, year) %>% 
  summarise(wtcpue = meanna(wtcpue))

# add stratum lat/lon/depth/area
dat_strat_yr <- left_join(dat_strat_yr, dat_strat, by = c("region", "stratum"))

# index of biomass per stratum: mean wtcpue times area
dat_strat_yr <- dat_strat_yr %>% 
  mutate(wttot = wtcpue * stratumarea)

# calculate mean lat
cent_bio_lat <- dat_strat_yr %>% 
  group_by(region, spp, year) %>% 
  summarise(lat = questionr::wtd.mean(lat, wttot, na.rm = TRUE))

# mean depth
cent_bio_depth <- dat_strat_yr %>% 
  group_by(region, spp, year) %>% 
  summarise(depth = questionr::wtd.mean(depth, wttot, na.rm = TRUE))

# mean lon
cent_bio_lon <- dat_strat_yr %>% 
  group_by(region, spp, year) %>% 
  summarise(lon = questionr::wtd.mean(lon, wttot, na.rm = TRUE))

# merge
cent_bio <- left_join(cent_bio_lat, cent_bio_depth, by = c("region", "spp", "year"))
cent_bio <- left_join(cent_bio, cent_bio_lon, by = c("region", "spp",  "year"))

# standard error for lat
cent_bio_lat_se <- dat_strat_yr %>%
  group_by(region, spp, year) %>% 
  summarise(lat_se = sqrt(questionr::wtd.var(lat, wttot, na.rm=TRUE, normwt=TRUE))/sqrt(sum(!is.na(lat) & !is.na(wttot))))

cent_bio <- left_join(cent_bio, cent_bio_lat_se, by = c("region", "spp", "year"))

cent_bio_depth_se <- dat_strat_yr %>%
  group_by(region, spp, year) %>% 
  summarise(depth_se = sqrt(questionr::wtd.var(depth, wttot, na.rm=TRUE, normwt=TRUE))/sqrt(sum(!is.na(depth) & !is.na(wttot))))

cent_bio <- left_join(cent_bio, cent_bio_depth_se, by = c("region", "spp", "year"))

cent_bio_lon_se <- dat_strat_yr %>%
  group_by(region, spp, year) %>% 
  summarise(lon_se = sqrt(questionr::wtd.var(lon, wttot, na.rm=TRUE, normwt=TRUE))/sqrt(sum(!is.na(lon) & !is.na(wttot))))

cent_bio <- left_join(cent_bio, cent_bio_lon_se, by = c("region", "spp", "year"))

BY_SPECIES_DATA <- cent_bio %>%
  ungroup() %>% 
  arrange(region, spp, year)

if(isTRUE(WRITE_BY_TABLES)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(BY_SPECIES_DATA, file = here::here("data_clean", "by_species.rds"))
  }else{
    write_csv(BY_SPECIES_DATA, here::here("data_clean", "by_species.csv"))
  }
}

rm(cent_bio, cent_bio_depth, cent_bio_depth_se, cent_bio_lat, cent_bio_lat_se, cent_bio_lon, cent_bio_lon_se, dat_strat, dat_strat_yr)

# Dat_exploded -  Add 0's ======================================================
print("Dat exploded") 
# these Sys.time() flags are here::here to see how long this section of code takes to run.
Sys.time()
# This takes about 10 minutes
if (DAT_EXPLODED == TRUE){
  dat.exploded <- as.data.table(dat)[,explode0(.SD), by="region"]
  dat_expl_spl <- split(dat.exploded, dat.exploded$region, drop = FALSE)
  
  if(isTRUE(WRITE_DAT_EXPLODED)){
    if(isTRUE(PREFER_RDATA)){
      lapply(dat_expl_spl, function(x) saveRDS(x, here::here("data_clean", paste0('dat_exploded', x$region[1], '.rds')))) 
    }else{
      lapply(dat_expl_spl, function(x) write_csv(x, gzfile(here::here("data_clean", paste0('dat_exploded', x$region[1], '.csv.gz')))))
    }
  }
  
}
Sys.time()

#clean up
rm(dat_expl_spl)

#By region data ================================================
print("by region data")

#Requires function species_data's dataset [by default: BY_SPECIES_DATA] or this function will not run properly.
## Calculate mean position through time for regions 
## Find a standard set of species (present every year in a region)
presyr <- present_every_year(dat_fltr, region, spp, year)

# num years in which spp was present
presyrsum <- num_year_present(presyr, region, spp)

# max num years of survey in each region
maxyrs <- max_year_surv(presyrsum, region)


# merge in max years
presyrsum <- left_join(presyrsum, maxyrs, by = "region") 

# retain all spp present at least once every time a survey occurs
spplist <- presyrsum %>% 
  filter(presyr >= (maxyrs)) %>% 
  select(region, spp)

# Make a new centbio dataframe for regional use, only has spp in spplist
centbio2 <- BY_SPECIES_DATA %>% 
  filter(paste0(region, spp) %in% paste0(spplist$region, spplist$spp))

# Calculate offsets of lat and depth (start at 0 in initial year of survey)
# find initial year in each region
startyear <- centbio2 %>%
  group_by(region) %>% 
  summarise(startyear = min(year))

# add to dataframe
centbio2 <- left_join(centbio2, startyear, by = "region")
# find starting lat and depth by spp
startpos <- centbio2 %>% 
  ungroup() %>% 
  filter(year == startyear) %>% 
  select(region, spp, lat, lon, depth) %>% 
  rename(startlat = lat, 
         startlon = lon, 
         startdepth = depth)

# add in starting lat and depth
centbio2 <- left_join(centbio2, startpos, by = c("region", "spp")) 



centbio2 <- centbio2 %>% 
  mutate(latoffset = lat - startlat, 
         lonoffset = lon - startlon,
         depthoffset = depth - startdepth)


# Calculate regional average offsets
regcentbio <- centbio2 %>% 
  group_by(year, region) %>% 
  summarise(lat = mean(latoffset), 
            depth = mean(depthoffset), 
            lon = mean(lonoffset))

regcentbiose <- centbio2 %>% 
  group_by(year, region) %>% 
  summarise(lat_se = se(latoffset), 
            depth_se = se(depthoffset), 
            lonse = se(lonoffset))

# calc number of species per region
regcentbiospp <- centbio2 %>% 
  ungroup() %>% 
  select(region, spp) %>% 
  distinct() %>% 
  group_by(region) %>% 
  summarise(numspp = n()) 

regcentbio <- left_join(regcentbio, regcentbiose, by = c("year", "region"))
regcentbio <- left_join(regcentbio, regcentbiospp, by = "region")


# order by region, year
BY_REGION_DATA  <- regcentbio %>% 
  arrange(region, year)

if(isTRUE(WRITE_BY_TABLES)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(BY_REGION_DATA, file = here::here("data_clean", "by_region.rds"))
  }else{
    write_csv(BY_REGION_DATA, here::here("data_clean", "by_region.csv"))
  }
}

# By national data ===========================================================
print("by national data")

#Returns national data
#Requires function species_data's dataset [by default: BY_SPECIES_DATA] or this function will not run properly.

## Calculate mean position through time for the US #


# Only include regions not constrained by geography in which surveys have consistent methods through time
regstouse <- c('Eastern Bering Sea', 'Northeast US Spring', 'Northeast US Fall') 

natstartyear <- 1982 # a common starting year for the focal regions

# find the latest year that all regions have in common
maxyears <- dat_fltr %>% 
  filter(region %in% regstouse) %>% 
  group_by(region) %>% 
  summarise(maxyear = max(year))

natendyear <- min(maxyears$maxyear)

## Find a standard set of species (present every year in the focal regions) for the national analysis
# For national average, start in prescribed year, only use focal regions
# find which species are present in which years
presyr <- present_every_year(dat_fltr, region, spp, year) %>% 
  filter(year >= natstartyear & year <= natendyear & 
           region %in% regstouse)  

# num years in which spp was present
presyrsum <- num_year_present(presyr, region, spp)

# max num years of survey in each region
maxyars <- max_year_surv(presyrsum, region)

# merge in max years
presyrsum <- left_join(presyrsum, maxyars, by = "region") 

# retain all spp present at least once every time a survey occurs
spplist2 <- presyrsum %>% 
  filter(paste0(region,presyr) %in% paste0(maxyars$region, maxyars$maxyrs)) %>% 
  select(region, spp)

# Make a new centbio dataframe for regional use, only has spp in spplist
centbio3 <- BY_SPECIES_DATA %>% 
  ungroup() %>% 
  filter(paste(region, spp) %in% paste(spplist2$region, spplist2$spp), 
         year >= natstartyear & year <= natendyear) %>% 
  select(region, spp, year, lat, lon, depth)

# Calculate offsets of lat and depth (start at 0 in initial year of survey)
# find initial year in each region
startyear <- centbio3 %>% 
  group_by(region) %>% 
  summarise(startyear = min(year))

# add to dataframe
centbio3 <- left_join(centbio3, startyear, by = "region") 

# find starting lat and depth by spp
startpos <- centbio3 %>% 
  filter(year == startyear) %>% 
  select(region, spp, lat, lon, depth) %>% 
  rename(startlat = lat, 
         startlon = lon, 
         startdepth = depth)

# add in starting lat and depth
centbio3 <- left_join(centbio3, startpos, by = c("region", "spp")) 

centbio3 <- centbio3 %>% 
  mutate(latoffset = lat - startlat,
         lonoffset = lon - startlon,
         depthoffset = depth - startdepth)

# Calculate national average offsets
natcentbio <- centbio3 %>% 
  group_by(year) %>% 
  summarise(lat = mean(latoffset), 
            depth = mean(depthoffset), 
            lon = mean(lonoffset))

natcentbiose <- centbio3 %>% 
  group_by(year) %>% 
  summarise(lat_se = se(latoffset), 
            depth_se = se(depthoffset), 
            lonse = se(lonoffset))

natcentbio <- left_join(natcentbio, natcentbiose, by = "year")

natcentbio$numspp <- lunique(paste(centbio3$region, centbio3$spp)) # calc number of species per region  

BY_NATIONAL_DATA <- natcentbio

if(isTRUE(WRITE_BY_TABLES)){
  if(isTRUE(PREFER_RDATA)){
    saveRDS(BY_NATIONAL_DATA, file = here::here("data_clean", "by_national.rds"))
  }else{
    write_csv(BY_NATIONAL_DATA, here::here("data_clean", "by_national.csv"))
  }
}

rm(centbio2, centbio3, maxyrs, natcentbio, natcentbiose, presyr, presyrsum, regcentbio, regcentbiospp, spplist, spplist2, startpos, startyear, regcentbiose)


if(isTRUE(PLOT_CHARTS)) {
  
  # Plot Species #####
  
  centbio <- BY_SPECIES_DATA
  centbio <- centbio[!is.na(centbio$spp),]
  
  # for latitude
  print("Starting latitude plots for species")
  pdf(file = here("plots", "sppcentlatstrat.pdf"), width=10, height=8)
  
  regs = sort(unique(centbio$region))
  for(i in 1:length(regs)){
    print(i)
    par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)
    spps = sort(unique(centbio$spp[centbio$region == regs[i]]))  
    
    
    xlims = range(as.numeric(centbio$year[centbio$region == regs[i]]))
    
    for(j in 1:length(spps)){
      inds = centbio$spp == spps[j] & centbio$region == regs[i]
      minlat = centbio$lat[inds] - centbio$lat_se[inds]
      maxlat = centbio$lat[inds] + centbio$lat_se[inds]
      minlat[is.na(minlat) | is.infinite(minlat)] = centbio$lat[inds][is.na(minlat) | is.infinite(minlat)] # fill in missing values so that polygon draws correctly
      maxlat[is.na(maxlat) | is.infinite(maxlat)] = centbio$lat[inds][is.na(maxlat) | is.infinite(maxlat)]
      ylims = c(min(minlat, na.rm=TRUE), max(maxlat, na.rm=TRUE))
      
      plot(0,0, type='l', ylab='Latitude (°)', xlab='Year', ylim=ylims, xlim=xlims, main=spps[j], las=1)
      polygon(c(centbio$year[inds], rev(centbio$year[inds])), c(maxlat, rev(minlat)), col='#CBD5E8', border=NA)
      lines(centbio$year[inds], centbio$lat[inds], col='#D95F02', lwd=2)
      
      if((j-1) %% 6 == 0) mtext(text='Latitude (°N)', side=2, line=2.3, cex=0.6)
      if(j %% 36 < 7) mtext(text=regs[i], side=3, line=1.3, cex=0.6)
    }
  }
  
  dev.off()
  
  
  # for depth
  print("Starting depth plots for species")
  pdf(file = here("plots", "sppcentdepthstrat.pdf"), width=10, height=8)
  
  centbio <- centbio[!is.na(centbio$depth),]
  
  regs = sort(unique(centbio$region))
  for(i in 1:length(regs)){
    print(i)
    par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)
    spps = sort(unique(centbio$spp[centbio$region == regs[i]]))  
    
    xlims = range(as.numeric(centbio$year[centbio$region == regs[i]]))
    
    for(j in 1:length(spps)){
      inds = centbio$spp == spps[j] & centbio$region == regs[i]
      mindep = centbio$depth[inds] - centbio$depth_se[inds]
      maxdep = centbio$depth[inds] + centbio$depth_se[inds]
      mindep[is.na(mindep) | is.infinite(mindep)] = centbio$depth[inds][is.na(mindep) | is.infinite(mindep)] # fill in missing values so that polygon draws correctly
      maxdep[is.na(maxdep) | is.infinite(maxdep)] = centbio$depth[inds][is.na(maxdep) | is.infinite(maxdep)]
      ylims = c(min(mindep, na.rm=TRUE), max(maxdep, na.rm=TRUE))
      
      plot(0,0, type='l', ylab='Depth (m)', xlab='Year', ylim=ylims, xlim=xlims, main=spps[j], las=1)
      polygon(c(centbio$year[inds], rev(centbio$year[inds])), c(maxdep, rev(mindep)), col='#CBD5E8', border=NA)
      lines(centbio$year[inds], centbio$depth[inds], col='#D95F02', lwd=2)
      
      if((j-1) %% 6 == 0) mtext(text='Depth (m)', side=2, line=2.3, cex=0.6)
      if(j %% 36 < 7) mtext(text=regs[i], side=3, line=1.3, cex=0.6)
    }
  }
  
  dev.off()
  
  
  # Plot Regional ####
  
  reg_lat_depth <- BY_REGION_DATA %>%
    ungroup() %>%
    mutate(year = as.numeric(year),
           mindpeth = depth - depth_se,
           maxdepth = depth + depth_se,
           minlat = lat - lat_se,
           maxlat = lat + lat_se, 
           minlat = ifelse(is.na(minlat), lat, minlat), 
           maxlat = ifelse(is.na(maxlat), lat, maxlat), 
           mindepth = ifelse(is.na(mindpeth), depth, mindpeth),
           maxdepth = ifelse(is.na(maxdepth), depth, maxdepth))
  
  
  reg_lat_plot <- ggplot(data = reg_lat_depth, aes(x=year, y=lat, ymin=minlat, ymax=maxlat)) + 
    geom_line(color = "#D95F02") + 
    geom_ribbon(alpha=0.5, color = "#CBD5E8") + 
    theme_bw ()+
    theme(
      panel.border = element_rect(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = "12", hjust = 0.5)
    ) +
    xlab("Year") + 
    ylab("Offset in latitude (°)") +
    ggtitle("Regional Latitude Offset") +
    facet_wrap(vars(region)) +
    scale_x_continuous(limit=c(1970,2020)
                       ,breaks=seq(1970,2020,15)
    )
  ggsave(reg_lat_plot, filename =  here::here("plots", "regional-lat.png"), width = 8.5, height = 11)
  
  reg_depth_plot <- ggplot(data = reg_lat_depth, aes(x=year, y=depth, ymin=mindepth, ymax=maxdepth)) + 
    geom_line(color = "#D95F02") + 
    geom_ribbon(alpha=0.5, color = "#CBD5E8") + 
    theme_bw ()+
    theme(
      panel.border = element_rect(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = "12", hjust = 0.5)
    ) +
    xlab("Year") + 
    ylab("Offset in depth (m)") +
    ggtitle("Regional Depth Offset") +
    facet_wrap(vars(region)) +
    scale_x_continuous(limit=c(1970,2020)
                       ,breaks=seq(1970,2020,15)
    )
  ggsave(reg_depth_plot, filename =  here::here("plots", "regional-depth.png"), width = 8.5, height = 11)
  
  
  # Plot National ####
  
  nat_lat_depth <- BY_NATIONAL_DATA %>%
    ungroup() %>%
    mutate(year = as.numeric(year),
           mindpeth = depth - depth_se,
           maxdepth = depth + depth_se,
           minlat = lat - lat_se,
           maxlat = lat + lat_se, 
           minlat = ifelse(is.na(minlat), lat, minlat), 
           maxlat = ifelse(is.na(maxlat), lat, maxlat), 
           mindepth = ifelse(is.na(mindpeth), depth, mindpeth),
           maxdepth = ifelse(is.na(maxdepth), depth, maxdepth))
  
  
  nat_lat_plot <- ggplot(data = nat_lat_depth, aes(x=year, y=lat, ymin=minlat, ymax=maxlat)) + 
    geom_line(color = "#D95F02") + 
    geom_ribbon(alpha=0.5, color = "#CBD5E8") + 
    theme_bw ()+
    theme(
      panel.border = element_rect(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = "12", hjust = 0.5, )
    ) +
    xlab("Year") + 
    ylab("Offset in latitude (°)") +
    ggtitle("National Latitude Offset") +
    scale_x_continuous(limit=c(1980,2020)
                       ,breaks=seq(1980,2020,15)
    )
  ggsave(nat_lat_plot, filename =  here::here("plots", "national-lat.png"), width = 6, height = 3.5)
  
  nat_depth_plot <- ggplot(data = nat_lat_depth, aes(x=year, y=depth, ymin=mindepth, ymax=maxdepth)) + 
    geom_line(color = "#D95F02") + 
    geom_ribbon(alpha=0.5, color = "#CBD5E8") + 
    theme_bw ()+
    theme(
      panel.border = element_rect(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = "12", hjust = 0.5)
    ) +
    xlab("Year") + 
    ylab("Offset in depth (m)") +
    ggtitle("National Depth Offset") +
    scale_x_continuous(limit=c(1980,2020)
                       ,breaks=seq(1980,2020,15)
    )
  ggsave(nat_depth_plot, filename =  here::here("plots", "national-depth.png"), width = 6, height = 3.5)
  
}
