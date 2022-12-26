################################################################################
#### R code to merge all separate datasets
#### Coding: Juliano Palacios Abrantes & Aurore A. Maureaud, December 2022
################################################################################

rm(list=ls())

# Load libraries
library(googledrive)
library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(readxl)
#devtools::install_github("r-barnes/dggridR", vignette=TRUE)
library(dggridR)

# Load relevant functions
source(here("./functions/write_clean_data.r"))
columns <- as.data.frame(read_excel(here("standard_formats/fishglob_data_columns.xlsx")))[,1]


#-------------------------------------------------------------------------------------------#
#### DOWNLOAD ALL RELEVANT DATASETS & merge ####
#-------------------------------------------------------------------------------------------#
# Load cleaned surveys
surveys <- c("AI",
             "DATRAS_v3",
             "EBS_v2",
             "GMEX",
             "GOA",
             "GSL-N_v2",
             "GSL-S_v2",
             "HS",
             "NEUS",
             "NOR-BTS_v3",
             "QCS",
             "SCS",
             "SOG",
             "SEUS",
             "WCANN",
             "WCHG",
             "WCTRI",
             "WCVI")

for(f in 1:length(surveys)){
  load(paste0("outputs/Cleaned_data/",surveys[f],"_clean.RData"))
  assign(surveys[f], data)
  rm(data)
}

fishglob <- data.frame()
for(f in 1:length(surveys)){
  xx <- get(surveys[f]) %>% 
    select(columns)
  xx$timestamp <- as.character(xx$timestamp)
  assign(surveys[f], xx) 
  
  rm(xx)
  if(identical(columns, names(get(surveys[f])))==TRUE){
    fishglob <- rbind(fishglob, get(surveys[f]))
  } else {
    missing_col <- setdiff(columns, names(get(surveys[f])))
    print(paste0(surveys[f], " columns not identical to fishglob format: ",missing_col))
  }
}

rm(surveys, columns)


#-------------------------------------------------------------------------------------------#
#### SURVEY UNIT ####
#-------------------------------------------------------------------------------------------#

# separate surveys per seasons/quarters/months
fishglob <- fishglob %>% 
  mutate(survey_unit = survey,
         survey_unit = ifelse(survey %in% c("BITS","NS-IBTS","SWC-IBTS"),
                              paste0(survey,"-",quarter),
                              survey_unit),
         survey_unit = ifelse(survey %in% c("NEUS","SEUS","SCS","GMEX"),
                              paste0(survey,"-",season),
                              survey_unit))


#-------------------------------------------------------------------------------------------#
#### SAVE SURVEY PRODUCT ####
#-------------------------------------------------------------------------------------------#

write_clean_data(data = fishglob, survey = "FishGlob_public_v1.7",
                 overwrite = T, compiled = TRUE)


#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#
surveys <- sort(unique(fishglob$survey))
survey_units <- sort(unique(fishglob$survey_unit))
fishglob_std <- fishglob %>% 
  mutate(flag_taxa = NA_character_,
         flag_trimming_hex7_0 = NA_character_,
         flag_trimming_hex7_2 = NA_character_,
         flag_trimming_hex8_0 = NA_character_,
         flag_trimming_hex8_2 = NA_character_,
         flag_trimming_2 = NA_character_)

# integrate taxonomic flags
for(i in 1:length(surveys)){
  if(!surveys[i] %in% c("FALK","GSL-N","MRT","NZ-CHAT","SCS", "SWC-IBTS")){
    xx <- data.frame(read_delim(paste0("standardization_steps/outputs/taxonomic_flagging/",
                                       surveys[i],"_flagspp.txt"),
                                delim=";", escape_double = FALSE, col_names = FALSE,
                                trim_ws = TRUE))
    xx <- as.vector(unlist(xx[1,]))
    
    fishglob_std <- fishglob_std %>% 
      mutate(flag_taxa = ifelse(survey == surveys[i] & accepted_name %in% xx,
                                "TRUE",flag_taxa))
    
    rm(xx)
  }
}

# integrate spatio-temporal flags
for(i in 1:length(survey_units)){
  
  if(!survey_units[i] %in% c("DFO-SOG","IS-TAU","SCS-FALL","WBLS")){
    
    hex_res7_0 <- read.csv(paste0("standardization_steps/outputs/trimming_method1/hex_res7/",
                                  survey_units[i], "_hex_res_7_trimming_0_hauls_removed.csv"),
                           sep = ";")
    hex_res7_0 <- as.vector(hex_res7_0[,1])
    
    hex_res7_2 <- read.csv(paste0("standardization_steps/outputs/trimming_method1/hex_res7/",
                                  survey_units[i], "_hex_res_7_trimming_02_hauls_removed.csv"),
                           sep = ";")
    hex_res7_2 <- as.vector(hex_res7_2[,1])
    
    hex_res8_0 <- read.csv(paste0("standardization_steps/outputs/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_0_hauls_removed.csv"),
                           sep= ";")
    hex_res8_0 <- as.vector(hex_res8_0[,1])
    
    hex_res8_2 <- read.csv(paste0("standardization_steps/outputs/trimming_method1/hex_res8/",
                                  survey_units[i], "_hex_res_8_trimming_02_hauls_removed.csv"),
                           sep = ";")
    hex_res8_2 <- as.vector(hex_res8_2[,1])
    
    trim_2 <- read.csv(paste0("standardization_steps/outputs/trimming_method2/",
                              survey_units[i],"_hauls_removed.csv"))
    trim_2 <- as.vector(trim_2[,1])
    
    fishglob_std <- fishglob_std %>% 
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
write_clean_data(data = fishglob_std, survey = "FishGlob_std_public_v1.7_std",
                 overwrite = T, compiled = TRUE)


#-------------------------------------------------------------------------------------------#
#### METADATA PRODUCT ####
#-------------------------------------------------------------------------------------------#

fishglob_public_metadata <- fishglob %>%
  select(survey, latitude, longitude, year, survey_unit) %>%
  distinct()

write_clean_data(data = fishglob_public_metadata,
                 survey = "FishGlob_public_metadata_v1.7",
                 overwrite = TRUE, compiled = TRUE)
