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
source("functions/write_clean_data.r")


#-------------------------------------------------------------------------------------------#
#### DOWNLOAD ALL RELEVANT DATASETS & merge ####
#-------------------------------------------------------------------------------------------#
# Load cleaned surveys
surveys <- c("AI",
             "CHL_v2",
             "COL",
             "DATRAS_v3",
             "DFO-NF",
             "EBS_v2",
             "FALK",
             "GIN_v2",
             "GMEX",
             "GOA",
             "GRL-DE",
             "GSL-N_v2",
             "GSL-S_v2",
             "HS",
             "ICE-GFS_v3",
             "IS-TAU",
             "MEDITS_v2",
             "MRT",
             "NAM",
             "NEUS",
             "NOR-BTS_v3",
             "NZ_v5",
             "QCS",
             "S_GEORG",
             "SCS",
             "SOG",
             "SEUS",
             "WBLS",
             "WCANN",
             "WCHG",
             "WCTRI",
             "WCVI",
             "ZAF_v2")

for(f in 1:length(surveys)){
  # x <- drive_find(pattern = paste0(surveys[f],"_clean.csv"))
  # drive_download(x[1,], overwrite = TRUE)
  # rm(x)
  # assign(surveys.files[f]) <- read.csv(paste0("cleaning.codes/",surveys[f],".csv"))
  assign(surveys[f], read_csv(paste0("~/Documents/FISHGLOB/data/Cleaned_data_DEC_2022_1/",surveys[f],"_clean.csv")))
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

# Just run this routine should be good for all
write_clean_data(data = fishglob, survey = "FISHGLOB_v1.7",
                 overwrite = T)

# write.csv(fishglob, file = "E:/fishglob data/Compiled_data/public_private/FISHGLOB_v1.4.csv")

# Write public data file
fishglob_public <- fishglob %>% 
  filter(!survey %in% c("CHL",
                        "COL",
                        "DFO-NF",
                        "FALK",
                        "GIN",
                        "GRL-DE",
                        "ICE-GFS",
                        "IS-TAU",
                        "IS-MOAG",
                        "MEDITS",
                        "MRT",
                        "NAM",
                        "NZ-CHAT",
                        "NZ-WCSI",
                        "NZ-ECSI",
                        "NZ-SUBA",
                        "S-GEORG",
                        "WBLS",
                        "ZAF"))

write_clean_data(data = fishglob_public, survey = "FISHGLOB_public_v1.7",
                 overwrite = T)

# write.csv(fishglob_public, file = "E:/fishglob data/Compiled_data/public/FISHGLOB_public_v1.4.csv")

# with traits
# write.csv(fishglob_trait, file = "E:/fishglob data/Compiled_data/public_private/FISHGLOB_surveys_and_traits_v1.4.csv")

# Write public data file
# fishglob_trait_public <- fishglob_trait %>% 
#   filter(!survey %in% c("CHL",
#                         "COL",
#                         "DFO-NF",
#                         "FALK",
#                         "GIN",
#                         "GRL-DE",
#                         "ICE-GFS",
#                         "IS-TAU",
#                         "IS-MOAG",
#                         "MEDITS",
#                         "MRT",
#                         "NAM",
#                         "NZ-CHAT",
#                         "NZ-WCSI",
#                         "NZ-ECSI",
#                         "NZ-SUBA",
#                         "S-GEORG",
#                         "WBLS",
#                         "ZAF"))

# write.csv(fishglob_trait_public, file = "E:/fishglob data/Compiled_data/public/FISHGLOB_surveys_and_traits_v1.4.csv")




#-------------------------------------------------------------------------------------------#
#### ADD STRANDARDIZATION FLAGS ####
#-------------------------------------------------------------------------------------------#
surveys <- sort(unique(fishglob$survey))
survey_units <- sort(unique(fishglob$survey_unit))
fishglob <- fishglob %>% 
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
    
    fishglob <- fishglob %>% 
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
    
    fishglob <- fishglob %>% 
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
write_clean_data(data = fishglob, survey = "FISHGLOB_v1.7_std",
                 overwrite = T)

# write.csv(fishglob, file = "E:/fishglob data/Compiled_data/public_private/FISHGLOB_v1.4.csv")

# Write public data file
fishglob_public <- fishglob %>% 
  filter(!survey %in% c("CHL",
                        "COL",
                        "DFO-NF",
                        "FALK",
                        "GIN",
                        "GRL-DE",
                        "ICE-GFS",
                        "IS-TAU",
                        "IS-MOAG",
                        "MEDITS",
                        "MRT",
                        "NAM",
                        "NZ-CHAT",
                        "NZ-WCSI",
                        "NZ-ECSI",
                        "NZ-SUBA",
                        "S-GEORG",
                        "WBLS",
                        "ZAF"))

write_clean_data(data = fishglob_public, survey = "FISHGLOB_public_v1.7_std",
                 overwrite = T)


# #-------------------------------------------------------------------------------------------#
# #### ADD TRAITS ####
# #-------------------------------------------------------------------------------------------#
# 
# load("~/FISHGLOB/fishglob_trait/data/output_JT/Fishbase_and_Morphometrics.RData")
# trait_pred <- data.frame(Fishbase_and_Morphometrics$beta_gv)
# trait_pred$accepted_name_fb <- row.names(trait_pred)
# row.names(trait_pred) <- NULL
# trait_pred <- trait_pred %>% 
#   mutate(spawning_type = ifelse(base==max(base,`spawning_typeguarders`,`spawning_typebearers`),
#                           "nonguarders",NA_character_),
#          spawning_type = ifelse(`spawning_typeguarders`==max(base,`spawning_typeguarders`,`spawning_typebearers`),
#                                 "guarders",spawning_type),
#          spawning_type = ifelse(`spawning_typebearers`==max(base,`spawning_typeguarders`,`spawning_typebearers`),
#                                 "bearers",spawning_type),
#          habitat = ifelse(`base.1`==max(`base.1`,`habitatbathymetric`,`habitatbenthopelagic`,
#                                         `habitatreef.associated`,`habitatpelagic`),
#                           "demersal",NA_character_),
#          habitat = ifelse(`base.1`==max(`base.1`,`habitatbathymetric`,`habitatbenthopelagic`,
#                                         `habitatreef.associated`,`habitatpelagic`),
#                           "bathydemersal",habitat),
#          habitat = ifelse(`habitatbenthopelagic`==max(`base.1`,`habitatbathymetric`,`habitatbenthopelagic`,
#                                         `habitatreef.associated`,`habitatpelagic`),
#                           "benthopelagic",habitat),
#          habitat = ifelse(`habitatpelagic`==max(`base.1`,`habitatbathymetric`,`habitatbenthopelagic`,
#                                         `habitatreef.associated`,`habitatpelagic`),
#                           "pelagic",habitat),
#          habitat = ifelse(`habitatreef.associated`==max(`base.1`,`habitatbathymetric`,`habitatbenthopelagic`,
#                                                 `habitatreef.associated`,`habitatpelagic`),
#                           "reef_associated",habitat),
#          feeding_mode = ifelse(`base.2`==max(`base.2`,`feeding_modemacrofauna`,`feeding_modeplanktivorous_or_other`),
#                                "generalist",NA_character_),
#          feeding_mode = ifelse(`feeding_modemacrofauna`==max(`base.2`,`feeding_modemacrofauna`,`feeding_modeplanktivorous_or_other`),
#                                "macrofauna",feeding_mode),
#          feeding_mode = ifelse(`feeding_modeplanktivorous_or_other`==max(`base.2`,`feeding_modemacrofauna`,`feeding_modeplanktivorous_or_other`),
#                                "planktivorous_or_other",feeding_mode),
#          body_shape = ifelse(`base.3`==max(`base.3`,`body_shapeelongated`,`body_shapeshort.and...or.deep`,
#                                          `body_shapeeel.like`,`body_shapeother`),
#                               "fusiform/normal",NA_character_),
#          body_shape = ifelse(`body_shapeelongated`==max(`base.3`,`body_shapeelongated`,`body_shapeshort.and...or.deep`,
#                                         `body_shapeeel.like`,`body_shapeother`),
#                              "elongated",body_shape),
#          body_shape = ifelse(`body_shapeshort.and...or.deep`==max(`base.3`,`body_shapeelongated`,`body_shapeshort.and...or.deep`,
#                                         `body_shapeeel.like`,`body_shapeother`),
#                              "short_and_or_deep",body_shape),
#          body_shape = ifelse(`body_shapeeel.like`==max(`base.3`,`body_shapeelongated`,`body_shapeshort.and...or.deep`,
#                                         `body_shapeeel.like`,`body_shapeother`),
#                              "eel_like",body_shape),
#          body_shape = ifelse(`body_shapeother`==max(`base.3`,`body_shapeelongated`,`body_shapeshort.and...or.deep`,
#                                         `body_shapeeel.like`,`body_shapeother`),
#                              "other",body_shape)
#          )
# 
# names_trait <- data.frame(sort(unique(trait_pred$accepted_name_fb))) # 30,017 names
# names(names_trait) <- "names_trait"
# names_fishglob <- data.frame(sort(unique(fishglob$accepted_name))) # 3,179 names
# names_fishglob <- fishglob %>% 
#   select(kingdom, phylum, class, order, family, genus, rank, accepted_name, aphia_id) %>% 
#   distinct()
# 
# ## for names absent from the trait database, find closest name to match traits
# missing_names <- anti_join(names_fishglob, names_trait, by = c("accepted_name" = "names_trait")) %>% 
#   filter(!is.na(accepted_name))
# missing_names$infer_from <- missing_names$accepted_name
# missing_names$infer_rank <- missing_names$rank
# ranks <- c("Subspecies","Species","Genus","Subfamily","Family","Suborder","Order","Infraclass","Class")
# for(i in 1:nrow(missing_names)){
#   while(!(missing_names$infer_from[i] %in% names_trait$names_trait) & missing_names$infer_rank[i]!="Class"){
#     missing_names$infer_rank[i] <- ranks[which(ranks==missing_names$infer_rank[i])+1]
#     if(missing_names$infer_rank[i]=="Species"){
#       spp <- str_extract_all(missing_names$infer_from[i], 
#                              boundary("word"), simplify = TRUE)[1,1:2]
#       missing_names$infer_from[i] <- paste(spp[1],spp[2], sep= " ")
#       rm(spp)
#     } else if(missing_names$infer_rank[i]=="Genus"){
#       missing_names$infer_from[i] <- missing_names$genus[i]
#     } else if(missing_names$infer_rank[i] %in% c("Family","Subfamily")){
#       missing_names$infer_from[i] <- missing_names$family[i]
#     } else if(missing_names$infer_rank[i] %in% c("Suborder","Order")){
#       missing_names$infer_from[i] <- missing_names$order[i]
#     } else if(missing_names$infer_rank[i] %in% c("Infraclass","Class")){
#       missing_names$infer_from[i] <- missing_names$class[i]
#     }
#   }
# }
# missing_names <- missing_names %>% 
#   mutate(infer_rank = ifelse(infer_rank %in% c("Suborder","Order"),"Order",infer_rank),
#          infer_rank = ifelse(infer_rank %in% c("Family","Subfamily"),"Family",infer_rank),
#          infer_rank = ifelse(infer_rank %in% c("Infraclass","Class"),"Class",infer_rank)) %>% 
#   select(accepted_name, infer_from, infer_rank)
# fishglob_trait <- left_join(fishglob, missing_names, by = "accepted_name") %>% 
#   filter(!is.na(accepted_name)) %>% 
#   mutate(infer_from = ifelse(is.na(infer_from), accepted_name, infer_from),
#          infer_rank = ifelse(is.na(infer_rank), rank, infer_rank),
#          infer_from = ifelse(infer_from == "Actinopteri","Actinopterygii",infer_from))
# 
# ## add traits
# fishglob_trait <- left_join(fishglob_trait, trait_pred, by = c("infer_from" = "accepted_name_fb")) %>% 
#   select(-`base`,-`base.1`,-`base.2`,-`base.3`,
#          -`spawning_typeguarders`,-`spawning_typebearers`,
#          -`habitatbathymetric`,-`habitatbenthopelagic`,-`habitatreef.associated`,
#          -`habitatpelagic`,-`feeding_modemacrofauna`,-`feeding_modeplanktivorous_or_other`,
#          -`body_shapeelongated`,-`body_shapeshort.and...or.deep`,-`body_shapeeel.like`,
#          -`body_shapeother`)
# 


#-------------------------------------------------------------------------------------------#
#### METADATA PRODUCT ####
#-------------------------------------------------------------------------------------------#
# 
# fishglob_public_metadata <- fishglob_public %>% 
#   select(survey, latitude, longitude, year, survey_unit) %>% 
#   distinct()
#   
# ggplot(fishglob_public_metadata) +
#   geom_point(aes(x = longitude, y = latitude))
# 
# write_clean_data(data = fishglob_public_metadata, 
#                  survey = "FISHGLOB_public_metadata_v1.2",
#                  overwrite = T)