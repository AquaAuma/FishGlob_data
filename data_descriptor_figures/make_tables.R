#######################################################
#### Coding: Aurore Maureaud, November 2023
#### Public survey metadata
#### Figures for data paper
#######################################################
rm(list = ls())

# load libraries
library(tidyverse)

load("outputs/Compiled_data/FishGlob_public_std_clean.RData")

table_1 <- data %>% 
  group_by(survey) %>% 
  mutate(min_year = min(year),
         max_year = max(year),
         number_of_years = length(unique(year)),
         month = as.numeric(month),
         months = paste(sort(unique(month)), collapse = ", "),
         number_of_hauls = length(unique(haul_id)),
         number_of_taxa = length(unique(accepted_name))) %>% 
  dplyr::select(survey, min_year, max_year, number_of_years,
                months, number_of_hauls, number_of_taxa) %>% 
  distinct()

write.csv(table_1, file = "data_descriptor_figures/table_1.csv", 
          row.names = F)
