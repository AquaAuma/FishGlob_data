################################################################################
#### R code to merge all separate datasets
#### Coding: Juliano Palacios Abrantes & Aurore A. Maureaud, December 2022
################################################################################

rm(list=ls())

library(readxl) # To load extra datasets
library(tidyverse) # for data wrangling
library(janitor) # for cleaning names
library(lubridate) # for fixing dates
library(rnaturalearth) # for removing points from land
library(sf) # for removing points from land
library(sp) # for removing points from land
library(here) # for easy work around on multiple computers
library(taxize) # for getting correct speceies names
library(magrittr) # for names wrangling
library(googledrive) # for interaction with the drive
library(knitr) # for nice tables
library(kableExtra)
library(plotly) # for interactiv plots
library(leaflet) 


### Survey data
# x <- drive_find(pattern = "FISHGLOB_v1.7_clean.csv")
# 
# drive_download(file = x[1,],
#                overwrite = TRUE)

load("outputs/Compiled_data/FishGlob_public_clean.RData")
survey <- data
rm(data)

### Map data
World_map <- rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf")) %>%
  clean_names() %>% 
  select(iso_a3,sovereignt,sov_a3,name,name_long,region_wb,continent,geometry) %>% 
  # st_transform(crs = "+proj=eck4") %>%
  st_transform(crs = 4326) %>% # 4326
  mutate(iso_a3 = ifelse(sovereignt == "Norway","NOR",
                         ifelse(name == "France","FRA",
                                iso_a3)
  )
  )

png(filename = "summary/fishglob_summary/map_cleaned_data.png",
    width = 12*200, height = 5*200, res = 200)
print(survey %>% 
  select(longitude,latitude, survey) %>% 
  distinct() %>% 
  filter(!is.na(latitude),
         !is.na(longitude)) %>% 
  ggplot() +
  geom_point(aes(x = longitude,y = latitude,color = survey),alpha = 0.5) +
  geom_sf(data = World_map, aes()) +
  theme_bw() +
  theme(legend.position = "none"))
dev.off()

## Summary of hauls
png(filename = "summary/fishglob_summary/hauls_time.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>% 
  group_by(year,haul_id, survey) %>% 
  summarise(n =  n()) %>% 
  group_by(year, survey) %>% 
  summarise(n_hauls = n()) %>% 
  ggplot()  +
  geom_line(aes(x = year,y = n_hauls, group = survey, color = survey)) +
  geom_point(aes(x = year,y = n_hauls, group = survey, color = survey)) +
  ylab("Number of hauls") + xlab("Year") + theme_bw() +
  facet_wrap(~ survey, scales = "free_y") + theme(legend.position = "none"))
dev.off()

### Summary of number of taxa
png(filename = "summary/fishglob_summary/nbr_taxa.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
  group_by(survey, year, haul_id) %>%
  summarize(nbr_taxa = length(accepted_name)) %>%  
  ggplot() +
  geom_boxplot(aes(x = as.factor(year),y = nbr_taxa,color = survey),outlier.shape = NA,size=0.5)  +
  ylab("")  + xlab("Year") +
  facet_wrap(~survey, scales = "free_y") +
  theme_bw()+ theme(legend.position = "none"))
dev.off()

### Summary of haul duration
png(filename = "summary/fishglob_summary/haul_duration.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        select(survey, year, haul_id, haul_dur) %>%
        distinct() %>%  
        ggplot() +
        geom_boxplot(aes(x = as.factor(year),y = haul_dur,color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()

### Summary of swept area
png(filename = "summary/fishglob_summary/area_swept.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        select(survey, year, haul_id, area_swept) %>%
        distinct() %>%  
        ggplot() +
        geom_boxplot(aes(x = as.factor(year),y = area_swept, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Summary of survey depth
survey$depth <- as.numeric(as.vector(survey$depth))
png(filename = "summary/fishglob_summary/depth.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        select(survey, year, haul_id, depth) %>%
        distinct() %>%  
        filter(!is.na(depth)) %>% 
        ggplot() +
        geom_boxplot(aes(x = as.factor(year),y = depth, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Summary of latitude
png(filename = "summary/fishglob_summary/latitude.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        select(survey, year, haul_id, latitude) %>%
        distinct() %>%  
        ggplot() +
        geom_boxplot(aes(x = as.factor(year),y = latitude, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Summary of longitude
png(filename = "summary/fishglob_summary/longitude.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        select(survey, year, haul_id, longitude) %>%
        distinct() %>%  
        ggplot() +
        geom_boxplot(aes(x = as.factor(year),y = longitude, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Number of hauls per month
png(filename = "summary/fishglob_summary/hauls_per_month.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        group_by(survey, year, haul_id, month) %>%
        summarize(hauls = length(haul_id)) %>%
        filter(!is.na(month)) %>% 
        ggplot() +
        geom_boxplot(aes(x = as.factor(month),y = hauls, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Number of spp per month
png(filename = "summary/fishglob_summary/taxa_per_month.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        group_by(survey, year, month) %>%
        summarize(taxa = length(unique(accepted_name))) %>%
        filter(!is.na(month)) %>%
        ggplot() +
        geom_boxplot(aes(x = as.factor(month),y = taxa, color = survey),outlier.shape = NA,size=0.5)  +
        ylab("")  + xlab("Year") +
        facet_wrap(~survey, scales = "free_y") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Number of taxa against depth
png(filename = "summary/fishglob_summary/taxa_per_depth.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        group_by(survey, year, haul_id, depth) %>%
        summarize(taxa = length(accepted_name)) %>%
        filter(!is.na(depth)) %>%
        ggplot(aes(x = depth,y = taxa, color = survey)) +
        geom_point()  +
        ylab("")  + xlab("Depth") +
        stat_smooth(method = "loess", formula = y ~ x) +
        facet_wrap(~survey, scales = "free") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Number of taxa against swept area
png(filename = "summary/fishglob_summary/taxa_per_areaswept.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        group_by(survey, year, haul_id, area_swept) %>%
        summarize(taxa = length(accepted_name)) %>%
        filter(!is.na(area_swept)) %>%
        ggplot(aes(x = area_swept,y = taxa, color = survey)) +
        geom_point(size=0.5)  +
        ylab("")  + xlab("Swept Area") +
        stat_smooth(method = "loess", formula = y ~ x) +
        facet_wrap(~survey, scales = "free") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()


### Num against depth
png(filename = "summary/fishglob_summary/num_per_depth.png",
    width = 12*200, height = 10*200, res = 200)
print(survey %>%
        group_by(survey, year, haul_id, depth) %>%
        summarize(num_cpue = sum(num_cpue, na.rm=T)) %>%
        filter(!is.na(depth)) %>%
        ggplot(aes(x = depth,y = num_cpue, color = survey)) +
        geom_point()  +
        ylab("")  + xlab("Depth") +
        stat_smooth(method = "loess", formula = y ~ x) +
        facet_wrap(~survey, scales = "free") +
        theme_bw()+ theme(legend.position = "none"))
dev.off()