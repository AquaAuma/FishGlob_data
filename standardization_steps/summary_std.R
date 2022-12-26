################################################################################
#### R code to summarize the std process across surveys
#### Coding: A. Maureaud, December 2022
################################################################################

#########   Preliminary steps

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

#load all functions
source(here::here("standardization_steps", "functions.R"))

# read survey data
load("outputs/Compiled_data/FishGlob_public_v1.7_clean.RData")
dat <- data
rm(data)

dat <- dat %>% 
  dplyr::select(survey, survey_unit) %>% 
  dplyr::distinct()

dat_survey <- dat %>% 
  dplyr::select(survey) %>% 
  dplyr::distinct()



std_taxa_all <- data.frame()
for(i in 1:nrow(dat_survey)){
  if(!dat_survey$survey[i] %in% c("SWC-IBTS","FALK","GSL-N","MRT","NZ-CHAT","SCS")){
    df <- read.csv(here::here("standardization_steps", "outputs", "taxonomic_flagging", 
                              paste0(dat_survey$survey[i],'_stats.csv')))
    df$survey <- dat_survey$survey[i]
    df$percentage[1] <- df$nb[2]
    df$number[1] <- df$nb[1]
    df <- df[1,3:5]
    std_taxa_all <- rbind(std_taxa_all, df)
    rm(df)
  } else {
    df <- c(dat_survey$survey[i], 0, NA)
    std_taxa_all <- rbind(std_taxa_all, df)
    rm(df)
  }
}

std_taxa_all$percentage <- as.vector(as.numeric(std_taxa_all$percentage))
std_taxa_all$number <- as.vector(as.numeric(std_taxa_all$number))

# plot of the summary per survey
library(ggplot2)
png("standardization_steps/outputs/taxa_per_survey_number_public.png", width = 8*500, height = 5*500, res=500)
std_taxa_all %>% 
  ggplot() + geom_point(aes(x = survey, y = number)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
                   axis.text.y = element_text(size=12),
                   axis.title = element_text(size=16))
dev.off()

# plot of the summary per survey
png("standardization_steps/outputs/taxa_per_survey_percentage_public.png", width = 8*500, height = 5*500, res=500)
std_taxa_all %>% 
  ggplot() + geom_point(aes(x = survey, y = percentage)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
       axis.text.y = element_text(size=12),
       axis.title = element_text(size=16)) +
  ylab("% of flagged taxa") +
  xlab("Survey")
dev.off()

# plot of the summary per survey
png("standardization_steps/outputs/number_versus_percentage_public.png", width = 5*500, height = 5*500, res=500)
std_taxa_all %>% 
  ggplot() + geom_point(aes(x = number, y = percentage)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()



std_dat <- data.frame()
for(i in 1:nrow(dat)){
  met1_7 <- read.csv(here::here("standardization_steps", "outputs", "trimming_method1", 
                                "hex_res7", paste0(dat$survey_unit[i],"_hex_res_7_stats_hauls.csv")),
                     sep = ",")
  met1_8 <- read.csv(here::here("standardization_steps", "outputs", "trimming_method1",
                                "hex_res8", paste0(dat$survey_unit[i],"_hex_res_8_stats_hauls.csv")),
                     sep = ",")
  
  if(!dat$survey_unit[i] %in% c("WBLS","DFO-SOG","SCS-FALL","IS-TAU")){
    met2 <- read.csv(here::here("standardization_steps", "outputs", "trimming_method2", 
                                paste0(dat$survey_unit[i],"_stats_hauls.csv")))
    met <- cbind(met1_7, met1_8[,2:3], met2[,2])
    names(met) <- c("percentage","hex7_0","hex7_2","hex8_0","hex8_2","biotime")
    met <- met[2,2:ncol(met)]
    met$survey_unit <- dat$survey_unit[i]
    std_dat <- rbind(std_dat, met)
    rm(met, met1_7, met1_8, met2)
  } else {
    met <- cbind(met1_7, met1_8[,2:3], NA)
    names(met) <- c("percentage","hex7_0","hex7_2","hex8_0","hex8_2","biotime")
    met <- met[2,2:ncol(met)]
    met$survey_unit <- dat$survey_unit[i]
    std_dat <- rbind(std_dat, met)
    rm(met, met1_7, met1_8)
  }
  
}

# plot of the summary per survey
library(ggplot2)
png("standardization_steps/outputs/summary_per_survey_public.png", width = 8*500, height = 5*500, res=500)
std_dat %>% 
  tidyr::pivot_longer(cols = 1:5, names_to = "std_method", values_to = "percentage") %>% 
  dplyr::filter(!is.na(percentage)) %>% 
  ggplot() + geom_boxplot(aes(x = survey_unit, y = percentage)) +
  theme_bw() + ylab("% flagged hauls") + xlab("Survey unit")+
  theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=16))
dev.off()

png("standardization_steps/outputs/summary_per_method_public.png", width = 8*500, height = 5*500, res=500)
std_dat %>% 
  tidyr::pivot_longer(cols = 1:5, names_to = "std_method", values_to = "percentage") %>% 
  dplyr::filter(!is.na(percentage)) %>% 
  ggplot() + geom_boxplot(aes(x = std_method, y = percentage)) +
  theme_bw() + ylab("% of flagged hauls") + xlab("Standardization method") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=16))
dev.off()


