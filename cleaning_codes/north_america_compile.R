#Pull in all and merge

library(googledrive)
library(here)

drive_download(file = "GOA_clean.csv",
               overwrite = TRUE)
drive_download(file = "EBS_clean.csv",
               overwrite = TRUE)
drive_download(file = "AI_clean.csv",
               overwrite = TRUE)
drive_download(file = "GMEX_clean.csv",
               overwrite = TRUE)
drive_download(file = "SEUS_clean.csv",
               overwrite = TRUE)
drive_download(file = "WCTRI_clean.csv",
               overwrite = TRUE)
drive_download(file = "WCANN_clean.csv",
               overwrite = TRUE)
drive_download(file = "NEUS_clean.csv",
               overwrite = TRUE)
drive_download(file = "SCS_clean.csv",
               overwrite = TRUE)
drive_download(file = "GSL-S_clean.csv",
               overwrite = TRUE)
drive_download(file = "GSL-N_clean.csv",
               overwrite = TRUE)
drive_download(file = "SOG_clean.csv",
               overwrite = TRUE)
drive_download(file = "QCS_clean.csv",
               overwrite = TRUE)
drive_download(file = "WCHG_clean.csv",
               overwrite = TRUE)
drive_download(file = "HS_clean.csv",
               overwrite = TRUE)
drive_download(file = "WCVI_clean.csv",
               overwrite = TRUE)


survey1 <- read.csv(here("GOA_clean.csv"))
survey2 <- read.csv(here("EBS_clean.csv"))
survey3 <- read.csv(here("AI_clean.csv"))
survey4 <- read.csv(here("GMEX_clean.csv"))
survey5 <- read.csv(here("SEUS_clean.csv"))
survey6 <- read.csv(here("WCTRI_clean.csv"))
survey7 <- read.csv(here("WCANN_clean.csv"))
survey8 <- read.csv(here("NEUS_clean.csv"))
survey9 <- read.csv(here("SCS_clean.csv"))
survey10 <- read.csv(here("GSL-N_clean.csv"))
survey11 <- read.csv(here("GSL-S_clean.csv"))
survey12 <- read.csv(here("SOG_clean.csv"))
survey13 <- read.csv(here("QCS_clean.csv"))
survey14 <- read.csv(here("WCHG_clean.csv"))
survey15 <- read.csv(here("HS_clean.csv"))
survey16 <- read.csv(here("WCVI_clean.csv"))

file.remove(
here("GOA_clean.csv"),
here("EBS_clean.csv"),
here("AI_clean.csv"),
here("GMEX_clean.csv"),
here("SEUS_clean.csv"),
here("WCTRI_clean.csv"),
here("WCANN_clean.csv"),
here("NEUS_clean.csv"),
here("SCS_clean.csv"),
here("GSL-N_clean.csv"),
here("GSL-S-clean.csv"),
here("SOG_clean.csv"),
here("QCS_clean.csv"),
here("WCHG_clean.csv"),
here("HS_clean.csv"),
here("WCVI_clean.csv"))

allsurveys_name <- ls(pattern = "^survey*")

allsurveys.dt <- do.call(rbind, mget(allsurveys_name))

length(unique(allsurveys.dt$accepted_name))
