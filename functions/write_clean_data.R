
# -------------------------------------------------------------- #
# NOTE (JEPA): TYPE MUST ALLWAYS BE CHARACTERS ""
# -------------------------------------------------------------- #
################################################################################
####Update
#### Juliano Palacios
#### August, 2025
####Following issue 66, included a chunk of code that installs/loads a package 
# that is missing. It now requires an embeded function `check_pkg.R`
################################################################################


# Makes sure all packages are installed
source(here::here("functions/check_pkg.R"))
check_pkg(
  c("readxl","here","tidyverse","readr","data.table","googledrive")
)

write_clean_data <- function(data, survey, overwrite = NA, remove =  T, type = "NA",
                             csv = FALSE, ggdrive = FALSE, rdata = TRUE,
                             compiled = FALSE, gzip = FALSE){

  # Warning message for character
  if(is.character(type) == FALSE){
    
    return(cat(paste0("Stopping function. Type must be a character, not a factor")))
  }
  

  if(rdata == TRUE & compiled == FALSE){
    readme <- as.data.frame(read_excel(here("standard_formats/fishglob_data_columns_std.xlsx")))
    data <- data %>%
      mutate(year = as.integer(year),
             #month = as.integer(month),
             #day = as.integer(day)
             )
    save(data, readme, file = paste0("outputs/Cleaned_data/",survey,"_clean.RData"))
  }
  
  
  if(rdata == TRUE & compiled == TRUE){
    readme <- as.data.frame(read_excel(here("standard_formats/fishglob_data_columns_std.xlsx")))
    data <- data %>% 
      mutate(year = as.integer(year),
             #month = as.integer(month),
             #day = as.integer(day)
             )
    save(data, readme, file = paste0("outputs/Compiled_data/",survey,"_clean.RData"))
  }
  
  if(csv == TRUE){
    save_path_name <- paste0("outputs/Cleaned_data/",survey,"_clean.csv")
    save_name <- paste0("outputs/Clean_data/",survey,"_clean.csv")
    
    # Writes data in local computer
    write_csv(data,save_path_name)
    
  }
  
  if(gzip == TRUE & compiled == TRUE){
    name <- paste0("outputs/Compiled_data/",survey,"_clean.csv")
    destname <- paste0("outputs/Compiled_data/",survey,"_clean.csv.gz")
    fwrite(data, file = name)
    gzip(name, destname = destname)
  }
  
  if(ggdrive == TRUE){
    # p <- drive_get("Compiled_data")
    # Gets path in google drive
    if(type == "fishglob"){
      p <- drive_get("Compiled_data")
    } else {
      p <- drive_get("Cleaned_data")
    }

  # -------------------------------------------------------------- #
  # NOTE. JEPA YOU CAN REMOVE EVERYHTING (the if else) THAT IS COMMENTED OUT AFTER HERE
  # Malin commented out the drive_upload() 8 December 2025 since we are not uploading to google now
  # -------------------------------------------------------------- #
  # if(overwrite == T){
    # Uploads data to google drive and overwrites previous files if they exist with same name
    #drive_upload(save_path_name,
    #             path=as_id(p),
    #             name = save_name,
    #             overwrite = overwrite)
  # }else{
    # Uploads data to google drive and adds additional file with same name
    # drive_upload(save_path_name,
    #              path=as_id(p),
    #              name = save_name,
    #              overwrite = F)
    # }
  
    if(remove == T){
      
      file.remove(save_path_name)
    }
  }
  
}

# ------------------------ #
# For testing the function 
# ------------------------ #
# library(readr)
# library(googledrive)

# write_clean_data(data,survey,overwrite = NA, remove = F, type = "NA")
# ------------------------ #
