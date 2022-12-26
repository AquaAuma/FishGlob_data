
# -------------------------------------------------------------- #
# NOTE (JEPA): TYPE MUST ALLWAYS BE CHARACTERS ""
# -------------------------------------------------------------- #

write_clean_data <- function(data, survey, overwrite = NA, remove =  T, type = "NA",
                             csv = FALSE, ggdrive = FALSE, rdata = TRUE){
  
  if(rdata == TRUE){
    save(data, file = paste0("outputs/Cleaned_data/",survey,"_clean.RData"))
  }
  
  if(csv == TRUE){
    save_path_name <- paste0(survey,"_clean.csv")
    save_name <- paste0(survey,"_clean.csv")
    
    # Writes data in local computer
    write_csv(data,save_path_name)
    
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
  # -------------------------------------------------------------- #
  # if(overwrite == T){
    # Uploads data to google drive and overwrites previous files if they exist with same name
    drive_upload(save_path_name,
                 path=as_id(p),
                 name = save_name,
                 overwrite = overwrite)
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
