#' Read and clean FISHGLOB data
#'
#' @return
#' @export
#'
read_clean_data <- function(surveys, std=FALSE){
  
  for(f in 1:length(surveys)){
    if(std == TRUE){
      load(paste0("outputs/Cleaned_data/",surveys[f],"_std_clean.RData"))
      columns <- as.data.frame(read_excel(here("standard_formats/fishglob_data_columns_std.xlsx")))[,1]
    } else {
      load(paste0("outputs/Cleaned_data/",surveys[f],"_clean.RData"))
      columns <- as.data.frame(read_excel(here("standard_formats/fishglob_data_columns.xlsx")))[,1]
    }
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
  
  return(fishglob)
}
