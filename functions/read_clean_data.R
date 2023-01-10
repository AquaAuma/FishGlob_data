#' Read and clean FISHGLOB data
#'
#' @return
#' @export
#'
read_clean_data <- function(survey){
  
  load(paste0("outputs/Cleaned_data/",survey,"_clean.RData"))
  dat <- data
  
  dat %>%
    tidyr::drop_na(longitude) %>%
    tidyr::drop_na(latitude) -> dat
  
  return(dat)
}
