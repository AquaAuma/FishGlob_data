#' Read and clean FISHGLOB data
#'
#' @return
#' @export
#'
read_clean_data <- function(){
  
  load("outputs/Cleaned_data/FishGlob_v1.7_clean.RData")
  dat <- data
  
  dat %>%
    tidyr::drop_na(longitude) %>%
    tidyr::drop_na(latitude) -> dat
  
  return(dat)
}
