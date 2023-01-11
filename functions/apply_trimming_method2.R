
#' adapted from BioTIME code sent by Jon Chase's lab
#' filter database to choose subsets of datasets with number of locations (ie hauls) in each year >=4 and duration >=10 years,
#' and matching locations across years to keep locations in similar configuration across years
#' outputs per survey_unit:
#' - a map of hauls retained and removed (png file);
#' - statistics of hauls retained / removed (csv file);
#' - a list of haul ids removed  (csv file);
#' - the global raw dataset with added columns indicating if the haul was retained / removed (dataframe)
#'
#'
#' @param data
#'
#' @return
#' @export
#'

apply_trimming_per_survey_unit_method2 <- function(data){
  
  bt_loc <- data %>%
    dplyr::select(survey_unit, year, latitude, longitude) %>%
    dplyr::distinct()
  
  # remove years with number of locations < 4
  bt_loc <-  bt_loc %>%
    dplyr::group_by(survey_unit, year) %>%
    dplyr::mutate(n_loc = dplyr::n_distinct(latitude, longitude)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_loc >= 4) %>%
    dplyr::select(-n_loc)
  
  # calculate number of locations, number of years and duration
  # and remove studies with number of years < 2 and duration < 10 years
  meta_year <- bt_loc %>%
    dplyr::group_by(survey_unit, year) %>%
    dplyr::summarise(n_loc = dplyr::n_distinct(latitude, longitude, na.rm = TRUE)) %>%
    dplyr::group_by(survey_unit) %>%
    dplyr::mutate(t_loc = sum(n_loc),
                  mean_loc = round(mean(n_loc),1),
                  min_loc = min(n_loc),
                  max_loc = max(n_loc),
                  n_years = dplyr::n_distinct(year, na.rm = TRUE),
                  duration = max(year) - min(year) +1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_years >=2 & duration >=10)
  
  meta <- meta_year %>%
    dplyr::select(-c(year, n_loc)) %>%
    dplyr::distinct()
  
  # remove studies with number of years < 2 and duration <10 years
  bt_4loc_10yr <- bt_loc %>%
    tidyr::unite(col = location, latitude, longitude, remove = FALSE) %>%
    tidyr::unite(col = survey_year, survey_unit, year, remove = FALSE) %>%
    dplyr::filter(survey_year %in% (meta_year %>% tidyr::unite(col = survey_year, survey_unit, year) %>%
                                      dplyr::pull(survey_year))) %>%
    dplyr::select(-survey_year)
  
  
  
  ############
  ## the number and locations of sites are somewhat different across years
  # match sites across years based on grid-cells to keep similar configurations of sites
  
  # Calculate minimum, maximum latitude and longitude, and spans of longitude and longitude
  meta_spat <- bt_4loc_10yr %>%
    dplyr::group_by(survey_unit) %>%
    dplyr::mutate(min_latitude =  min(latitude),
                  max_latitude =  max(latitude),
                  min_longitude =  min(longitude),
                  max_longitude =  max(longitude)) %>%
    # If locations cross 180 degree, update the minimum longitude to calculate spans of longitude
    dplyr::mutate(longitude = ifelse((max_longitude - min_longitude) >180 & longitude < 0, longitude + 360, longitude)) %>%
    dplyr::mutate(span_latitude = max(latitude) - min(latitude),
                  span_longitude = max(longitude) - min(longitude)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(survey_unit, min_latitude, max_latitude, min_longitude, max_longitude, span_latitude, span_longitude)
  
  
  # add cells for each survey. cells are only comparable within studies
  bt_4loc_10yr <- bt_4loc_10yr %>%
    dplyr::mutate(cell = NA)
  
  for(i in 1:nrow(meta_spat)){
    survey_spat <- meta_spat[i, c("survey_unit", "min_longitude", "max_longitude", "min_latitude", "max_latitude", "span_longitude", "span_latitude")] %>%
      as.data.frame()
    # set the resolution as the mean 1/5 spans of longitude and longitude
    res <- mean(c(survey_spat[, 6]/5, survey_spat[, 7]/5))
    ras <- raster::raster(xmn = survey_spat[1, 2] - res, xmx = survey_spat[1, 3] + res,
                          ymn = survey_spat[1, 4] - res, ymx = survey_spat[1, 5] + res,
                          resolution = res)
    raster::values(ras) <- 1:length(ras)
    # raster::plot(ras)
    # points(bt_4loc_10yr %>%
    #          dplyr::filter(survey_unit == survey_spat[1, 1]) %>%
    #          dplyr::select(longitude, latitude))
    id <- bt_4loc_10yr$survey_unit == survey_spat[1, 1]
    #Get cell numbers of raster from dtafrane columns
    bt_4loc_10yr$cell[id] <- raster::cellFromXY(ras, bt_4loc_10yr[id, c("longitude", "latitude")] %>%
                                                  as.data.frame())
  }
  
  # filter the dataset to remove rare cells and years with small extent
  bt_4loc_10yr_filtered <- NULL
  bt_years_max_loc <- NULL
  
  for(i in 1:nrow(meta_spat)){
    # perform loop for each survey
    survey <- bt_4loc_10yr %>%
      dplyr::filter(survey_unit == meta_spat$survey_unit[i]) # %>% distinct(year, sample)
    
    # calculate number of locations in each cell of each year: rows are years, columns are cells, elements are number of locations
    year_cell <- as.matrix(xtabs( ~ year + cell, data = survey[, c("year","cell")], sparse=TRUE))
    
    # keep cells with locations in all years or cells with density of locations grater than 50% of the mean value
    cell_loc <- data.frame("cell" = colnames(year_cell),
                           "p_years" = colMeans(year_cell>0),
                           "mean_loc" = colMeans(year_cell)) %>%
      dplyr::mutate(p_loc = round(mean_loc/sum(mean_loc),3)) #relative density of locations
    
    # keep cells with locations in all years or with density of locations > the half of the mean
    # if the remaining cells have >= 4 locations
    id_cell <- with(cell_loc, p_years==1 | p_loc > 0.5*1/nrow(cell_loc))
    if(max(rowSums(year_cell[, id_cell, drop=FALSE]))>=4){
      year_cell <-  year_cell[, id_cell, drop=FALSE]
    }
    
    # number of co-occurred (in the same cells) locations between years
    co_loc <- as.matrix(vegan::designdist(year_cell, method = "J", terms= "minimum"))
    
    # find which two years (year-pair) have the maximum number of co-occurred locations and duration >=10 years
    # these two years have priority to be kept, and other years will be compared to the two years
    max_co_loc <- reshape2::melt(co_loc) %>%
      dplyr::as_tibble() %>%
      purrr::set_names("year1","year2","n_loc") %>%
      dplyr::mutate(year1 = as.numeric(year1),
                    year2 = as.numeric(year2),
                    duration = year2 - year1 + 1) %>%
      dplyr::filter(duration > 9 & n_loc > 3)
    if(nrow(max_co_loc) == 0) {next}
    max_co_loc <-  dplyr::filter(max_co_loc, n_loc >= 0.9*max(n_loc))
    max_co_loc <- dplyr::filter(max_co_loc, duration == max(duration))
    max_co_loc <-  dplyr::filter(max_co_loc, n_loc == max(n_loc))
    
    # keep the cells that have locations in the both determined years
    cell_year1 <- year_cell[rownames(year_cell) %in% unlist(max_co_loc[1,1]),]
    cell_year2 <- year_cell[rownames(year_cell) %in% unlist(max_co_loc[1,2]),]
    cell_shared <- cell_year1 > 0 & cell_year2 > 0
    year_cell <- year_cell[,cell_shared, drop=FALSE]
    
    # other years except the two priority years will be compared to the two years
    # weight cells based on mean number of locations and calculate the sums of weights of cells with locations for each year
    weight <- colMeans(year_cell)/sum(colMeans(year_cell))
    year_cell_binomial <- (year_cell > 0)*1
    cum_weight_year <- year_cell_binomial %*% weight
    
    # remove years with the sum weight less than 90%: the years with small extent
    id_year <- cum_weight_year > 0.9
    year_cell <-  year_cell[id_year, , drop=FALSE]
    
    # remove years with less than 50% of mean number of locations, but keep the duration >= 10 years
    id_year1 <- rowSums(year_cell) >= 0.5*mean(rowSums(year_cell))
    year_keep <- as.numeric(rownames(year_cell))[id_year1]
    if((max(year_keep) - min(year_keep) + 1) > 9){
      year_cell <-  year_cell[id_year1, , drop=FALSE]
    }
    
    # keep the locations if in the chosen cells and years
    rare_survey <- survey %>%
      dplyr::filter(cell %in% colnames(year_cell) & year %in% rownames(year_cell))
    
    bt_years_max_loc <- dplyr::bind_rows(bt_years_max_loc, dplyr::bind_cols(survey_unit = meta_spat$survey_unit[i], max_co_loc[1, ]))
    bt_4loc_10yr_filtered  <- dplyr::bind_rows(bt_4loc_10yr_filtered, rare_survey)
  }
  
  
  
  # remove years with < 4 locations and studies with number of years < 2 and duration < 10 years
  bt_4loc_10yr_filtered <- bt_4loc_10yr_filtered %>%
    dplyr::group_by(survey_unit, year) %>%
    dplyr::mutate(n_loc = dplyr::n_distinct(latitude, longitude)) %>%
    dplyr::filter(n_loc >= 4) %>%
    dplyr::group_by(survey_unit) %>%
    dplyr::mutate(n_years = dplyr::n_distinct(year, na.rm = TRUE),
                  duration = max(year) - min(year) + 1) %>%
    dplyr::filter(n_years >= 2 & duration >= 10) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(n_loc, n_years, duration))
  
  # add the column "is_retained_biotime" to distinguish the locations that should be kept or removed
  bt_4loc_10yr <- bt_4loc_10yr %>%
    dplyr::left_join(bt_4loc_10yr_filtered %>%
                       dplyr::mutate(is_retained_biotime = TRUE)) %>%
    dplyr::mutate(is_retained_biotime = ifelse(is.na(is_retained_biotime), FALSE, is_retained_biotime))
  
  
  ###### export outputs
  
  
  # full dataset with added column is_retained_biotime
  data_new <- data %>%
    dplyr::full_join(bt_4loc_10yr %>%
                       dplyr::select(-c(location, cell)), by = c("survey_unit", "year", "latitude", "longitude")) %>%
    dplyr::mutate(is_retained_biotime = ifelse(is.na(is_retained_biotime), FALSE, is_retained_biotime))
  
  
  # output per survey
  
  for(i in 1:nrow(meta_spat)){
    
    # get data per survey
    data_new_survey <- data_new %>%
      dplyr::filter(survey_unit == meta_spat$survey_unit[i])
    
    #write list of hauls codes/locations removed per survey
    haul_id_removed <- data_new_survey$haul_id[data_new_survey$is_retained_biotime == FALSE]
    readr::write_delim(as.data.frame(haul_id_removed),  file = here::here("outputs", "Flags", "trimming_method2", paste0(meta_spat$survey_unit[i],  "_hauls_removed.csv")), delim = ";")
    
    #write to csv file number and percentage of hauls/locations to remove per survey
    data_new_survey %>%
      dplyr::group_by(haul_id, is_retained_biotime) %>%
      dplyr::distinct() %>%
      dplyr::filter(!any(is_retained_biotime)) -> removed_hauls
    data_new_survey %>%
      dplyr::group_by(haul_id) %>%
      dplyr::distinct() -> all_hauls
    
    #make dataframe
    df <- data.frame("name" = c("number of hauls removed", "percentage of hauls removed"), 
                     "nb" = c(nrow(removed_hauls), round(100*nrow(removed_hauls) / nrow(all_hauls),1)))
    
    write.csv(df, row.names = FALSE, file = here::here("outputs", "Flags", "trimming_method2", paste0(meta_spat$survey_unit[i], "_stats_hauls.csv")))
    
    
    
  }
  
  
  
  for(i in 1:nrow(bt_years_max_loc)){
    survey <- bt_4loc_10yr %>%
      dplyr::filter(survey_unit %in% bt_years_max_loc$survey_unit[i])
    
    # correct longitudes to allow correct plotting if locations cross 180 degree
    survey <- survey %>%
      dplyr::mutate(max_longitude = max(longitude)) %>%
      dplyr::mutate(min_longitude = min(longitude)) %>%
      dplyr::mutate(longitude = ifelse((max_longitude - min_longitude) > 180 & longitude < 0, longitude + 360, longitude))
    
    #survey_title <- meta$survey_unit[i]
    survey_title <- paste("survey=", bt_years_max_loc[i,1],
                          "year1=", bt_years_max_loc[i,2],
                          "year2=", bt_years_max_loc[i,3],
                          "max.shared.samples=", bt_years_max_loc[i,4],
                          "duration=", bt_years_max_loc[i,5])
    
    p <- ggplot2::ggplot(data = survey, ggplot2::aes(longitude, latitude)) +
      ggplot2::facet_wrap(~year) +
      ggplot2::geom_point(ggplot2::aes(colour = is_retained_biotime), size = 1, alpha = 0.5) +
      ggplot2::labs(title = survey_title) +
      ggplot2::coord_fixed() +
      # ggplot2::scale_color_manual(values = c("TRUE" = "purple", "FALSE" = "grey10")) +
      ggplot2::scale_color_manual(values = c("red", "black"), labels = c("haul \nremoved", "haul \nretained"), name = "") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                     legend.position = "right",
                     legend.text = ggplot2::element_text(size = 8),
                     axis.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
                     axis.text.y = ggplot2::element_text(size = 8))
    
    ggplot2::ggsave(here::here("outputs", "Flags", "trimming_method2", paste0(bt_years_max_loc[i,1],"_map_per_haul.png")), p)
    
  }
  
  #return full dataset
  return(data_new)
  
  
}
