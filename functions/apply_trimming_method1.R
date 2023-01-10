#' code adapted from https://github.com/zoekitchel/trawl_spatial_turnover/blob/master/data_prep_code/species/explore_NorthSea_trimming.Rmd
#' trim haul dataset per survey_unit based on a discrete global grid of given resolution and 2 trimming options :
#' 1- trim hauls with any number of cells x years missing
#' 2- trim hauls with number of cells x years >2 % missing
#' outputs per survey_unit and grid resolution:
#' - a plot of number of cells x years with overlaid trimming options (png file);
#' - a map per haul showing nb of hauls retained and removed per trimming option (png file);
#' - a map per cell showing nb of years removed per trimming option (png file);
#' - statistics of hauls retained / removed per trimming option (csv file);
#' - a table of cells and years combination removed per trimming option (csv file);
#' - a table of combinations of hauls ids, cells and years removed per trimming option per survey (csv file);
#' - the global raw dataset with added columns indicating if the haul was retained / removed per trimming option (dataframe)
#'
#' @param data globfish raw dataset
#' @param hex_res resolution of grid hex size (7 o 8)
#'
#' @return
#' @export
#'

apply_trimming_per_survey_unit_method1 <- function(data, hex_res){
  
  
  # load coastlines for maps ----
  
  coast <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  
  
  
  # PREPARE GLOBAL GRID ----
  
  ## Shift longitudes ----
  
  data <- data.frame(data) %>%
    dplyr::filter(!is.na(latitude)) %>% 
    dplyr::mutate(longitude_s = ifelse(longitude > 150,
                                       longitude - 360,
                                       longitude))
  
  
  # Set up global grid ----
  
  dggs <- dggridR::dgconstruct(res = hex_res, metric = TRUE)
  
  
  # Pull out unique lat-lon ----
  
  unique_latlon <- data %>% 
    dplyr::distinct(latitude, longitude_s) %>%
    dplyr::select(latitude, longitude_s)
  
  
  # Get corresponding grid cells ----
  
  unique_latlon <- unique_latlon %>%
    dplyr::mutate(
      cell = dggridR::dgGEO_to_SEQNUM(dggs, longitude_s, latitude)$"seqnum")
  
  
  # Find cell centers ----
  
  cellcenters <- dggridR::dgSEQNUM_to_GEO(dggs, unique_latlon$"cell")
  
  
  # Linking cell centers to unique_latlon ----
  
  unique_latlon <- unique_latlon %>%
    dplyr::mutate(cell_center_longitude_s = cellcenters$"lon_deg") %>%
    dplyr::mutate(cell_center_latitude = cellcenters$"lat_deg")
  
  
  # Link centers back to main data table ----
  
  data_dg <- merge(data, unique_latlon, by = c("latitude", "longitude_s"),
                   all.x = TRUE)
  
  
  ## Make a list of all unique combination of year x cell x surveyid ----
  
  year_cell_count <- expand.grid("cell"        = unique(data_dg$"cell"),
                                 "year"        = unique(data_dg$"year"),
                                 "survey_unit" = unique(data_dg$"survey_unit"))
  
  
  ## Compute N hauls per year x cell x surveyid ----
  
  n_hauls <- data_dg %>%
    dplyr::group_by(cell, year, survey_unit) %>%
    dplyr::summarise(nhaul = length(unique(haul_id)))
  
  
  ## Merge to full list ----
  
  year_cell_count <- merge(year_cell_count, n_hauls, all.x = TRUE)
  
  
  ## Fill in for year x cell x surveyid that aren't present ----
  
  year_cell_count <- year_cell_count %>%
    dplyr::mutate(nhaul = tidyr::replace_na(nhaul, 0))
  
  
  # ORDER data ----
  
  survey_units <- sort(as.character(unique(year_cell_count$"survey_unit")))
  
  
  plot_list <- list()
  filtered_ordered_data <- list()
  data_dg_list <- list()
  
  for (surveyid in survey_units) {
    
    print("----------------surveyid----------------------")
    print(surveyid)
    
    ## Subset data for a surveyid ----
    
    dat <- subset(year_cell_count, year_cell_count$"survey_unit" == surveyid)
    
    
    ## Pivot (row = cells & columns = years) ----
    
    dat <- tidyr::pivot_wider(dat[ , -3], names_from = year, 
                              values_from = nhaul, values_fn = ~.x)
    
    dat <- as.data.frame(dat)
    
    
    ## Convert to matrix ----
    
    row_names <- dat[ , 1, drop = TRUE]
    # dat <- data.matrix(dat[ , -1])
    dat <- dat[ , -1]
    rownames(dat) <- row_names
    
    
    ## Remove empty years ----
    
    cols <- apply(dat, 2, sum)
    cols <- names(cols[cols > 0])
    dat  <- dat[ , cols]
    
    
    ## Remove empty cells ----
    
    rows <- apply(dat, 1, sum)
    rows <- which(rows > 0)
    dat  <- dat[rows, ]
    
    
    ## Convert n_hauls to boolean ----
    
    tab <- data.matrix(dat)
    tab[tab > 0] <- 1
    tab <- as.data.frame(tab)
    
    
    ## Compute number of cells for each year ----
    
    cols <- apply(tab, 2, sum)
    
    n_years <- data.frame("year" = names(cols), "n_cells" = cols, 
                          row.names = NULL)
    n_years <- n_years[order(n_years$"n_cells", decreasing = TRUE), ]
    
    
    ## Compute number of years for each cell ----
    
    rows <- apply(tab, 1, sum)
    
    n_cells <- data.frame("cell" = names(rows), "n_years" = rows, 
                          row.names = NULL)
    n_cells <- n_cells[order(n_cells$"n_years", decreasing = TRUE), ]
    
    
    ## Get unique values of N years ----
    
    unique_n_years <- n_cells[!duplicated(n_cells$"n_years"), "n_years"]
    
    
    ## Final vectors of order ----
    
    cells_order <- NULL
    years_order <- NULL
    
    
    for (i in 1:length(unique_n_years)) {
      
      
      ## Cells with same number of years ----
      
      pos <- which(n_cells$"n_years" == unique_n_years[i])
      
      
      ## Extract missing years per cell ----
      
      missing_years_list <- list()
      
      for (j in 1:length(pos)) {
        
        cells <- dat[which(rownames(dat) == n_cells[pos[j], "cell"]), ]
        cells <- data.matrix(cells)
        
        missing_years_list[[j]] <- colnames(cells)[which(cells == 0)]
        names(missing_years_list)[j] <- rownames(cells)
      }
      
      
      ## Get unique missing years ----
      
      yyears <- rev(sort(table(unlist(missing_years_list))))
      
      
      ## Order cells by years ----
      
      if (length(yyears) == 0) {
        
        cells <- names(missing_years_list)
        cells <- dat[which(rownames(dat) %in% cells), ]
        cells <- apply(cells, 1, sum)
        cells <- cells[order(cells, decreasing = TRUE)]
        
        cells_order <- c(cells_order, names(cells))
        
      } else {
        
        for (j in 1:length(yyears)) {   
          
          cells <- unlist(lapply(missing_years_list, function(x) {
            ifelse(names(yyears)[j] %in% x, 1, 0)
          }))
          
          cells <- names(cells[cells == 1])
          cells <- cells[which(!(cells %in% cells_order))]
          
          cells <- dat[which(rownames(dat) %in% cells), ]
          cells <- apply(cells, 1, sum)
          cells <- cells[order(cells, decreasing = TRUE)]
          
          cells_order <- c(cells_order, names(cells))
        }
      }
      
      
      ## Order years by cells ----
      
      yyears <- names(yyears)
      yyears <- yyears[which(!(yyears %in% years_order))]
      years_order <- c(years_order, yyears)
    }
    
    
    ## Clean orders ----
    
    cells_order <- rev(cells_order)
    
    if (length(years_order) < length(n_years$"year")) {
      years_order <- c(years_order, 
                       n_years[which(!(n_years$"year" %in% years_order)), "year"])
    }
    
    
    ## Order data from these vectors ----
    
    ordered_data <- subset(year_cell_count, year_cell_count$"survey_unit" == surveyid)
    
    ordered_data <- ordered_data %>%
      dplyr::filter(cell %in% cells_order) %>%
      dplyr::filter(year %in% years_order)
    
    ordered_data$"cell" <- factor(as.character(ordered_data$"cell"), 
                                  levels = cells_order)
    
    ordered_data$"year" <- factor(as.character(ordered_data$"year"), 
                                  levels = years_order)
    
    
    # calculate num missing cells x years for different thresholds. slow.
    
    cutoffs <- expand.grid("cell" = cells_order, "year" = years_order)
    
    n_years <- length(years_order)
    n_cells <- length(cells_order)
    
    for (j in 1:n_years) {
      
      for (k in 1:n_cells) {
        
        ## Subset data ----
        
        data_sub <- ordered_data %>%
          dplyr::filter(cell %in% cells_order[k:n_cells] & 
                          year %in% years_order[j:n_years])
        
        
        ## Get total nb of hauls (including missing) ----
        
        thisntot <- dplyr::summarize(data_sub, n = length(nhaul))
        thisntot <- thisntot$"n"
        
        
        ## Get nb of hauls with no data ----
        
        thisnmiss <- data_sub %>%
          dplyr::filter(nhaul == 0) %>%
          dplyr::summarize(n = length(nhaul))
        thisnmiss <- thisnmiss$"n"
        
        
        ## Get nb of hauls with data ----
        
        thisnkeep <- thisntot - thisnmiss
        
        
        ## Store results ----
        
        ro_w <- which(cutoffs$"year" == years_order[j] &
                        cutoffs$"cell" == cells_order[k])
        
        cutoffs[ro_w, "ntot"]  <- thisntot
        cutoffs[ro_w, "nmiss"] <- thisnmiss
        cutoffs[ro_w, "nkeep"] <- thisnkeep
      }
    }
    
    
    # FIND CUTOFF ----
    
    ## Cutoff based on nothing missing (NA = 0%) ----
    
    chosen_cutoff_0 <- cutoffs %>%
      dplyr::filter(nmiss == 0) %>%
      dplyr::filter(nkeep == max(nkeep))
    chosen_cutoff_0 <- chosen_cutoff_0[1, ]
    
    
    ## Cutoff based on nothing missing (NA < 0.2%) ----
    
    chosen_cutoff_02 <- cutoffs %>%
      dplyr::filter(nmiss / ntot < 0.02) %>%
      dplyr::filter(nkeep == max(nkeep))
    chosen_cutoff_02 <- chosen_cutoff_02[1, ]
    
    
    ## Add column IS_RETAINED_TRIMMING (NA = 0%) ----
    
    sel_years_0 <- years_order[which(years_order == chosen_cutoff_0$"year"):n_years]
    sel_cells_0 <- cells_order[which(cells_order == chosen_cutoff_0$"cell"):n_cells]
    
    pos_0 <- which(ordered_data$"cell" %in% sel_cells_0 & 
                     ordered_data$"year" %in% sel_years_0)
    
    ordered_data$"is_retained_trimming_0"        <- FALSE
    ordered_data$"is_retained_trimming_0"[pos_0] <- TRUE
    
    
    ## Add column IS_RETAINED_TRIMMING (NA < 0.2%) ----
    
    sel_years_02 <- years_order[which(years_order == chosen_cutoff_02$"year"):n_years]
    sel_cells_02 <- cells_order[which(cells_order == chosen_cutoff_02$"cell"):n_cells]
    
    pos_02 <- which(ordered_data$"cell" %in% sel_cells_02 & 
                      ordered_data$"year" %in% sel_years_02)
    
    ordered_data$"is_retained_trimming_02"         <- FALSE
    ordered_data$"is_retained_trimming_02"[pos_02] <- TRUE
    
    
    filtered_ordered_data[[surveyid]] <- ordered_data
    
    
    ## Make plot ----
    
    data_for_plot <- ordered_data %>%
      dplyr::filter(nhaul > 0)
    
    
    plot_list[[surveyid]] <- ggplot2::ggplot(data_for_plot) +
      
      ggplot2::geom_point(ggplot2::aes(x = year, y = cell, colour = nhaul), size = 6) +
      
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, 
                                                         hjust = 1, size = 8),
                     axis.text.y = ggplot2::element_text(size = 8),
                     plot.title  = ggplot2::element_text(hjust = 0.5)) +
      
      ggplot2::ggtitle(paste0("Survey: ", surveyid, " (Hex res = ", hex_res, ")")) +
      
      ggnewscale::new_scale_color() +
      
      #cutoff 0% missing io red
      ggplot2::geom_vline(ggplot2::aes(xintercept = chosen_cutoff_0$"year", 
                                       colour = 'red')) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = chosen_cutoff_0$"cell", 
                                       colour = 'red')) +
      
      #cutoff 2% missing io purple
      ggplot2::geom_vline(ggplot2::aes(xintercept = chosen_cutoff_02$"year", 
                                       colour = 'purple')) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = chosen_cutoff_02$"cell", 
                                       colour = 'purple')) +
      
      # scale
      ggplot2::theme(legend.position = "right") +
      ggplot2::scale_colour_manual(values = c('purple', 'red'), 
                                   labels = c( "2%", "0%"), 
                                   name = "Trimming")
    
    # save plot
    ggplot2::ggsave(here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_plot.png")), plot_list[[surveyid]], width = 10, height = 10)
    
    
    
    
    
    ## write some outputs ----
    
    # Table of cells and years removed per trimming option per surveyid
    
    filtered_ordered_data_df <- as.data.frame(filtered_ordered_data[[surveyid]])
    names(filtered_ordered_data_df) = c("cell", "year"," survey_unit", "nhaul", "is_retained_trimming_0", "is_retained_trimming_02")
    
    cell_years_removed0 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_0"] == FALSE,][, c("cell", "year")]
    cell_years_removed02 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_02"] == FALSE,][, c("cell", "year")]
    
    write.csv(cell_years_removed0, row.names = FALSE, file = here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_0_cell_year_removed.csv")))
    write.csv(cell_years_removed02, row.names = FALSE, file = here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_02_cell_year_removed.csv")))  
    
    
    # Append results to raw data
    
    cell_years_retained0 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_0"] == TRUE,][, c("cell", "year")]
    cell_years_retained02 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_02"] == TRUE,][, c("cell", "year")]
    
    data_dg_list[[surveyid]] <- data_dg %>%
      dplyr::filter(survey_unit == surveyid) %>% 
      dplyr::mutate(is_retained_trimming_0_hex = ifelse(year %in% cell_years_retained0$year & cell %in% cell_years_retained0$cell, TRUE, FALSE)) %>%
      dplyr::mutate(is_retained_trimming_02_hex = ifelse(year %in% cell_years_retained02$year & cell %in% cell_years_retained02$cell, TRUE, FALSE))
    
    colnames(data_dg_list[[surveyid]]) <- ifelse(grepl("is_retained", colnames(data_dg_list[[surveyid]])),
                                                 paste0(colnames(data_dg_list[[surveyid]]), hex_res),
                                                 colnames(data_dg_list[[surveyid]]))
    
    
    # Table of combinations of hauls ids, cells and years removed per trimming option per survey
    
    haul_id_removed_trimming_0 <- data_dg_list[[surveyid]] %>% 
      dplyr::filter(get(paste0("is_retained_trimming_0_hex", hex_res)) == FALSE) %>% 
      dplyr::select(haul_id, year, cell) %>% 
      dplyr::distinct()
    
    haul_id_removed_trimming_02 <- data_dg_list[[surveyid]] %>% 
      dplyr::filter(get(paste0("is_retained_trimming_02_hex", hex_res)) == FALSE) %>% 
      dplyr::select(haul_id, year, cell) %>% 
      dplyr::distinct()
    
    readr::write_delim(as.data.frame(haul_id_removed_trimming_0),  file = here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_0_hauls_removed.csv")), delim = ";")
    readr::write_delim(as.data.frame(haul_id_removed_trimming_02),  file = here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_02_hauls_removed.csv")), delim = ";")
    
    
    # Statistics of hauls removed for each trimming option
    
    data_dg_list[[surveyid]] %>%
      dplyr::distinct(haul_id) -> all_hauls
    
    #make dataframe
    df <- data.frame("name" = c("number of hauls removed", 
                                "percentage of hauls removed"), 
                     "0 percent trimming" = c(nrow(haul_id_removed_trimming_0), round(100*nrow(haul_id_removed_trimming_0) / nrow(all_hauls),1)),
                     "2 percent trimming" = c(nrow(haul_id_removed_trimming_02), round(100*nrow(haul_id_removed_trimming_02) / nrow(all_hauls),1)))
    
    write.csv(df, row.names = FALSE, file = here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_stats_hauls.csv")))
    
    
    
    
    
    
    ## Make maps --
    
    # Map of hauls retained and removed 
    
    # 0% trimming
    data_dg_list[[surveyid]] %>%
      dplyr::select(haul_id, longitude_s, latitude, paste0("is_retained_trimming_0_hex", hex_res)) %>% 
      dplyr::distinct() -> dat_plot
    
    map0_hauls <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_sf(data = coast) +
      ggplot2::geom_point(ggplot2::aes(x = longitude_s, y = latitude, colour = get(paste0("is_retained_trimming_0_hex", hex_res))), alpha = 0.6, size = 1) +
      ggplot2::scale_colour_manual(values = c("black", "red"), breaks = c(TRUE, FALSE), labels = c("haul \nretained", "haul \nremoved"), name = "") +
      ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 0% Hex res", hex_res)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                     axis.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
                     axis.text.y = ggplot2::element_text(size = 5)) +
      ggplot2::xlim(min(dat_plot$longitude_s), max(dat_plot$longitude_s)) +
      ggplot2::ylim(min(dat_plot$latitude), max(dat_plot$latitude)) +
      ggplot2::theme(plot.margin=ggplot2::margin(0,0,0,0))
    
    # 2% trimming
    data_dg_list[[surveyid]] %>%
      dplyr::select(haul_id, longitude_s, latitude, paste0("is_retained_trimming_02_hex", hex_res)) %>% 
      dplyr::distinct() -> dat_plot
    
    map02_hauls <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_sf(data = coast) +
      ggplot2::geom_point(ggplot2::aes(x = longitude_s, y = latitude, colour = get(paste0("is_retained_trimming_02_hex", hex_res))), alpha = 0.6, size = 1) +
      ggplot2::scale_colour_manual(values = c("black", "red"), breaks = c(TRUE, FALSE), labels = c("haul \nretained", "haul \nremoved"), name = "") +
      ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 2% Hex res", hex_res)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                     axis.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
                     axis.text.y = ggplot2::element_text(size = 5)) +
      ggplot2::xlim(min(dat_plot$longitude_s), max(dat_plot$longitude_s)) +
      ggplot2::ylim(min(dat_plot$latitude), max(dat_plot$latitude)) +
      ggplot2::theme(plot.margin=ggplot2::margin(0,0,0,0))
    
    map_hauls <- gridExtra::grid.arrange(map0_hauls, map02_hauls, ncol = 2)
    
    ggplot2::ggsave(here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_haul.png")), map_hauls)
    
    
    
    
    
    # Maps of cells (nb of years removed)  
    
    
    # Base map with empty cells
    
    if (surveyid %in% c("AI", "NZ-CHAT")){
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, year, cell) %>% 
        #apply longitude shift for mapping
        dplyr::mutate(cell_center_longitude_s = ifelse(cell_center_longitude_s > 150,
                                                       cell_center_longitude_s - 360,
                                                       cell_center_longitude_s)) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot
    }else{
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, year, cell) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot
    }
    
    map_cells_nyears <- ggplot2::ggplot() +
      ggplot2::geom_sf(data=coast) +
      ggplot2::geom_point(data = dat_plot, ggplot2::aes(x = cell_center_longitude_s, y = cell_center_latitude, fill = "0"), size = 2, shape = 1) + # for 0 years removed
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                     axis.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
                     axis.text.y = ggplot2::element_text(size = 5),
                     legend.title = ggplot2::element_blank()) +
      ggplot2::xlim(min(dat_plot$cell_center_longitude_s), max(dat_plot$cell_center_longitude_s)) +
      ggplot2::ylim(min(dat_plot$cell_center_latitude), max(dat_plot$cell_center_latitude)) +
      ggplot2::theme(plot.margin=ggplot2::margin(0,0,0,0)) 
    
    
    
    
    # 0% trimming - map of nb of years removed per cell
    
    if (surveyid %in% c("AI", "NZ-CHAT")){
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, paste0("is_retained_trimming_0_hex", hex_res), year, cell) %>% 
        #filter removed cells
        dplyr::filter(get(paste0("is_retained_trimming_0_hex", hex_res)) == FALSE) %>% 
        #apply longitude shift for mapping
        dplyr::mutate(cell_center_longitude_s = ifelse(cell_center_longitude_s > 150,
                                                       cell_center_longitude_s - 360,
                                                       cell_center_longitude_s)) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot0
    }else{
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, paste0("is_retained_trimming_0_hex", hex_res), year, cell) %>% 
        #filter removed cells
        dplyr::filter(get(paste0("is_retained_trimming_0_hex", hex_res)) == FALSE) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot0
    }
    
    if (nrow(dat_plot0)>=1){
      map0_cells_nyears <- map_cells_nyears +
        ggplot2::geom_point(data = dat_plot0, ggplot2::aes(x = cell_center_longitude_s, y = cell_center_latitude, colour = nyear), size = 2) +
        ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 0% Hex res", hex_res, "- nb yrs removed")) 
    }else{
      map0_cells_nyears <- map_cells_nyears +
        ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 0% Hex res", hex_res, "- nb yrs removed"))
    }
    
    
    
    
    # 2% trimming - map of nb of years removed per cell
    
    if (surveyid %in% c("AI", "NZ-CHAT")){
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, paste0("is_retained_trimming_02_hex", hex_res), year, cell) %>% 
        #filter removed cells
        dplyr::filter(get(paste0("is_retained_trimming_02_hex", hex_res)) == FALSE) %>% 
        #apply longitude shift for mapping
        dplyr::mutate(cell_center_longitude_s = ifelse(cell_center_longitude_s > 150,
                                                       cell_center_longitude_s - 360,
                                                       cell_center_longitude_s)) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot02
    }else{
      data_dg_list[[surveyid]] %>%
        dplyr::select(cell_center_longitude_s, cell_center_latitude, paste0("is_retained_trimming_02_hex", hex_res), year, cell) %>% 
        #filter removed cells
        dplyr::filter(get(paste0("is_retained_trimming_02_hex", hex_res)) == FALSE) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(cell_center_longitude_s, cell_center_latitude, cell) %>% 
        dplyr::summarize(nyear=length(year)) -> dat_plot02
    }
    
    if (nrow(dat_plot02)>=1){
      map02_cells_nyears <- map_cells_nyears +
        ggplot2::geom_point(data = dat_plot02, ggplot2::aes(x = cell_center_longitude_s, y = cell_center_latitude, colour = nyear), size = 2) +
        ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 2% Hex res", hex_res, "- nb yrs removed"))
    }else{
      map02_cells_nyears <- map_cells_nyears +
        ggplot2::ggtitle(paste("Survey:", surveyid, "- trimming 2% Hex res", hex_res, "- nb yrs removed"))
    }
    
    
    map_cells_nyears_multi <- gridExtra::grid.arrange(map0_cells_nyears, map02_cells_nyears, ncol = 2)
    
    ggplot2::ggsave(here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_grid_nyears.png")), map_cells_nyears_multi, width = 15, height = 7)
    
    
    
    
    
    
    
    # combined map of hauls and cells
    
    map <- gridExtra::grid.arrange(map0_hauls, map02_hauls, map0_cells_nyears, map02_cells_nyears, ncol = 2, nrow = 2)
    ggplot2::ggsave(here::here("outputs", "Flags", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_haul_grid_nyears.png")), map, width = 10, height =10)
    
    
    
  } #end of surveyid loop
  
  
  
  ## Return full dataset
  data_new <- do.call(rbind, data_dg_list)
  
  return(data_new)
  
}

