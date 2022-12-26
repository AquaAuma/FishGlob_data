





#' Read and clean FISHGLOB data
#'
#' @return
#' @export
#'
read_clean_data <- function(){
  
  load("outputs/Compiled_data/FishGlob_v1.7_clean.RData"))
  dat <- data

  dat %>%
    tidyr::drop_na(longitude) %>%
    tidyr::drop_na(latitude) -> dat

  return(dat)
}




#' Do taxonomic check per region
#' A species is flagged if:
#' (1) it has periods of absence or presence lasting less than 95% of the years and
#' (2) it has less than 4 periods of presence vs periods of absence
#' outputs per region: 
#' -a list of flagged species (txt file) 
#' -statistics of flagged species (csv file)
#' -a plot of flagged species vs years indicating presence / absence
#' code adapted from https://github.com/pinskylab/OceanAdapt/blob/master/R/add-spp-to-taxonomy.Rmd#L33
#' @param df fishglob dataset
#' @param region name of region as per survey column
#'
#' @return
#' @export
#'
flag_spp <- function(df, region){

  cat("taxonomy check for region: ", region, sep="\n")

  #select region (survey column)
  df %>%
    dplyr::mutate(survey = forcats::as_factor(survey)) %>%
    dplyr::filter(survey == region) -> df_region

  #get grouped species names per year
  df_region %>%
    dplyr::select(accepted_name, year) %>%
    dplyr::group_by(accepted_name) %>%
    dplyr::distinct() -> df

  #make species x year matrix
  years <- unique(df$year)
  df2 <- data.frame(matrix(ncol = length(years)+1, nrow = length(df$accepted_name)))
  x <- c('spp', unique(df$year))
  colnames(df2) <- x
  df2$spp <- df$accepted_name

  #fill up matrix with FALSE/TRUE depending on species presence/absence
  n <- 2
  for (y in years){
    df2[,n] <- c(apply(df, 1, function(r) any(r %in% c(y))))
    n <- n+1
  }

  #summarize
  df2 <- df2 %>%
    dplyr::group_by(spp) %>%
    dplyr::summarize_all(any)

  #create new dataframe
  df3 <- data.frame(matrix(ncol = 3, nrow = length(df2$spp)))
  colnames(df3) <- c('spp', 'rlmax', 'PresToAbs')

  #fill up the dataframe
  for (n in 1:length(df2$spp)){

    #Compute the lengths and values of runs of equal values in a vector
    r <- rle(df2[n,])

    sp <- c(df2[n,1])

    rlmax <- max(r$lengths)

    df3[n,1] <- sp
    df3[n,2] <- rlmax #maximum run length
    df3[n,3] <- length(r$values) #total nb of runs

  }

  #get species with maximum run length lower than 95% of the years (56)
  #and that occurred in less than 4 runs
  x <- round(0.95*length(years))
  df3 <- df3[df3$rlmax < x,]
  df3 <- df3[df3$PresToAbs < 4,]

  testdf <- data.frame(spp = df3$spp)

  if (dim(testdf)[1] > 0) {

    #write txt of flagged spp in region
    sink(here::here("standardization_steps", "outputs", "taxonomic_flagging", paste0(region, "_flagspp.txt")))
    cat(testdf$spp, sep = " ; ")
    sink()
    
    #write stats of species flagged
    data <- data.frame("name" = c("Total number of species", "Percentage of species flagged"),
                       "nb" = c(length(df2$spp), round(100*(length(testdf$spp) /  length(df2$spp)), 1)))
    write.csv(data, file = here::here("standardization_steps", "outputs", "taxonomic_flagging", paste0(region, "_stats.csv")), row.names = FALSE)
    
    #create absence dataframe for plotting
    allyears <- min(years):max(years)
    abs <- data.frame("accepted_name" = rep(df3$spp, each = length(allyears)),
                      "year" = rep(allyears, length(df3$spp)))

    #get presence dataframe for plotting
    pres <- subset(df_region, accepted_name %in% testdf$spp) %>%
      dplyr::select(accepted_name, year)

    #make summary scatterplot for region
    p <- ggplot2::ggplot() +
      #absences
      ggplot2::geom_point(data = abs, ggplot2::aes(x = year, y = accepted_name, color = "black"), alpha = 0.2) +
      #presences
      ggplot2::geom_point(data = pres, ggplot2::aes(x = year, y = accepted_name, fill = "black")) +
      ggplot2::ggtitle(paste0("Region: ", region)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(values=c('black'), labels = c("presence"), name = "") +
      ggplot2::scale_colour_manual(values=c('black'), labels = c("absence"), name = "") +
      ggplot2::scale_x_continuous(breaks = allyears)

    ggplot2::ggsave(p, filename = here::here("standardization_steps", "outputs", "taxonomic_flagging", paste0(region, "_taxonomic_flagging.png")))

  }else{
    
    print("----- No species flagged for this region")
  }


}






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
    ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_plot.png")), plot_list[[surveyid]], width = 10, height = 10)
    
    
    
    
    
    ## write some outputs ----
    
    # Table of cells and years removed per trimming option per surveyid
    
    filtered_ordered_data_df <- as.data.frame(filtered_ordered_data[[surveyid]])
    names(filtered_ordered_data_df) = c("cell", "year"," survey_unit", "nhaul", "is_retained_trimming_0", "is_retained_trimming_02")
    
    cell_years_removed0 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_0"] == FALSE,][, c("cell", "year")]
    cell_years_removed02 <- filtered_ordered_data_df[filtered_ordered_data_df["is_retained_trimming_02"] == FALSE,][, c("cell", "year")]
    
    write.csv(cell_years_removed0, row.names = FALSE, file = here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_0_cell_year_removed.csv")))
    write.csv(cell_years_removed02, row.names = FALSE, file = here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_02_cell_year_removed.csv")))  
    
    
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
    
    readr::write_delim(as.data.frame(haul_id_removed_trimming_0),  file = here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_0_hauls_removed.csv")), delim = ";")
    readr::write_delim(as.data.frame(haul_id_removed_trimming_02),  file = here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_trimming_02_hauls_removed.csv")), delim = ";")
    
    
    # Statistics of hauls removed for each trimming option
    
    data_dg_list[[surveyid]] %>%
      dplyr::distinct(haul_id) -> all_hauls
    
    #make dataframe
    df <- data.frame("name" = c("number of hauls removed", 
                                "percentage of hauls removed"), 
                     "0 percent trimming" = c(nrow(haul_id_removed_trimming_0), round(100*nrow(haul_id_removed_trimming_0) / nrow(all_hauls),1)),
                     "2 percent trimming" = c(nrow(haul_id_removed_trimming_02), round(100*nrow(haul_id_removed_trimming_02) / nrow(all_hauls),1)))
    
    write.csv(df, row.names = FALSE, file = here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_stats_hauls.csv")))
    
    
    
    
    
    
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
    
    ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_haul.png")), map_hauls)
    
    
    
    
    
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
    
    ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_grid_nyears.png")), map_cells_nyears_multi, width = 15, height = 7)
    
    
    
    
    
    
    
    # combined map of hauls and cells
    
    map <- gridExtra::grid.arrange(map0_hauls, map02_hauls, map0_cells_nyears, map02_cells_nyears, ncol = 2, nrow = 2)
    ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method1", paste0("hex_res", hex_res), paste0(surveyid, "_hex_res_", hex_res, "_map_per_haul_grid_nyears.png")), map, width = 10, height =10)
    
    
    
  } #end of surveyid loop
  
  
  
  ## Return full dataset
  data_new <- do.call(rbind, data_dg_list)
  
  return(data_new)
  
}










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
    readr::write_delim(as.data.frame(haul_id_removed),  file = here::here("standardization_steps", "outputs", "trimming_method2", paste0(meta_spat$survey_unit[i],  "_hauls_removed.csv")), delim = ";")

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
    
    write.csv(df, row.names = FALSE, file = here::here("standardization_steps", "outputs", "trimming_method2", paste0(meta_spat$survey_unit[i], "_stats_hauls.csv")))
    
    

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

    ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method2", paste0(bt_years_max_loc[i,1],"_map_per_haul.png")), p)

  }

  #return full dataset
  return(data_new)


}
