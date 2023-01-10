#######################################################
#### Coding: Aurore Maureaud, December 2022
#### Public survey metadata
#### Figures for data paper
#######################################################
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(sf)
sf::sf_use_s2(FALSE)
library(data.table)
library(googledrive)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
  # st_make_valid() %>% 
  # filter(continent %in% c("Europe","North America"))
target_crs <- st_crs("+proj=natearth2 +lon_0=-10")
world2 <- world %>% st_transform(crs = target_crs) %>% st_make_valid()

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)


#### Load files
survey <- read.csv("~/Documents/FISHGLOB/data/Compiled_data/public/FISHGLOB_public_v1.6_clean.csv")
meta_survey <- read_sf("/Volumes/Elements/Last save DTU AQUA DEC2020/PhD DTU Aqua/(vii) BTS paper/TrawlSurveyMetadata/data/metadata/Metadata_18062020.shp") %>% 
  filter(Survey %in% c("AI","BAS","BITS","CGFS","DFO-HS","DFO-QCS","DFO-WCHG","DFO-WCVI",
                       "EBS","EVHOE","GOA","GSL-N","GSL-S","Gulf of Mexico",
                       "IE-IGFS","NIGFS","NOR-BTS","Northeast US",
                       "NS-IBTS","PT-IBTS","ROCKALL","Scotian Shelf","Southeast US",
                       "SWC-IBTS","WCANN"))


################################################################################
#### Map of survey hauls
################################################################################
png("data_descriptor_figures/public_meta_map.png", width = 12*400, height = 10*400, res = 400)
print(ggplot(world)+
        geom_sf(fill = "antiquewhite3", color = NA)+
        geom_sf(data = meta_survey, fill="#41B6C4", color = "black", 
                size = 0.25, alpha = 0.5)+
        scale_color_distiller(palette = "Paired")+
        theme_minimal() + theme(legend.position = "none") +
        coord_sf(crs = "+proj=laea +x_0=40 +y_0=-74 +lon_0=-74 +lat_0=40")+
        xlab("") + ylab(""))
dev.off()


################################################################################
#### Tile plot of time-series
################################################################################
xx <- survey %>% 
  group_by(year, survey) %>% 
  summarize(nbr_haul = length(unique(haul_id)),
            nbr_taxa = length(unique(accepted_name)))
xx <- data.frame(xx)
xx_c <- complete(xx, expand(xx, survey, year))
xx_l <- data.frame(xx) %>% 
  group_by(survey) %>% 
  filter(year == first(year))

# make tile plot
png("data_descriptor_figures/tile_for_map.png",
    width = 10*200, height = 5*200, res =200)
ggplot(xx_c, aes(y = survey, x = year, fill = nbr_haul)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "grey95") +
  theme_bw(base_size=8)+
  scale_y_discrete(expand=c(0,0)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1970, 1980, 1990, 2000, 2010, 2020))+
  theme(legend.text=element_text(size = 10),
        axis.ticks=element_line(linewidth=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_text(angle = 60, size = 10, hjust = 1),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  xlab("") + ylab("") + labs(fill="Number of hauls")
dev.off()


################################################################################
#### Plots of survey flags for EVHOE
################################################################################
evhoe <- read.csv("~/Documents/FISHGLOB/data/Compiled_data/public/FISHGLOB_public_v1.4.csv") %>% 
  filter(survey == "EVHOE") %>% 
  select(haul_id, year, longitude, latitude, flag_trimming_hex7_0,
         flag_trimming_hex7_2, flag_trimming_hex8_0, flag_trimming_hex8_2,
         flag_trimming_2) %>% 
  mutate(flag_trimming_hex7 = ifelse(flag_trimming_hex7_0==TRUE & flag_trimming_hex7_2==TRUE,"both", NA),
         flag_trimming_hex7 = ifelse(flag_trimming_hex7_0==TRUE & is.na(flag_trimming_hex7_2),"0", flag_trimming_hex7),
         flag_trimming_hex7 = ifelse(is.na(flag_trimming_hex7_0) & flag_trimming_hex7_2==TRUE,"2", flag_trimming_hex7),
         flag_trimming_hex8 = ifelse(flag_trimming_hex8_0==TRUE & flag_trimming_hex8_2==TRUE,"both", NA),
         flag_trimming_hex8 = ifelse(flag_trimming_hex8_0==TRUE & is.na(flag_trimming_hex8_2),"0", flag_trimming_hex8),
         flag_trimming_hex8 = ifelse(is.na(flag_trimming_hex8_0) & flag_trimming_hex8_2==TRUE,"2", flag_trimming_hex8)) %>% 
  distinct() %>% 
  st_as_sf(coords = c("longitude","latitude"), crs = st_crs(world))

png("data_descriptor_figures/evhoe_hex8.png", 
    width = 10*200, height = 12*200, res = 200)
ggplot(world) + geom_sf(color = NA, fill = "antiquewhite3") +
  geom_sf(data = evhoe[is.na(evhoe$flag_trimming_hex8),], size = 5, shape = 21, col = "black") +
  geom_sf(data = evhoe[!is.na(evhoe$flag_trimming_hex8),], size = 5, alpha = 0.3, aes(color = flag_trimming_hex8)) +
  scale_color_manual(values = c("blue","aquamarine3"), na.value = "white") +
  coord_sf(xlim = c(-12,0),ylim=c(43,53)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 25))
dev.off()

png("data_descriptor_figures/evhoe_hex7.png", 
    width = 10*200, height = 12*200, res = 200)
ggplot(world) + geom_sf(color = NA, fill = "antiquewhite3") +
  geom_sf(data = evhoe[is.na(evhoe$flag_trimming_hex7),], size = 5, shape = 21, col = "black") +
  geom_sf(data = evhoe[!is.na(evhoe$flag_trimming_hex7),], size = 5, alpha = 0.3, aes(color = flag_trimming_hex7)) +
  scale_color_manual(values = c("blue","aquamarine3"), na.value = "white") +
  coord_sf(xlim = c(-12,0),ylim=c(43,53)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 25))
dev.off()

png("data_descriptor_figures/evhoe_biotime.png", 
    width = 10*200, height = 12*200, res = 200)
ggplot(world) + geom_sf(color = NA, fill = "antiquewhite3") +
  geom_sf(data = evhoe[!is.na(evhoe$flag_trimming_2),], size=5, alpha = 0.3, aes(color = flag_trimming_2)) +
  scale_color_manual(values = c("orange"), na.value = "grey90") +
  geom_sf(data = evhoe[is.na(evhoe$flag_trimming_2),], size = 5, shape = 21, col = "black") +
  coord_sf(xlim = c(-12,0),ylim=c(43,53)) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 25))
dev.off()


################################################################################
#### Tile plots for EVHOE - method 1
################################################################################

dat <- survey

dat %>%
  tidyr::drop_na(longitude) %>%
  tidyr::drop_na(latitude) -> dat

  data <- dat
  hex_res <- 7
  # load coastlines for maps ----
  coast <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  
  # PREPARE GLOBAL GRID ----
  
  ## Shift longitudes ----
  data <- data %>%
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
  surveyid <- "EVHOE"
  
  plot_list <- list()
  filtered_ordered_data <- list()
  data_dg_list <- list()
    
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
    
    data_for_plot_c <- complete(data_for_plot, expand(data_for_plot, cell, year))
    
    plot_list[[surveyid]] <- ggplot2::ggplot(data_for_plot_c) +
      
      ggplot2::geom_tile(ggplot2::aes(x = year, y = cell, fill = nhaul),
                         alpha = 0.8,colour = "white", linewidth = 0.25) +
      
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, 
                                                         hjust = 1, size = 16),
                     axis.text.y = ggplot2::element_text(size = 16),
                     plot.title  = ggplot2::element_text(hjust = 0.5)) +
      ylab("") + xlab("") +
      scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "grey95") +
      
      #cutoff 0% missing io red
      # ggplot2::geom_vline(ggplot2::aes(xintercept = chosen_cutoff_0$"year", 
      #                                  colour = 'blue', linewidth = 2)) +
      # ggplot2::geom_hline(ggplot2::aes(yintercept = chosen_cutoff_0$"cell", 
      #                                  colour = 'blue', linewidth = 2)) +
      # 
      # #cutoff 2% missing io purple
      # ggplot2::geom_vline(ggplot2::aes(xintercept = chosen_cutoff_02$"year", 
      #                                  colour = 'aquamarine3', linewidth = 2)) +
      # ggplot2::geom_hline(ggplot2::aes(yintercept = chosen_cutoff_02$"cell", 
      #                                  colour = 'aquamarine3', linewidth = 2)) +
      
      # scale
      ggplot2::theme(legend.position = "right") +
      ggplot2::scale_colour_manual(values = c('aquamarine3', 'blue'), 
                                   labels = c( "2%", "0%"), 
                                   name = "Trimming")
    
png(paste0("data_descriptor_figures/tile_for_flags_no_lines_",hex_res,".png"),
    width = 10*200, height = 10*200, res =200)
print(plot_list)
dev.off()


################################################################################
#### Tile plots for EVHOE - method 2
################################################################################


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
    #readr::write_delim(as.data.frame(haul_id_removed),  file = here::here("standardization_steps", "outputs", "trimming_method2", paste0(meta_spat$survey_unit[i],  "_hauls_removed.csv")), delim = ";")
    
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
    
    #write.csv(df, row.names = FALSE, file = here::here("standardization_steps", "outputs", "trimming_method2", paste0(meta_spat$survey_unit[i], "_stats_hauls.csv")))
    
    
    
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
    
    #ggplot2::ggsave(here::here("standardization_steps", "outputs", "trimming_method2", paste0(bt_years_max_loc[i,1],"_map_per_haul.png")), p)
    
  }

  
bt_4loc_10yr <- bt_4loc_10yr %>% 
  filter(survey_unit == "EVHOE") %>% 
  group_by(year, cell, is_retained_biotime) %>% 
  summarize(nhaul = length(cell))

bt_4loc_10yr_c <- complete(data.frame(bt_4loc_10yr), expand(bt_4loc_10yr, cell, year))

plot_list[[surveyid]] <- ggplot2::ggplot(bt_4loc_10yr_c) +
  
  ggplot2::geom_tile(ggplot2::aes(x = year, y = as.factor(cell), fill = nhaul), linewidth = 0.25) +
  ggplot2::geom_tile(data = bt_4loc_10yr_c[bt_4loc_10yr_c$is_retained_biotime==FALSE,],
                     aes(x = year, y = as.factor(cell), fill = nhaul),
                     colour = "darkorange2", linewidth = 1) +
  
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, 
                                                     hjust = 1, size = 16),
                 axis.text.y = ggplot2::element_text(size = 16),
                 plot.title  = ggplot2::element_text(hjust = 0.5),
                 legend.position = "none") +
  ylab("") + xlab("") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "grey95") +
  #scale_color_manual(values = c("orange", "white"), na.value = "white") +
  scale_x_continuous(expand=c(0,0))



png(paste0("data_descriptor_figures/tile_for_flags_method2.png"),
    width = 9.5*200, height = 10*200, res =200)
print(plot_list)
dev.off()
