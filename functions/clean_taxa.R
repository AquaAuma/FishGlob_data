# ------------------------------------------ #
# Function: clean taxa
# Author: Juliano Palacios Abrantes | j.palacios@oceans.ubc
# Last Updated: June 2021
# ------------------------------------------ #

# ------------#
# Explanation
# ------------#
# This function is used to clean taxa names. It follows a series
# of steps where it i) corrects misspellings, ii) identifies synonyms and 
# non-accepted names, iii) gets corrected names and worm's aphia id, iv) gets
# fishbase ID and v) filters out non-fish classes.

# ------------#
# Inputs
# ------------#
# taxon_list; Expects a list of scientific names or worm's aphia ID (e.g., Gadus morhua or 125342)
# input_survey = "NA", expects a character identifying the survey code (e.g., "CHL")
# save = F, If set to T will save the final output and the non-cleaned taxa 
# output = NA, if "over" it will override any previous data identified by input_survey

# ------------#
# Outputs
# ------------#
# This function returns two main data frames:

# A data frame with the supplied taxa/aphia id, the valid scientific name and aphia ID,
# fishbase id, kingdom, phylum, class, order,family, genus, and the rank of the supplied
# taxa

# It also saves a data frame with the taxa that was not identified by the function. Note
# that this result only appears if save = T.

# ------------#
# Requires
# ------------#
# The following packages are needed for the function to run: "tidyverse","taxize","worrms","here", "worms"
# Note that the function will automatically install any package you need. You do not need
# to call any of the libraries neither.

# ------------#
# Example
# ------------#

# Test for misspelled species
# taxa <- c("Gadus morhua","plop", "Thunus alalonga","Octopus vulgaris")
# # Call function
# clean_taxa(taxon_list = taxa, input_survey = "Test", save = F, output = "")

# OUTPUT
# Time difference of -3.160841 secs
# query worms_id SpecCode         taxa  kingdom   phylum          class      order  family genus    rank        survey
# 1 gaduss morhua   126436       69 Gadus morhua Animalia Chordata Actinopterygii Gadiformes Gadidae Gadus Species  test

# ------------#
# Function
# ------------#

clean_taxa <- function(taxon_list, input_survey = "NA", save = F, output = NA, fishbase=TRUE){
  
  # Make sure you have all packages installed
  packages_needed <- c("tidyverse","taxize","worrms","here","readr","worms","rfishbase")
  install_me <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
  
  if(length(install_me) > 0){
    print(paste("Installing package(s) <",install_me,"> before running the function"))
    install.packages(install_me)
  } 
  
  # Start routine
  s_time <- Sys.time()
  
  # Get WoRM's id for sourcing
  wrm <- taxize::gnr_datasources() %>% 
    dplyr::filter(title == "World Register of Marine Species") %>% 
    dplyr::pull(id)
  
  ##---------------##
  # Worms Names and IDs
  ##---------------##
  
  # Convert to number a numeric character list and get species
  if(stringr::str_detect(taxon_list[1],"[1-9]") == TRUE){
    
    # Set list to numeric
    suppressWarnings( # No need for NAs warning. already taken into consideration
    taxon_id <- as.numeric(taxon_list)
    )
    # Remove NAs in data (in case you have a mix of spp and codes) 
    # Will return an error that you should check
    taxon_id <- taxon_id[!is.na(taxon_id)]
    
    # Get discarded aphias
    missing_aphiaid <- tibble::tibble(
      query = taxon_list) %>% 
      filter(!query %in% taxon_id)
    
    
    # For some reason `wm_record)()` only works for 50 species, it is unfortunate 
    # cus it is way faster than `wormsbyid()` which is from worms package
    if(length(taxon_id) <= 50){
      worms_db <- worrms::wm_record(taxon_id) %>% 
        # Select only marine species (NOTE: These are not exclusively marine species)
        dplyr::select(
          taxa = valid_name,
          worms_id = valid_AphiaID,kingdom:genus,isMarine,rank) %>% 
        # Include originally supplied taxa
        dplyr::mutate(
          query = as.character(taxon_id)
        )
    }else{
      worms_db <-  worms::wormsbyid(taxon_id,verbose = F) %>% # works but takes time
        dplyr::select(
          taxa = valid_name,
          worms_id = valid_AphiaID,
          kingdom:genus,isMarine,rank) %>%
      # Include originally supplied taxa
      dplyr::mutate(
        query = as.character(taxon_id) # so it matches cases when the query is a scientific name
      )
    }
    
    # Get list of taxa name from worms
    taxon_list <- worms_db %>% dplyr::pull(taxa)
    
    # Get missing data for saving latter
    missing_data <- tibble(
      query = as.character(taxon_id)) %>% 
      dplyr::filter(!query %in% worms_db$query) 
    
    if(nrow(missing_aphiaid)==0){missing_data <- missing_data
    }else{missing_data <- rbind(missing_aphiaid, missing_data)}
    
  }else{ # close when taxon list is AphaiID
    # If scientific names are provided, check synonyms and get correct name and ID
    # NOTE: it takes longer because it goes trough a
    # series of checkpoints a taxon validation
    
    
    # If scientific names are provided, check misspelling
    fix_taxon <- taxize::gnr_resolve(taxon_list,
                                     data_source_ids = wrm,
                                     best_match_only = TRUE,
                                     canonical = TRUE,
                                     ask = FALSE) %>% 
      dplyr::select(
        query = user_supplied_name,
        taxa = matched_name2)
    
    # Missing in fix_taxon
    missing_misspelling <- tibble(
      query = taxon_list) %>% 
      dplyr::filter(!query %in% fix_taxon$query)
    
    # Get Alphaid of taxon (Takes some time)
    
    alphaid <- tibble::tibble(
      fix_taxon,
      worms_id = cbind(worms::wormsbynames(fix_taxon$taxa,
                                           verbose = FALSE)
                       )
    )
    
    
    # Missing in AphiaIDs
    missing_alphaid <- alphaid %>% 
      dplyr::filter(is.na(worms_id$AphiaID)) %>% 
      dplyr::select(query)
    
    # Get correct names and full classification
    worms_db <-  worms::wormsbyid(as.numeric(alphaid$worms_id$AphiaID),
                                  verbose = F) %>% 
      dplyr::select(
        taxa = valid_name, # selects valid name in case is synonym
        worms_id = valid_AphiaID, # selects valid id in case is synonym
        kingdom,phylum,class,order,family,genus,isMarine,rank
      ) %>%
      # Include originally supplied taxa
      dplyr::mutate(
        query = fix_taxon$query
      )
    
    
    # Get Missing information
    missing_data <- dplyr::bind_rows(missing_alphaid,missing_misspelling)
    
    
  } # close else of species names
  
  
  ##---------------##
  # Filter out unwanted records
  ##---------------##
  
  worms_db_fish <- worms_db %>% 
    # Select only marine species (NOTE: These are not exclusively marine species)
    dplyr::filter(isMarine == 1) %>% 
    # remove non-fish species (a.k.a. invertebrates, mammals...)
    dplyr::filter(class %in% c("Elasmobranchii","Actinopterygii","Holocephali","Myxini",
                               "Petromyzonti", "Actinopteri", "Teleostei", "Holostei",
                               "Chondrostei")) %>% 
    dplyr::select(-isMarine) # we don't really need this information
  
  # Feedback message
  if(nrow(worms_db) != nrow(worms_db_fish)){
    n_dropped <- nrow(worms_db)- nrow(worms_db_fish)
    print(paste("Dropped",n_dropped,"non-fish/non-marine taxa"))
  }
  
  ##---------------##
  # Get fishbase id
  ##---------------##
  
  if(fishbase==TRUE){
  fishbase_id <- rfishbase::species(worms_db_fish$taxa, server = "fishbase") %>% 
    dplyr::select(SpecCode,
                  taxa=Species)
  } else {
    fishbase_id <- data.frame(taxa = worms_db_fish$taxa) %>% 
      mutate(SpecCode = NA)
  }
  
  ##---------------##
  # Final Database
  ##---------------##
  
  # Final clean output data frame
  output_df <- worms_db_fish %>% 
    dplyr::left_join(fishbase_id,
                     by = "taxa") %>% 
    # rest of selection
    dplyr::distinct() %>% 
    dplyr::select(
      query,
      worms_id,
      SpecCode,
      everything()
    ) %>% 
    dplyr::mutate(survey = input_survey)
  
  ##---------------##
  # Save data
  ##---------------##
  
  if(save == TRUE){
    
    # If there is n existing dtabase, it creates one
    if(file.exists(here::here("taxa_analysis/results/clean_taxon.csv")) == F){
      old_data <- tibble::tibble()
    }else{
      
      # Load existing data
      suppressMessages(
        old_data <- read_csv(here::here("taxa_analysis/results/clean_taxon.csv"))
      )
      
      # Output options, overwrite, add or just create the table
      
      # If the survey is already in the data we need to select a step; overwrite or add
      if(input_survey %in% old_data$survey){
        
        if(is.na(output) | !output %in% (c("over","add"))){
          print("In order to save an existing surve you need ot select an output option. Select output = over for overwriting existing data or output = add for adding to current data.")
          stop()
          
        }
        
        # Do you want to overwrite the previous data?
        if(output == "over"){
          
          old_data <- old_data %>% 
            dplyr::filter(survey != input_survey)
        }
        
        # Do you want to add to previous data?
        if(output == "add"){
          
          # Removes species that already exist
          old_data <- old_data %>% 
            dplyr::anti_join(output_df,
                             by = c("worms_id", "SpecCode",
                                    "taxa", "kingdom", "phylum", "class", "order", "family", "genus", 
                                    "survey")
            )
          
        }
        
      } # Close if of existing survey
      
      
      # include new list 
      bind_rows(old_data,output_df) %>% 
        write_csv(here::here("taxa_analysis/results/clean_taxon.csv"))
      
      # Return a message with basic info
      c_list <- paste(unique(old_data$survey),collapse =",")
      
      print(paste(input_survey,"survey data saved!","The taxon list now contains information for the following surveys",c_list,"and",input_survey))
      
      
      # Save Non cleaned data if there are any
      if(nrow(missing_data) > 0){
        
        missing_name <- paste0(input_survey,"_missing.csv")
        # Missing taxa
          write_csv(missing_data, paste0(here::here("taxa_analysis/results",missing_name)))
    }else{
      print("No missing taxa from worms, all good")
    }  
  } # Close if for missing taxa
  
}

# Misc,
if(exists("n_dropped")==FALSE){n_dropped <- 0}
print(paste("Returned", nrow(output_df), "taxa, dropped",n_dropped,"and failed to identify",nrow(missing_data),". All taxa assessed =", nrow(output_df) + n_dropped  == length(taxon_list)))
e_time <- Sys.time()
print(s_time-e_time)
return(output_df)

}