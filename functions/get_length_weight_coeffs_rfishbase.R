get_coeffs <- function(taxon_list, survey, date, save=F){
  
  library(data.table)
  library(dplyr)
  library(readr)
  library(rfishbase)
  library(stringr)
  
  s_time <- Sys.time()
  
  
  ### Load Data
  datalw <- taxon_list %>%
    dplyr::rename(level = rank) %>% 
    mutate(species = str_split(taxa, " ", simplify=T, n=2)[,2],
           level = case_when(level=="Family" ~ "family",
                             level=="Genus" ~ "genus",
                             level=="Species" ~"species",
                             level=="Subspecies" ~ "species")) %>% 
    select(family, genus, species, level) %>% 
    filter(!is.na(family))
  
  datalw$a <- datalw$b <- datalw$taxo <- datalw$source <- datalw$Type <- NA
  
  ### Get relationships
  
  for(i in 1:nrow(datalw)){
    
    if(datalw$level[i]=='species'){
      
      # Species
      level <- 'spe'
      lw.spe <- length_weight(paste(datalw$genus[i],datalw$species[i], sep = " ")) %>% 
        filter(!is.na(a),
               !is.na(b),
               Type=="TL")
      
      # Genus
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){
        genus <- datalw$genus[i]
        species_in_genus <- species_list(Genus = genus)
        lw.spe <- length_weight(species_in_genus) %>% 
          filter(!is.na(a),
                 !is.na(b),
                 Type=="TL")
        level <- 'gen'
        rm(genus, species_in_genus)
      }
      
      # Family
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){
        family <- datalw$family[i]
        species_in_family <- species_list(Family = family)
        lw.spe <- length_weight(species_in_family) %>% 
          filter(!is.na(a),
                 !is.na(b),
                 Type=="TL")
        level <- 'fam'
        rm(family, species_in_family)
      }
      
      if(nrow(lw.spe)>0){
      datalw$a[i] <- mean(lw.spe$a)
      datalw$b[i] <- mean(lw.spe$b)
      datalw$taxo[i] <- level
      datalw$Type[i] <- paste(unique(lw.spe$Type), collapse = "-")
      datalw$source[i] <- paste("fb", available_releases()[1], sep="-")}					
      rm(level, lw.spe)
    }
    
    if(datalw$level[i]=='genus'){
      # Genus
      genus <- datalw$genus[i]
      species_in_genus <- species_list(Genus = genus)
      lw.spe <- length_weight(species_in_genus) %>% 
        filter(!is.na(a),
               a<1,
               !is.na(b),
               Type=="TL")
      level <- 'gen'
      rm(genus, species_in_genus)
    
      # Family
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){
        family <- datalw$family[i]
        species_in_family <- species_list(Family = family)
        lw.spe <- length_weight(species_in_family) %>% 
          filter(!is.na(a),
                 !is.na(b),
                 Type=="TL")
        level <- 'fam'
        rm(family, species_in_family)
      }
      
      if(nrow(lw.spe)>0){
        datalw$a[i] <- mean(lw.spe$a)
        datalw$b[i] <- mean(lw.spe$b)
        datalw$taxo[i] <- level
        datalw$Type[i] <- paste(unique(lw.spe$Type), collapse = "-")
        datalw$source[i] <- paste("fb", available_releases()[1], sep="-")
      }					
      rm(level, lw.spe)
    }
    
    if(datalw$level[i]=='family'){
      
      # Family
      family <- datalw$family[i]
      species_in_family <- species_list(Family = family)
      lw.spe <- length_weight(species_in_family) %>% 
        filter(!is.na(a),
               !is.na(b),
               Type=="TL")
      level <- 'fam'
      rm(family, species_in_family)
      
      if(nrow(lw.spe)>0){      
        datalw$a[i] <- mean(lw.spe$a)
        datalw$b[i] <- mean(lw.spe$b)
        datalw$taxo[i] <- level
        datalw$Type[i] <- paste(unique(lw.spe$Type), collapse = "-")
        datalw$source[i] <- paste("fb", available_releases()[1], sep="-")
      }					
      rm(level, lw.spe)
    }
    
    
  }
  
  if(any(is.na(datalw$a))==FALSE){print("All length-weight relationship coefficients found!")}
  else{"Some coefficients are still missing!"}
  
  datalw <- datalw %>% 
    select(-family, -genus, -species, -level) %>% 
    rename(level_inferred = taxo)
  datalw <- cbind(taxon_list, datalw)
  
  if(save==TRUE){
    write.csv(datalw, file=paste0("length_weight/length.weight_",survey,"_",date,".csv"))
  }
  
  e_time <- Sys.time()
  print(s_time-e_time)
  return(datalw)
  
}