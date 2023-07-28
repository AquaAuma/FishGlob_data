taxon_list <- read_csv("length_weight/Spanish.taxa.DATRAS.FB.tofill.csv")


get_coeffs <- function(taxon_list, survey, save=F){
  
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
           fao = ifelse(survey %in% c("GRL-DE","NOR-BTS","NS-IBTS","NIGFS",
                                      "BITS","SWC-IBTS","ROCKALL","PT-IBTS",
                                      "EVHOE","IE-IGFS","FR-CGFS","SP-PORC","SP-NORTH","SP-ARSA"),27,NA),
           lme = ifelse(survey=="NOR-BTS",20,lme),
           lme = ifelse(survey=="GRL-DE",19,lme),
           level = case_when(level=="Family" ~ "family",
                             level=="Genus" ~ "genus",
                             level=="Species" ~"species",
                             level=="Subspecies" ~ "species")) %>% 
    select(family, genus, species, level, lme, fao)
  
  
  ref.lme <- read.csv('functions/ref.lme.fishbase.csv')
  
  ### Add lme from fishbase
  datalw <- left_join(datalw, ref.lme, by='lme')
  unique(datalw$fishbase.lme) # no NA, ok
  setnames(datalw, old='fishbase.lme', new='FB_E_Code')
  datalw$a <- datalw$b <- datalw$taxo <- datalw$type.length <- datalw$source <- datalw$Type <- NA
  
  ### Get relationships
  
  for(i in 1:nrow(datalw)){
    
    fao <- datalw$fao[i]
    
    if(datalw$level[i]=='species'){
      
      # Species
      level <- 'spe'
      lw.spe <- length_weight(paste(datalw$genus[i],datalw$species[i], sep = " ")) %>% 
        filter(!is.na(a),
               !is.na(b),
               !is.na(Type))
      
      # Genus
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){
        genus <- datalw$genus[i]
        species_in_genus <- species_list(Genus = genus)
        lw.spe <- length_weight(species_in_genus) %>% 
          filter(!is.na(a),
                 !is.na(b),
                 !is.na(Type))
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
                 !is.na(Type))
        level <- 'fam'
        rm(family, species_in_family)
      }
      
      if(nrow(lw.spe)>0){
      datalw$a[i] <- mean(lw.spe$a)
      datalw$b[i] <- mean(lw.spe$b)
      datalw$taxo[i] <- level
      datalw$Type[i] <- paste(unique(lw.spe$Type), collapse = "-")
      datalw$source[i] <- paste("fb", available_releases()[1], sep="-")}					
      rm(level, lw.spe,fao)
    }
    
    if(datalw$level[i]=='genus'){
      # Genus
      genus <- datalw$genus[i]
      species_in_genus <- species_list(Genus = genus)
      lw.spe <- length_weight(species_in_genus) %>% 
        filter(!is.na(a),
               !is.na(b),
               !is.na(Type))
      level <- 'gen'
      rm(genus, species_in_genus)
    
      # Family
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){
        family <- datalw$family[i]
        species_in_family <- species_list(Family = family)
        lw.spe <- length_weight(species_in_family) %>% 
          filter(!is.na(a),
                 !is.na(b),
                 !is.na(Type))
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
      rm(level, lw.spe, fao)
    }
    
    if(datalw$level[i]=='family'){
      
      # Family
      family <- datalw$family[i]
      species_in_family <- species_list(Family = family)
      lw.spe <- length_weight(species_in_family) %>% 
        filter(!is.na(a),
               !is.na(b))
      level <- 'fam'
      rm(family, species_in_family)
      
      if(nrow(lw.spe)>0){      
        datalw$a[i] <- mean(lw.spe$a)
        datalw$b[i] <- mean(lw.spe$b)
        datalw$taxo[i] <- level
        datalw$Type[i] <- paste(unique(lw.spe$Type), collapse = "-")
        datalw$source[i] <- paste("fb", available_releases()[1], sep="-")
      }					
      rm(level, lw.spe, fao)
    }
    
    
  }
  
  if(any(is.na(datalw$a))==FALSE){print("All length-weight relationship coefficients found!")}
  else{"Some coefficients are still missing!"}
  
  datalw <- datalw %>% 
    select(-family, -genus, -species, -level, -lme, -fao, -FB_E_Code) %>% 
    rename(level_inferred = taxo)
  datalw <- cbind(taxon_list, datalw)
  
  if(save==TRUE){
    write.csv(datalw, file=paste0("length.weight/length.weight_",survey,".csv"))
  }
  
  e_time <- Sys.time()
  print(s_time-e_time)
  return(datalw)
  
}