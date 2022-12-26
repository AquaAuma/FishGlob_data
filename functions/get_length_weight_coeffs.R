
get_coeffs <- function(taxon_list, survey, save=F){
  
  library(data.table)
  library(dplyr)
  library(RODBC)
  library(car)
  library(readr)
  
  s_time <- Sys.time()
  
  ### CONNECT
  fb <- odbcConnect('Fbapp')
  
  ### Load Data
  datalw <- taxon_list %>%
    dplyr::rename(level = rank) %>% 
    mutate(species = str_split(taxa, " ", simplify=T, n=2)[,2],
           fao = ifelse(survey %in% c("GRL-DE","NOR-BTS","NS-IBTS","NIGFS",
                                      "BITS","SWC-IBTS","ROCKALL","PT-IBTS",
                                      "EVHOE","IE-IGFS","FR-CGFS"),27,NA),
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
  datalw$a <- datalw$b <- datalw$taxo <- datalw$type.length <- datalw$source <- NA
  
  ### Get relationships
  
  for(i in 1:nrow(datalw)){
    
    fao <- datalw$fao[i]
    
    if(datalw$level[i]=='species'){
      
      # Species LME
      lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((SPECIES.Species)='",as.character(datalw$species[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))
      level <- 'spe-LME'
      
      # Species FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((SPECIES.Species)='",as.character(datalw$species[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'spe-FAO'}
      
      # Species all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((SPECIES.Species)='",as.character(datalw$species[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'spe-Ocean'}
      
      # Genus LME
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))
      level <- 'gen-LME'}
      
      # Genus FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'gen-FAO'}
      
      # Genus all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'gen-Ocean'}
      
      # Family LME
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){fam <- data.frame(sqlQuery(fb, paste("SELECT SPECIES.Genus, FAMILIES.Family FROM FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"'))",sep="")))
      level <- 'fam-LME'
      lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))}
      
      # Family FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'fam-FAO'}
      
      # Family all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'fam-Ocean'}
      
      if(nrow(lw.spe)>0){datalw$a[i] <- mean(lw.spe$a)
      datalw$b[i] <- mean(lw.spe$b)
      datalw$taxo[i] <- level
      datalw$type.length[i] <- 'TL'
      datalw$source[i] <- 'fb'}					
      rm(level, lw.spe,fao)
    }
    
    if(datalw$level[i]=='genus'){
      # Genus LME
      lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))
      level <- 'gen-LME'
      
      # Genus FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'gen-FAO'}
      
      # Genus all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'gen-Ocean'}
      
      # Family LME
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){fam <- data.frame(sqlQuery(fb, paste("SELECT SPECIES.Genus, FAMILIES.Family FROM FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode WHERE (((SPECIES.Genus)='",as.character(datalw$genus[i]),"'))",sep="")))
      level <- 'fam-LME'
      lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))}
      
      # Family FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'fam-FAO'}
      
      # Family all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'fam-Ocean'}
      
      if(nrow(lw.spe)>0){datalw$a[i] <- mean(lw.spe$a)
      datalw$b[i] <- mean(lw.spe$b)
      datalw$taxo[i] <- level
      datalw$type.length[i] <- 'TL'
      datalw$source[i] <- 'fb'}					
      rm(level, lw.spe,fao)
    }
    
    if(datalw$level[i]=='family'){
      
      # Family LME
      lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, ECOSYSTEMCNTREF.E_CODE FROM (POPLW INNER JOIN (SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) ON POPLW.SpecCode = SPECIES.SpecCode) INNER JOIN ECOSYSTEMCNTREF ON POPLW.C_Code = ECOSYSTEMCNTREF.C_CODE WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((ECOSYSTEMCNTREF.E_CODE)=",datalw$FB_E_Code[i],"))",sep="")))
      level <- 'fam-LME'
      
      # Family FAO
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality, COUNTFAO.AreaCode FROM ((SPECIES INNER JOIN FAMILIES ON SPECIES.FamCode = FAMILIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode) INNER JOIN COUNTFAO ON (POPLW.C_Code = COUNTFAO.C_Code) AND (POPLW.SpecCode = COUNTFAO.SpecCode) WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL') AND ((COUNTFAO.AreaCode)=",fao,"))",sep="")))
      level <- 'fam-FAO'}
      
      # Family all ocean
      if(nrow(lw.spe)==0 | ncol(lw.spe)<3){lw.spe <- data.frame(sqlQuery(fb, paste("SELECT FAMILIES.Family, SPECIES.Genus, SPECIES.Species, POPLW.a, POPLW.b, POPLW.Type, POPLW.Locality FROM (FAMILIES INNER JOIN SPECIES ON FAMILIES.FamCode = SPECIES.FamCode) INNER JOIN POPLW ON SPECIES.SpecCode = POPLW.SpecCode WHERE (((FAMILIES.Family)='",as.character(datalw$family[i]),"') AND ((POPLW.Type)='TL'))",sep="")))
      level <- 'fam-Ocean'}
      
      if(nrow(lw.spe)>0){datalw$a[i] <- mean(lw.spe$a)
      datalw$b[i] <- mean(lw.spe$b)
      datalw$taxo[i] <- level
      datalw$type.length[i] <- 'TL'
      datalw$source[i] <- 'fb'}					
      rm(level, lw.spe,fao)
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