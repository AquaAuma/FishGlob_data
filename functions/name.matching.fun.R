#### ----------------------------  ####
#### Author:  Juliano Palacios Abrantes
#### Date first created:  Feb 11, 2021
#### Cescription:
# This script standarizes the taxon list
# for the project and creates a csv file
# to read fom.

#### -------------------------  ####

#### -------------------------  ####
# STEP 1: Function tha creates data
#### -------------------------  ####

call.taxon.names <- function(x){
  
  taxon_correction <- tibble(
    Species = as.factor(c('Dipturus batis',
                          "Dipturus flossada",
                          "Dipturus batis-complex",
                          "Dipturus intermedia",
                          "Liparis montagui",
                          'Liparis liparis',
                          'Liparis liparis liparis',
                          'Chelon aurata',
                          'Chelon ramada',
                          'Mustelus mustelus/asterias',
                          'Mustelus',
                          'Mustelus mustelus',
                          'Mustelus asterias',
                          'Alosa',
                          'Alosa alosa',
                          'Alosa fallax',
                          'Argentina',
                          'Argentinidae',
                          'Argentina silus',
                          'Argentina sphyraena',
                          'Callionymus reticulatus',
                          'Callionymus maculatus',
                          'Ciliata mustela',
                          'Ciliata septentrionalis',
                          'Gaidropsarus',
                          'Gaidropsaurus macrophthalmus',
                          'Gaidropsaurus mediterraneus',
                          'Gaidropsaurus vulgaris',
                          'Sebastes',
                          'Sebastes norvegicus',
                          'Sebastes mentella',
                          'Sebastes marinus',
                          'Syngnathus',
                          'Syngnathus rostellatus',
                          'Syngnathus acus',
                          'Syngnathus typhle',
                          'Nerophis ophidion',
                          'Pomatoschistus',
                          'Pomatoschistus microps',
                          'Pomatoschistus minutus',
                          'Pomatoschistus pictus',
                          'Lesueurigobius',
                          'Gobius cobitis',
                          'Gobius niger',
                          'Leusueurigobius friesii',
                          'Neogobius melanostomus',
                          'Neogobius'
    )),
    corrected_species = as.factor(c(rep('Dipturus',4),
                          rep("Liparis",3),
                          rep('Chelon',2),
                          rep('Mustelus',4),
                          rep('Alosa',3),
                          rep('Argentina',4),
                          rep('Callionymus',2),
                          rep('Ciliata',2),
                          rep('Gaidropsarus',4),
                          rep("Sebastes",4),
                          rep('Syngnatus',5),
                          rep('Pomatoschistus',4),
                          rep('Gobius',6)
    )
  )
  )
  
  return(taxon_correction)
  
}     
