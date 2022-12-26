#### ----------------------------  ####
#### Author:  Juliano Palacios Abrantes
#### Date first created:  Feb 11, 2021
#### Cescription:
# This script filters out specific data
# according to a list of species, the
# specific survey and the BycSpecRecCode

#### -------------------------  ####

#### -------------------------  ####
# STEP 1: List of species
#### -------------------------  ####

spp_BycSpecRecCode_0  <- c('Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                           'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')

spp_BycSpecRecCode_2 <- c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                          'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus','Lumpenus lampretaeformis',
                          'Mullus surmuletus','Squalus acanthias','Trachurus trachurus',
                          'Platichthys flesus','Pleuronectes platessa','Limanda limanda','Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                          'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus','Scophthalmus rhombus','Solea solea',
                          'Pollachius virens','Pollachius pollachius','Trisopterus luscus','Trisopterus minutus','Micromesistius poutassou','Molva molva',
                          'Merluccius merluccius','Brosme brosme','Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                          'Merlangius merlangus','Trisopterus esmarkii')

spp_BycSpecRecCode_3 <- c('Pollachius virens','Pollachius pollachius','Trisopterus luscus','Trisopterus minutus','Micromesistius poutassou','Molva molva',
                          'Merluccius merluccius','Brosme brosme','Clupea harengus','Sprattus sprattus',
                          'Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')

spp_BycSpecRecCode_4 <-  c('Platichthys flesus','Pleuronectes platessa','Limanda limanda','Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                           'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus','Scophthalmus rhombus','Solea solea',
                           'Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                           'Merlangius merlangus','Trisopterus esmarkii')

spp_BycSpecRecCode_5 <- c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                          'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus','Lumpenus lampretaeformis',
                          'Mullus surmuletus','Squalus acanthias','Trachurus trachurus','Clupea harengus',
                          'Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                          'Merlangius merlangus','Trisopterus esmarkii')

#### -------------------------  ####
# STEP 2: Function to filter all levels
#### -------------------------  ####

filter_out_fun <-  function(data){
  
  
  out_data <- data %>% 
    filter(!(BycSpecRecCode==0 & Survey=='NS-IBTS' & !Species %in% spp_BycSpecRecCode_0),
           !(BycSpecRecCode==2 & !Species %in% spp_BycSpecRecCode_2),
           !(BycSpecRecCode==3 & !Species %in% spp_BycSpecRecCode_3),
           !(BycSpecRecCode==4 & !Species %in% spp_BycSpecRecCode_4),
           !(BycSpecRecCode==5 & !Species %in% spp_BycSpecRecCode_5)
           )

  
  return(out_data)
}
