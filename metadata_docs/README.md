# Metadata for bottom trawl surveys

Please add notes and descriptions about the surveys, with links to documentation if possible. Surveys are alphabetized by first letter. However, notes about all ICES surveys should be added to the section at the top. 

**Please follow the survey template located at the bottom of this document**


## ICES - DATRAS surveys

### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Swept area: re-estimated partly from linear model for EVHOE, NS-IBTS, NIGFS, IE-IGFS, EVHOE, SWC-IBTS, BITS, IE-IGFS, FR-CGFS, NIGFS, ROCKALL, PT-IBTS, SP-ARSA, SP-PORC, SP-NORTH
- Taxa: fixing temporal wrong identification with expert knowledge & grouping at genus level some species impossible to identify at the species level
- Weight data are re-estimated from abundance at length data, and checked against the reported weight (when reported). Length-weight relationships come from FishBase
- Hauls where not all species are recorded were removed
- See specific surveys for details



## Aleutian Islands - AI

### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_ai.R)
### Related issues
- AI is not sampled at consistent time intervals. Surveys have been conducted 2 to 5 years apart. 


## Baltic Sea - BITS
- Season: Data are collected from two different quarters: quarter 1 and 4
- Gear: BITS has samples with a LOT of gears, some have already been removed, but there are still a few in the fishglob data. If one wants to only use one gear, maybe take the "small" gear


## Canada Hecate Strait - DFO-HS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_dfo-hs.R) 
### Related issues



## Canada Maritimes - DFO-SCS

### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_scs.R) 
- We create a haul_id by combining the mission, stratum, and depth, separated by "_".
- We group the data by haul_id, stratum, stratumarea, year, lat, lon, depth, and spp and then sum up all of the wtcpue values for each group and reassign that as the wtcpue.
- We only keep rows with the season value “SUMMER”
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/mar)



## Canada Gulf of St Lawrence North - GSL-N
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_gsl-n.R) 
-See [here](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/359839.pdf) for helpful guidance on correction for gear and vessel change 
-Requires corrections. CHECK if these have been included



## Canada Gulf of St Lawrence South - GSL-S
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_gsl-s.R) 
- The survey was conducted annually with the same vessel and gear until the mid 1980s. In 1985, the RV Lady Hammond replaced the RV EE Prince
1970-1983 = random stratified, 1984-1987 = fixed station, 1988-present = random stratified.
- There are three different types of trawl used, with different widths and surface area covered during a standard 30 minute tow (EE Princee = Yankee 36, Lady Hammond = Western IIA, Navicula = 50' Flounder)
- See [here](https://publications.gc.ca/collections/collection_2012/mpo-dfo/Fs97-6-2505-eng.pdf) and [here](https://waves-vagues.dfo-mpo.gc.ca/Library/115732.pdf) for helpful guidance on differences in swept areas and corrections applied for gear and vessel changes in 1985 and 1992
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/GSLsouth)



## Canada Queen Charlotte Sound - DFO-QCS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_dfo-qcs.R) 
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/cpac)
### Related issues
- No reported issues



## Canada Strait of Georgia Survey - DFO-SOG
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_dfo-sog.R) 
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/cpac)
### Related issues



## Canada DFO West Coast Haida Gwaii Survey - DFO-WCHG
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_dfo-wchg.R) 
-Our taxonomic naming cleaning process classifies SEBASTES ALEUTIANUS/MELANOSTICTUS COMPLEX as just Sebastes. It is up to the user whether or not they want to sum these observations, they are currently separate rows but with matching accepted_name and haul_id.
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/cpac)
### Related issues



## Canada DFO West Coast Vancouver Island Survey - DFO-WCVI
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_dfo-wcvi.R) 
-One duplicate is maintained because our taxnomic cleaning process directs two verbatim_names (Sebastes and SEBASTES ALEUTIANUS/MELANOSTICTUS COMPLEX Sebastes) to the single accepted name Sebates. It is up to the user to decide how to proceed.
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/cpac)
### Related issues



## Eastern Bering Sea - EBS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_ebs.R) 
### Related issues



## English Channel - FR-CGFS
- There was a change in 2015 when they switched to a new vessel, with subsequent changes in catchability, issue is not fixed at this point, see [issue #6](https://github.com/AquaAuma/FishGlob_data/issues/6)


## Gulf of Alaska - GOA
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_goa.R) 
- Some of the files contain extra headers in the data rows, so we remove any data rows that contain the word “LATITUDE” in the LATITUDE column.
- We create a haulid by combining a 3 digit leading zero vessel number with a 3 digit leading zero cruise number and a 3 digit leading zero haul number, separated by “-”, for example: (vessel-cruise-haul) 354-067-001.
- If wtcpue is recorded as “-9999”, we change the value to NA.
- We remove any SCIENTIFIC spp values that contain the word “egg” or where the only value in the SCIENTIFIC field is white space.
- Any values SCIENTIFIC values that contain the word “Lepidopsetta” are changed to “Lepidopsetta sp.” because more than one genus/spp combo was used to describe the same organism over time. This also holds true for Myoxocephalus sp. excluding scorpius and Bathyraja sp. excluding panthera.
- We group the data by haul_id, year, lat, lon, depth, and spp and then sum up all of the wtcpue values for each group and reassign that as the wtcpue.
-Our taxonomic naming cleaning process classifies Platichthys stellatus X Pleuronectes quadrituberculatus hybrid as just Platichthys stellatus. It is up to the user whether or not they want to sum these observations, they are currently separate rows but with matching accepted_name and haul_id.
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/goa)


## Gulf of Mexico - GMEX
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_gmex.R) 
- Cleaned GMEX contains some duplicates between accepted_name and haul_id, but note that these are not duplicates between verbatim_name and haul_id and therefore have to do with taxonomic cleaning process in [this script](https://github.com/AquaAuma/FishGlob_data/blob/main/functions/clean_taxa.R).
  - Etropus crossotus and Etropus intermedius both fix to Etropus crossotus
  - Monacanthus hispidus, Monacanthus setifer, and Stephanolepis hispida all fix to Stephanolepis hispida
  - Ophidion beani and Ophidion holbrooki both fix to Ophidion holbrookii
  - Anthias tenuis and Anthias tenuis and woodsi both fix to Choranthias tenuis
  - Multiple genuses resolve together (Cynoscion, Bothus, Opsanus)
  -User decisions with what to do with repeats due to taxonomic classifications depend on goals of data use, and therefore are maintained in FishGlob data product
  - See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/gmex)
### Related Issues



## Irish Sea IE-IGFS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
### Related issues



## Northeast US - NEUS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_neus.R) 
- Survey area: The area south of Cape Hatteras (at the southern end of the study domain) is only sampled in a small number of years. 
- Gear: The agency that conducts this survey, the Northeast Fishery Science Center, does not record CPUE either in abundance or weight, or area swept. Thus, it is difficult to calculate CPUE for this region. This is why these columns are NA in FISHGLOB. CPUE can be estimated by trimming out tows that are not close to 30min in duration, calculating `wgt_h` or `num_h` per hour by multiplying the `wgt` or `num` columns by 2, and calculating CPUE (`wgt_cpue` and `num_cpue`) by dividing the `wgt` or `num` columns by 0.0384 (the average area swept in km^2 as per NOAA staff). 
- Gear: There was a gear change in 2009 in NEUS. This tech memo describes calibration factors for many (but not all) species, which we apply as part of the data processing: Miller, T. J., C. Das, P. J. Politis, A. S. Miller, S. M. Lucey, C. M. Legault, R. W. Brown, and P. J. Rago. “Estimation of Albatross IV to Henry B. Bigelow Calibration Factors. Northeast Fisheries Science Center Reference Document 10-05.” Woods Hole, MA: US. Department of Commerce, National Marine Fisheries Service, Northeast Fisheries Science Center, 2010.
- Before 2020, we emailed a staff member at NOAA with a data request and recieved a RData file. This file was a combination of the SVBIO, SVCAT, and SVSTA files and some column names were changed. Now we download the files from the publicly available data set. We combine those files and change the column names to match the column names we used to receive so that subsequent code will work. The changes include changing EST_YEAR to YEAR, changing DECDEG_BEGLAT to LAT, DECDEG_BEGLON to LON, AVGDEPTH to DEPTH, EXPCATCHWT to BIOMASS.
- There are some commas and special characters in the svcat.csv files that cause them to parse incorrectly. We import those files with read_lines, remove the commas and special characters from the comments, and proceed to read them into R as .csvs.
- We group the data by YEAR, SEASON, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM, and SVSPP and sum the BIOMASS (which is reported by sex) to calculate wtcpue.
- We create a haulid by combining a 6 digit leading zero cruise number with a 3 digit leading zero station number and a 4 digit leading zero stratum number, separated by “-”, for example: (cruise-station-stratum) 456354-067-0001.
- We convert square nautical miles to square kilometers.
- We remove any SCINAME spp values that contain the word “egg” or “unidentified”, or where the only value in the SCINAME field is white space.
- We group the data by haul_id, stratum, stratumarea, year, lat, lon, depth, and spp and then sum up all of the wtcpue values for each group and reassign that as the wtcpue.
- We separate the trawls into Fall and Spring seasons.
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/neus)



## Irish Sea NIGFS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
### Related Issues



## Norway NOR-BTS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_norway.R) 
- Gear: many inappropriate gears were removed based on expertise from IMR colleagues. Kept only the "shrimp trawl gears" (3236: Campelen 1800 shrimp trawl with 35 mm mesh Reketrål. Campelen 1800 ma 35 mm m/40 m. sveiper, Rockhopper gear (Standard sampling-trål); 3270: Campelen 1800 shrimp trawl with 22mm mesh size. Reketrål. Campelen 1800 ma 20 mm m/40 m sveiper. Rockhopper gear. ; 3271: like 3270 with strapping Reketrål. Campelen 1800 ma 20 mm m/40 m sveiper. Rockhopper gear, strapping.)
- Swept area: swept areas re-estimated based on linear models and sampling characteristics
- Weight data are re-restimated from abundance at length data
- Changes in the design of the survey in 2004, especially changes of season sampled, this can cause inconsistencies in time-series analysis. Often better to use timse-series before/after 2004 and/or to be careful of which surveyed months are kept in the analysis
- This dataset is a collection of many coastal and offshore surveys, to target the IMR summer ecosystem survey, one can use the filters: Gear%in%c("3270","3271") & ShootLat>70 & Month%in%c(8,9)



## North Sea - NS-IBTS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Community sampling was standardized in 1983. Before that, surveys are not reliable for community related questions.
- Time-series: Community sampling was standardized in 1983. Before that, surveys are not reliable for community related questions.
- Season: Data are collected in two different quarters: quarter 1 and 4
- Gear: removed a bunch of gears that are not GOV (Grande Ouverture Verticale)



## PT-IBTS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Gear: removing one sampling gear
- there is no sampling in 2012, 2018, 2019
- PT-IBTS is surveyed annually, but missing 2012 data, and there is one year of data (2002) before the annual survey starts in 2005.
- Taxa: the total number of taxa per year is low, likely not all taxa are included in this dataset



## Rockall - ROCKALL
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Gear: there have been some gear/design changes for that survey in the recent years (after after 2010), but it's minor enough we don't need to correct, unless we observe an important change 2010+
###Related Issues



## Souhteast US  - SEUS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_seus.R) 
### Related Issues



## Scottish West Coast - SWC-IBTS
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Season: Data are collected from two different quarters: quarter 1 and 4
- Gear: there have been some gear/design changes for that survey in the recent years (after after 2010), but it's minor enough we don't need to correct, unless we observe an important change 2010+



## Northern Spanish Coast - SP-NORTH
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Hauls and length data: some hauls include length data that does not seem realistic and were removed from the dataset (872 hauls)



## Spanish survey Gulf of Cadiz - SP-ARSA
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R) 
- Taxa: total number of taxa seems low, maybe not all taxa are reported in this dataset
- Hauls and length data: some hauls include length data that does not seem realistic and were removed from the dataset (14 hauls)



## Spanish Porcupine Bank - SP-PORC
### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_datras.R)



## West Coast Triennial - WCTRI
- Time-series: The annual West Coast survey started in 2003, but the triennial survey was repeated in 2004, so there are two West Coast surveys in 2004 with different spatial footprints and methodologies. 

### Data cleaning process [script](https://www.nwfsc.noaa.gov/data/api/v1/source)
- Munro, P. T. 1998. A decision rule based on the mean square error for correcting relative fishing power differences in trawl survey data. Fish. Bull. 96:538-546.
- Helser, Thomas, André Punt, and Richard Methot. 2004. “A Generalized Linear Mixed Model Analysis of a Multi-Vessel Fishery Resource Survey.” Fisheries Research 70 (December): 251–64
- Cooper, Andrew B., Andrew A. Rosenberg, Gunnar Stefánsson, and Marc Mangel. 2004. “Examining the Importance of Consistency in Multi-Vessel Trawl Survey Design Based on the U.S. West Coast Groundfish Bottom Trawl Survey.” Fisheries Research, Models in Fisheries Research: GLMs, GAMS and GLMMs, 70 (2): 239–50.
- The Northwest Fisheries Science Center’s West Coast Groundfish Bottom Trawl Survey: History, Design, and Description
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/wctri)

### Related issues



## West Coast Annual - WCANN

### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_wcann.R)
- Time-series: This survey started in 2003, but the West Coast Triennial survey was repeated in 2004, so there are two West Coast surveys in 2004 with different spatial footprints and methodologies. 
- A few rockfish (Sebastes sp.) duplicates are maintained as independent observations. Our taxnomic cleaning process changes Sebastes sp. (miniatus / crocotulus) and Sebastes sp. (aleutianus / melanostictus) to Sebastes (same accepted name, different verbatim names). End user can decide if they want to merge these observations and therefore sum abundance/weight.
- We create a “strata” value by using lat, lon and depth to create a value in 100m bins.
- We calculate a wtcpue value with the units kg per hectare (10,000 m2) by dividing total_catch_wt_kg by area_swept_ha_der.
- We remove any SPECIES_NAME spp values that contain the word “egg” or where the only value in the SPECIES_NAME field is white space.
- Any values SPECIES_NAME values that contain the word “Lepidopsetta” are changed to “Lepidopsetta sp.” because more than one genus/spp combo was used to describe the same organism over time. This also holds true for Bathyraja sp.
- We group the data by haulid, stratum, stratumarea, year, lat, lon, depth, and spp and then sum up all of the wtcpue values for each group and reassign that as the wtcpue.
- See OceanAdapt for more info: [OceanAdapt](https://github.com/pinskylab/OceanAdapt/tree/master/metadata/wcann)

### Related issues


----------

# Survey Template

Use the template below to add a new survey description. Please copy and paste it, do not remove the template.

## Survey Name - Survey ISO

### Data cleaning process [script](https://github.com/AquaAuma/FishGlob_data/blob/main/cleaning_codes/get_SURVEY_headings.R) 

- Description A
- Description B
- Description C
.
.
.
- Description N

### Related issues

- [#X Name of Issue](link to the github issue)
- [#Y Name of Issue](link to the github issue)
