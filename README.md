# integrated_fishglob_surveys

[![DOI](https://zenodo.org/badge/580133169.svg)](https://zenodo.org/badge/latestdoi/580133169)

This repository contains the FishGlob s database, including the methods to load, clean, and process the public bottom trawl surveys in it. The database is described in the manuscript, "An integrated database of fish biodiversity sampled with scientific bottom trawl surveys" by Aurore A. Maureaud, Juliano Palacios-Abrantes, Zoë Kitchel, Laura Mannocci, Malin L. Pinsky, Alexa Fredston, Esther Beukhof, Daniel L. Forrest, Romain Frelat, Maria L.D. Palomares, Laurene Pecuchet, James T. Thorson, P. Daniël van Denderen, and Bastien Mérigot.

This database is a product of the CESAB working group, [FishGlob: Fish biodiversity under global change – a worldwide assessment from scientific trawl surveys](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/fishglob/).

<img src ="https://github.com/AquaAuma/integrated_fishglob_surveys/blob/main/fishglob_logo.png" width ="200">

Main contact: Aurore A. Maureaud [aurore.aqua@gmail.com](mailto:aurore.aqua@gmail.com)

### Structure of the repository

* **cleaning._odes** includes all scripts to process and perform quality control on the trawl surveys.
* **data_descriptor_figures** contains the R script to construct figures 2-4 for the data descriptor manuscript. 
* **functions** contains useful functions used in other scripts
* **length_weight** contains the length-weight relationships for surveys where weights have to be calculated from abundance at length data (including NOR-BTS and DATRAS)
* **metadata_docs** has a README with notes about each survey. This is a place to document changes in survey methods, quirks, etc. It is a growing list. If you have information to add, please open an Issue.
* **outputs/Cleaned_data** contains all process survey data files per survey *XX_clean.RData*, with corresponding version with standardization steps *XX_std_clean.RData*
* **outputs/Compiled_data** contains the compiled dataset, but not stored onto GitHub because too big of a file
* **outputs/Flags/taxonomic_flagging** contains the outputs from the flagging methods for each survey: *XX_flagspp.txt* with the list of flagged taxa, *XX_stats.csv* with the proportion of flagged taxa compared to the total number of taxa in the survey, *XX_taxonomic_flagging.png* with a figure summarizing the occurrence of flagged taxa per year
* **outputs/Flags/trimming_method1** contains the outputs from the flagging spatio-temporal method (i) per survey at grid cell resolutions 7 and 8: summarizing files with survey- and method-specific maps, plots, list of hauls removed, and statistics
* **outputs/Flags/trimming_method2** contains the outputs from the flagging spatio-temporal method (ii) per survey: summarizing files with survey- and method-specific maps, plots, list of hauls removed, and statistics
* **standard_formats** includes definitions of file formats in the FishGlob database, including survey ID codes.
* **standardization_steps** contains the R codes to run a full survey standardization and a cross-survey summary of flagging methods
* **summary** contains the quality check plots for each survey

### Survey data cleaning steps
*add an overall description here*

**Steps** 
1. Merge the data files for one survey
2. Clean & homogenize column names following the format described in *standard_formats/fishglob_data_columns.xlsx*
3. Create missing columns and standardize units using the standard format *standard_formats/fishglob_data_columns.xlsx*
4. Integrate the cleaned taxonomy by merging old names with new clean name created in *taxa_data_routine.Rmd*
5. Perform quality checks, including the output in the *summary* folder
6. Merge across surveys with the merge.R code

### Survey data standardization and flags
*add an overall description here*

**Steps**

1. Taxonomic quality control: run flag_spp() for each survey region
2. Apply methods to identify a standard spatial footprint through time for each survey-season/quarter (the survey_unit column). Use the functions apply_trimming_per_survey_unit_method1() and apply_trimming_per_survey_unit_method2() 
3. Display and integrate results in the summary files

### Author contributions
*Contributors to code*
- **Cleaning taxonomy**: Juliano 
- **Cleaning surveys**: Juliano, Aurore, Zoë, Dan
- **Summary of surveys**: Juliano, Aurore, Zoë, Laura
- **Merge surveys**: Aurore
- **Standardize surveys**: Laura, Malin, Aurore, Zoë, Alexa
