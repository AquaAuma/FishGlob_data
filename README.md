# FishGlob_data

[![DOI](https://zenodo.org/badge/580133169.svg)](https://zenodo.org/badge/latestdoi/580133169)

This repository contains the FishGlob database, including the methods to load, clean, and process the public bottom trawl surveys in it. The database is described in the manuscript, "An integrated database of fish biodiversity sampled with scientific bottom trawl surveys" by Aurore A. Maureaud, Juliano Palacios-Abrantes, Zoë Kitchel, Laura Mannocci, Malin L. Pinsky, Alexa Fredston, Esther Beukhof, Daniel L. Forrest, Romain Frelat, Maria L.D. Palomares, Laurene Pecuchet, James T. Thorson, P. Daniël van Denderen, and Bastien Mérigot.

This database is a product of the CESAB working group, [FishGlob: Fish biodiversity under global change – a worldwide assessment from scientific trawl surveys](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/fishglob/).

<img src ="https://github.com/AquaAuma/FishGlob_data/blob/main/fishglob_logo.png" width ="200">

Main contacts: Aurore A. Maureaud [aurore.aqua@gmail.com](mailto:aurore.aqua@gmail.com),  Juliano Palacios-Abrantes [j.palacios@oceans.ubc.ca ](mailto:j.palacios@oceans.ubc.ca), and Malin L. Pinsky [malin.pinsky@rutgers.edu](mailto:malin.pinsky@rutgers.edu)

### Structure of the repository

* **cleaning_codes** includes all scripts to process and perform quality control on the trawl surveys.
* **data_descriptor_figures** contains the R script to construct figures 2-4 for the data descriptor manuscript. 
* **functions** contains useful functions used in other scripts
* **length_weight** contains the length-weight relationships for surveys where weights have to be calculated from abundance at length data (including NOR-BTS and DATRAS)
* **metadata_docs** has a README with notes about each survey. This is a place to document changes in survey methods, quirks, etc. It is a growing list. If you have information to add, please open an Issue.
* **outputs** contains all survey data processed .RData files and flagging outputs
* **standard_formats** includes definitions of file formats in the FishGlob database, including survey ID codes.
* **standardization_steps** contains the R codes to run a full survey standardization and a cross-survey summary of flagging methods
* **summary** contains the quality check plots for each survey

### Survey data processing steps

Data processing and cleaning is done on a per survey basis unless formats are similar across a group of surveys. The current repository can process 26 scientific bottom-trawl surveys, according to the following steps.

**Steps** 
1. Merge the data files for one survey
2. Clean & homogenize column names following the format described in *standard_formats/fishglob_data_columns.xlsx*
3. Create missing columns and standardize units using the standard format *standard_formats/fishglob_data_columns.xlsx*
4. Integrate the cleaned taxonomy by applying the function *clean_taxa()* and apply expert knowledge on taxonomic treatments
5. Perform quality checks, including the output in the *summary* folder

### Survey data standardization and flags

Data standardization and flags are done on a per survey basis and per survey_unit basis (integrating seasons and quarters). Flags are performed both on the temporal occurrence of taxa and the spatio-temporal sampling footprint according to the following steps.

**Steps**
1. Taxonomic quality control: run flag_spp() for each survey region
2. Apply methods to identify a standard spatial footprint through time for each survey-season/quarter (the survey_unit column). Use the functions apply_trimming_per_survey_unit_method1() and apply_trimming_per_survey_unit_method2() 
3. Display and integrate results in the summary files

### Final data products

**Options**
Users can either use the single survey data products in **outputs/Cleaned_data/** and work with survey .RData files including flags or not (inclusion of flags is specified by XX_std_clean.RData), or generate their own compiled version of the data by running the **cleaning_codes/merge.R** which will write local versions of the database in **outputs/Compiled_data/**

### Author contributions
*Contributors to code*
- **Cleaning taxonomy**: Juliano Palacios-Abrantes 
- **Cleaning surveys**: Juliano Palacios-Abrantes, Aurore Maureaud, Zoë Kitchel, Dan Forrest, Daniel van Denderen
- **Summary of surveys**: Juliano Palacios-Abrantes, Aurore Maureaud, Zoë Kitchel, Laura Mannocci
- **Merge surveys**: Aurore Maureaud
- **Standardize surveys**: Laura Mannocci, Malin Pinsky, Aurore Maureaud, Zoë Kitchel, Alexa Fredston
