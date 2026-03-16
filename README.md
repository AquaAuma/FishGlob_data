
# FishGlob_data

[![DOI](https://zenodo.org/badge/580133169.svg)](https://zenodo.org/badge/latestdoi/580133169)

<img src ="https://github.com/FishGlob/FishGlob_data/blob/main/fishglob_logo.png" width ="200">

This repository contains the FishGlob database. Its purpose is to understand the status and trends of marine ecosystems. The repository includes the methods to load, clean, and process 29 publicly available bottom trawl surveys from Europe and North America, plus the final database. This database is a product of the CESAB working group, [FishGlob: Fish biodiversity under global change – a worldwide assessment from scientific trawl surveys](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/fishglob/). For more information, please contact [fishglobconsortium@gmail.com](fishglobconsortium@gmail.com).

### Credit and citation

Our full citation policy is described in the [Fishglob_data disclaimer](https://docs.google.com/document/d/1uiEIcUugCf-dOSvio6hB1r8xFf0sm1Ip2IzjbMu9I4o/).

[![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

### Using the data products

To use these data, please:
- Read about the dataset in [Maureaud *et al.* 2024](https://www.nature.com/articles/s41597-023-02866-w)
- Read our data disclaimer and citation policy in the [Fishglob_data disclaimer](https://docs.google.com/document/d/1uiEIcUugCf-dOSvio6hB1r8xFf0sm1Ip2IzjbMu9I4o/)
- Read the [survey-specific metadata](https://github.com/FishGlob/FishGlob_data/tree/main/metadata_docs)
- Consider doing a [tutorial](https://fishglob.sites.ucsc.edu/training/)
- Either:
  - Use a compiled version of the data in **[outputs/Compiled_data/](outputs/Compiled_data)** or create your own by running the **cleaning_codes/merge.R**; or
  - Use the single survey data products in **[outputs/Cleaned_data/](outputs/Cleaned_data)**.

### Structure of the FishGlob_data repository

* **[cleaning_codes](cleaning_codes)** includes all scripts to process and perform quality control on the trawl surveys.
* **[data_descriptor_figures](data_descriptor_figures)** contains the R script to construct figures 2-4 for the data descriptor manuscript. 
* **[functions](functions)** contains useful functions used in other scripts
* **[length_weight](length_weight)** contains the length-weight relationships for surveys where weights have to be calculated from abundance at length data (including NOR-BTS and DATRAS)
* **[metadata_docs](metadata_docs)** has a README with notes about each survey. This is a place to document changes in survey methods, quirks, etc. It is a growing list. If you have information to add, please open an Issue.
* **[outputs](outputs)** contains all survey data processed .RData files and flagging outputs
* **[QAQC](QAQC)** contains the additional QAQC performed on surveys that required supplementary checks (DATRAS-sourced surveys)
* **[raw_data](raw_data)** has some of the raw data files that are public but cannot be easily obtained elsewhere
* **[standard_formats](standard_formats)** includes definitions of file formats in the FishGlob database, including survey ID codes.
* **[standardization_steps](standardization_steps)** contains the R codes to run a full survey standardization and a cross-survey summary of flagging methods
* **[summary](summary)** contains QAQC plots for each survey

### Survey data processing steps

Data processing and cleaning is done on a per survey basis unless formats are similar across a group of surveys. The current repository can process 29 scientific bottom-trawl surveys, according to the following steps.

*Survey data processing steps*
1. Merge the data files for one survey
2. Clean & homogenize column names following the format described in *standard_formats/fishglob_data_columns.xlsx*
3. Create missing columns and standardize units using the standard format *standard_formats/fishglob_data_columns.xlsx*
4. Integrate the cleaned taxonomy by applying the function *clean_taxa()* and apply expert knowledge on taxonomic treatments
5. Perform quality checks, including the output in the *summary* folder and specific QAQC for other surveys detailed in the QAQC folder

### Survey data standardization and flags

Data standardization and flags are done on a per survey basis and per survey_unit basis (integrating seasons and quarters). Flags are performed both on the temporal occurrence of taxa and the spatio-temporal sampling footprint according to the following steps.

*Survey data standardization and flagging steps*
1. Taxonomic quality control: run flag_spp() for each survey region
2. Apply methods to identify a standard spatial footprint through time for each survey-season/quarter (the survey_unit column). Use the functions apply_trimming_per_survey_unit_method1() and apply_trimming_per_survey_unit_method2() 
3. Display and integrate results in the summary files

### Author contributions
We thank (in alphabetical order) Esther Beukhof, Daniël van Denderen, Daniel Forrest, Alexa Fredston, Zoë Kitchel, Laura Mannocci, Aurore Maureaud, Juliano Palacios-Abrantes, Laurene Pecuchet, Malin Pinsky, and Michelle Stuart for their work cleaning, summarizing, merging, standardizing, and providing QAQC on survey data.

### Updates policy

The FISHGLOB Steering Committee aims to update this database approximately once a year to incorporate additional data from included surveys and to improve the data pipeline. [Releases](https://github.com/fishglob/FishGlob_data/releases) with major data updates (new survey regions, new data structure) will have a new version number. Smaller updates (e.g., new years in existing surveys) get a new minor version (1.1). Bug fixes get a new patch number (1.1.1). Updates are made on separate branches or forks, then a pull request with clear documentation is made to the dev branch. A member of the Steering Committee (someone not involved in the original coding) will review the change and approve pull requests. The version number will get incremented (either major, minor, or patch) when the dev branch is pulled to the main branch and a new release is made.

See our [NEWS](NEWS.md) file.

### Community involvement 

- *Do you want to connect with other members of our community?* Check out our [Discussion forum](https://github.com/fishglob/FishGlob_data/discussions/) where you can share ideas, possible developments and collaborations, as well as any other topics related to FISHGLOB.
- *Found a bug in the code?* Please open an [issue](https://github.com/fishglob/FishGlob_data/issues) so we can investigate. Clear descriptions and suggested solutions are helpful. Contributions and suggested fixes are warmly encouraged by forking this repository, making the change, and making a pull request.
- *Want to request specific changes or enhancements?* Please open an [issue](https://github.com/fishglob/FishGlob_data/issues) so we and the broader community can concider it in our future updates. Contributions are warmly encouraged by forking this repository, making the change, and making a pull request.
- *Want to learn more about FISHGLOB?* Check out our [website](https://fishglob.sites.ucsc.edu/), https://fishglob.sites.ucsc.edu/
