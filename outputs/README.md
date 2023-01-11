## **outputs**

The list of columns, units, and their descriptions are available in [standard_formats](https://github.com/AquaAuma/FishGlob_data/tree/main/standard_formats)

* **Cleaned_data** contains all process survey data files per survey *XX_clean.RData*, with corresponding version with standardization steps *XX_std_clean.RData*
* **Compiled_data** contains the compiled dataset, but not stored onto GitHub because too big of a file
* **Flags/taxonomic_flagging** contains the outputs from the flagging methods for each survey: *XX_flagspp.txt* with the list of flagged taxa, *XX_stats.csv* with the proportion of flagged taxa compared to the total number of taxa in the survey, *XX_taxonomic_flagging.png* with a figure summarizing the occurrence of flagged taxa per year
* **Flags/trimming_method1** contains the outputs from the flagging spatio-temporal method (i) per survey at grid cell resolutions 7 and 8: summarizing files with survey- and method-specific maps, plots, list of hauls removed, and statistics
* **Flags/trimming_method2** contains the outputs from the flagging spatio-temporal method (ii) per survey: summarizing files with survey- and method-specific maps, plots, list of hauls removed, and statistics