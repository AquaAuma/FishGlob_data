## **outputs/Compiled_data**

By using the **cleaning_codes/merge.R** R script, the function *read_clean_data()* and *write_clean_data()* will store compiled versions of the database in this folder. The generated files include a data file and a readme file including the columns names, units, and their descriptions.

Three versions of the compiled datasets are created:
* the metadata, unique combination of survey, latitude, longitude, survey_unit, year
* the compiled dataset
* the compiled dataset with flags

The formats generated are:
* an .RData file easy to use on R
* a csv.gz file easy to use outside of R (compressed .csv)