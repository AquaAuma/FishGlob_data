# FishGlob_data 2.0.2

## Overview
This version contains:
* Updated individual survey datasets (e.g., cleaned survey data) for flags and other issues
* Updated the compiled dataset
* Updated documentation on workflow
* Included new documentations (e.g., news)

## New Data Additions
* There were no data additions in this version
* The taxonomic flagging and WORMS calls were updated.

## Improvements
* Created new issues workflow
* Included auto package read for internal functions
* Included missing values in some surveys

## Bug Fixes
* Fixed issues 47 to 74 (excluding 46 and 56)
* Fixed handling of 0 vs NA values in weight/number columns. Some surveys had 0 values for wgt and num based columns where they should have NAs, as described in [issue 47](https://github.com/AquaAuma/FishGlob_data/issues/47).
* Addressed issues with missing columns in certain surveys
* Solved issues of numeric/character csv reading writing. The `haul_id` column is no longer composed of a long string of numerics. Fixes issue that haul_id was incorrectly rounded if loaded from a .csv programmatically in R (with `read_csv()` or `read.csv()`). As documented in [issue #49](https://github.com/AquaAuma/FishGlob_data/issues/49), this led to errors in the `haul_id` column, and could occur regardless of the "class" assigned to this column. 
* Corrections to flagging for spatial-temporal footprint trimming methods, caused by haul_id read-in problems from the previous issue. The change in flags is almost entirely for the spatiotemporal flags, which were almost entirely incorrect or missing for GMEX, NEUS, and WCANN. See pull request [75](https://github.com/fishglob/FishGlob_data/pull/75) for detailed information.

## Methodology Updates

* Updated the clean_taxa() function in response to the taxize package update
* Included auto package load and install for internal fishglob functions

## Documentation
* Updated README with clearer guidance on workflows
* Added a News file to document all issues and changes in each version


# FishGlob_data 2.0.1

* Solved problems in flags for GSL-N.

# FishGlob_data 2.0.0

* This fixed [issue #29](https://github.com/AquaAuma/FishGlob_data/issues/29).
* Norwegian survey was erroneous and was replaced with a Barents Sea centered survey over 2004-onwards which changed the spatio-temporal coverage of the region (coordinated by Laurene Pecuchet with IMR), see [issue #29](https://github.com/AquaAuma/FishGlob_data/issues/29)
