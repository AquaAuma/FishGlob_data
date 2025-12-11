# FishGlob_data 2.0.2

* Note that there has been a change in the number of flags from previous versions to the current compiled data. The change in flags is almost entirely for the spatiotemporal flags, which were almost entirely incorrect or missing for GMEX, NEUS, and WCANN. See pull request [75](https://github.com/fishglob/FishGlob_data/pull/75) for detailed information.
* Fixes issue that some surveys had 0 values for wgt and num based columns where they should have NAs, as described in [issue 47](https://github.com/AquaAuma/FishGlob_data/issues/47).
* The `haul_id` column is no longer composed of a long string of numerics. Fixes issue that haul_id was incorrectly rounded if loaded from a .csv programmatically in R (with `read_csv()` or `read.csv()`). As documented in [issue #49](https://github.com/AquaAuma/FishGlob_data/issues/49), this led to errors in the `haul_id` column, and could occur regardless of the "class" assigned to this column. 

# FishGlob_data 2.0.1

* Solved problems in flags for GSL-N.

# FishGlob_data 2.0.0

* This fixed [issue #29](https://github.com/AquaAuma/FishGlob_data/issues/29).
* Norwegian survey was erroneous and was replaced with a Barents Sea centered survey over 2004-onwards which changed the spatio-temporal coverage of the region (coordinated by Laurene Pecuchet with IMR), see [issue #29](https://github.com/AquaAuma/FishGlob_data/issues/29)
