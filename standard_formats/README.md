# **standard_formats**

includes definitions of file formats in the FishGlob database, including survey ID codes.
* *survey_units.xlsx* details all surveys that need to be separated by seasons or quarter because sampling occurs more than once a year
* *Surveys_ID.xlsx* details of survey codes per region
* *fishglob_data_columns.xlsx* details all column names used in the XX_clean.RData files with units and description
* *fishglob_data_columns_std.xlsx* similar but also including description of flagging columns

## survey_ID
| ID | Area | Responsible |
| --- | --- | --- |
| `AI` | Aleutian Islands | National Oceanic and Atmospheric Administration (NOAA) |
| `BITS` | Baltic Sea | ICES |
| `FR-CGFS` | English Channel | ICES |
| `DFO-HS` | Canada, Hecate Strait | Department of Fisheries and Oceans (DFO) |
| `DFO-SOG` | Canada, Strait of Georgia | Department of Fisheries and Oceans (DFO) |
| `DFO-QCS` | Canada, Queen Charlotte | Department of Fisheries and Oceans (DFO) |
| `DFO-WCHG` | Canada, West Coast Haida Gwaii | Department of Fisheries and Oceans (DFO) |
| `DFO-WCVI` | Canada, West Coast Vancouver Island | Department of Fisheries and Oceans (DFO) |
| `EBS` | Eastern Bering Sea | National Oceanic and Atmospheric Administration (NOAA) |
| `EVHOE` | Bay of Biscay | ICES |
| `GMEX` | Gulf of Mexico | Ocean Adapt |
| `GOA` | Gulf of Alaska | National Oceanic and Atmospheric Administration (NOAA) |
| `GSL-N` | Northern Gulf of St Lawrence | Department of Fisheries and Oceans (DFO) |
| `GSL-S` | Southern Gulf of St Lawrence | Nicolas Rolland- Department of Fisheries and Oceans (DFO) |
| `IE-IGFS` | Irish Sea | ICES |
| `NEUS` | Northeast US | Ocean Adapt |
| `NIGFS` | Northern Ireland | ICES |
| `NOR-BTS` | Norway | IMR |
| `NS-IBTS` | North Sea | ICES |
| `PT-IBTS` | Portugal | ICES |
| `ROCKALL` | Rockall Plateau | ICES |
| `SCOROC` | Rockall Plateau | ICES |
| `SCOWCGFS` | Scotland Shelf Sea | ICES |
| `SCS` | Scotian Shelf | Ocean Adapt (Canadian Department of Fisheries and Oceans (DFO)) |
| `SEUS` | Southeast US | Ocean Adapt |
| `SWC-IBTS` | Scotland Shelf Sea | ICES |
| `WCTRI` | California Current (Trienniall) | National Oceanic and Atmospheric Administration (NOAA) |
| `WCANN` | California Current (Annual) | National Oceanic and Atmospheric Administration (NOAA) |

## fishglob_data_columns_std
| Column name fishglob | Unit | Type | Description |
| --- | --- | --- | --- |
| `survey` | — | character | Survey codes |
| `source` | — | character | Institutions names or acronyms |
| `timestamp` | — | character | Datasets version from source |
| `haul_id` | — | character | Unique ID created as Survey/Year/Quarter/Country/Ship/Gear/StNo/HaulNo for ICES surveys or any other unique combination including long/lat |
| `country` | — | character | Country names |
| `sub_area` | — | character_integer | relevant for some surveys, such as MEDITS where GSA (e.g. GSA7). If not applicable for some surveys: NA |
| `continent` | — | character | modalities: europe, n_america, s_america, asia, africa, oceania, arctic, antarctica |
| `stat_rec` | — | character | ICES rectangles (only works for Datras surveys) |
| `station` | — | character | Sampling station, NA is not available |
| `stratum` | — | character | Stratum of sampling (only works for US/Canadian?) |
| `year` | — | integer | year of sampling |
| `month` | — | integer | month of sampling, from 1 to 12 |
| `day` | — | integer | day of sampling |
| `quarter` | — | integer | 1 to 4, 1=January to March, 2=April to June, etc. |
| `season` | — | character | Winter, Spring, Summer, Automn |
| `latitude` | degrees, Geographic Coordinate System, WGS84 | numeric | haul latitude, ideally at begining of sampling |
| `longitude` | degrees, Geographic Coordinate System, WGS84 | numeric | haul longitude, ideally at begining of sampling |
| `haul_dur` | hours | numeric | haul duration |
| `area_swept` | km2 | numeric | haul swept area, NA if not available |
| `gear` | — | character | gear used for sampling, NA if not available |
| `depth` | m | numeric | sampling depth, ideally at begining of sampling |
| `sbt` | degrees C | numeric | sampling sea bottom temperature |
| `sst` | degrees C | numeric | sampling sea surface temperature |
| `num` | number of individuals | numeric | number of individuals sampled |
| `num_cpue` | number of ind./hour | numeric | num/haul_dur |
| `num_cpua` | number of ind./km2 | numeric | num/area_swept |
| `wgt` | kg | numeric | weight of sampled individuals |
| `wgt_cpue` | kg/hour | numeric | wgt/haul_dur |
| `wgt_cpua` | kg/km2 | numeric | wgt/area_swept |
| `verbatim_name` | — | character | name from the data received |
| `verbatim_aphia_id` | — | character | aphia id from the data received |
| `accepted_name` | — | character | accepted scientific name of the taxa (verified with WoRMS) |
| `aphia_id` | — | character | Worms AphiaID valid name code |
| `SpecCode` | — | numeric | fishbase species code |
| `kingdom` | — | character | Worms kingdom |
| `phylum` | — | character | Worms phylum |
| `class` | — | character | Worms class |
| `order` | — | character | Worms order |
| `family` | — | character | Worms family |
| `genus` | — | character | Worms genus |
| `rank` | — | character | Worms rank |
| `survey_unit` | — | character | combination of survey with quarter or season (useful for BITS, NS-IBTS, SWC-IBTS, NEUS, SEUS, SCS, GMEX) |
| `flag_trimming_hex7_0` | — | character | TRUE if haul is flagged, NA if not |
| `flag_trimming_hex7_2` | — | character | TRUE if haul is flagged, NA if not |
| `flag_trimming_hex8_0` | — | character | TRUE if haul is flagged, NA if not |
| `flag_trimming_hex8_2` | — | character | TRUE if haul is flagged, NA if not |
| `flag_trimming_2` | — | character | TRUE if haul is flagged, NA if not |
| `flag_taxa` | — | character | TRUE if haul is flagged, NA if not |
