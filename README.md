# fishies
Analysis of environmental and anthropogenic drivers of fish occurrence across life history strategy in the North Eastern Atlantic

There are two sections to this work 
  - cleaning and standardising survey effort (scripts 1 - 7)
  - cleaning and standardising biodiversity data (scripts 8 - 12)

Code is adapted from work by Meadhbh Moriarty in 2016 and built on by Ruth Kelly 2021.

In general the goals of the scripts in series are outline below:

Goal 1. Check survey data for outliers and query with data providers. This code has been re-run to insure all errors that have been 
            reported have been removed from DATRAS, otherwise a "fix" is used
            
Goal 2. Derive Surveys and fix errors not updated in DATRAS

Goal 3. Estimate missing/incorrect gear parameters

Goal 4. Calculate Swept Area and Swepth Volume

Goal 5. Estimate missing/incorrect biological parameters

Goal 6. Calculate Swept Area densities

## DATA NEEDED:

# DATRAS DOWNLOAD 04/2022

R version 3.6.3 "Holding the Windsock" 


The DATRAS data provides us with a response variable (fish occurrence or abundance, and maybe some tratis)

We must search the internet for our co-variates



# Fishing pressure

EMODnet Vessel density EU
- https://www.emodnet-humanactivities.eu/search-results.php?dataname=Vessel+Density+#ID0EAEA
One year one month = 30 MB
can include "service" vessels and underwater opperations vessels

Global fish watch
- https://globalfishingwatch.org/data-download/datasets/public-fishing-effort

# Sea surface temperature

COPERNICUS - Sea surface temperature daily data from 1981 to present derived from satellite observations
https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-surface-temperature?tab=form
one  year one month all days = 511 MB

Ocean colour NASA - sea surface temperature from 2002 - 2022
https://oceancolor.gsfc.nasa.gov/l3/


Offshore infrastructure 
pH?
Green index (primary productivity)
Substrate 

in DATRAS dataset
 - depth
 


