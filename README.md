# fishies
Analysis of environmental and anthropogenic drivers of fish occurrence across life history strategy in the North Eastern Atlantic

R version 3.6.3 "Holding the Windsock" 

and

R version 4.2.0 "Vigorous Calestetics" (for using the rfishbase package, then back to 3.6.3 for using raster)

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

## scripts we use

Haul data cleaning: 1 - 7

Biological data cleaning: 8a & 8

Covariate wrangling: 13


We are subsetting all available DATRAS data to just surveys in the North East Atlantic (i.e. not the North sea) in order to establish our modelling pipeline. We would currently 
like to expand to modelling all available data if this goes well (i.e. after Caroline finished her PhD).

Covariates will be modelled at 10km2 spatial and mean monthly values temporal resolution.

The DATRAS data provides us with a response variable (fish occurrence or abundance, and maybe some tratis)

We must search the internet for our co-variates

# Fishing pressure

# OSPAR spatial data layers of fishing intensity/pressure - oh my god why was that so incredibly hard to find on the internet
2009 - 2020
https://ices-library.figshare.com/articles/dataset/Data_for_OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18601508


EMODnet Vessel density EU
- https://www.emodnet-humanactivities.eu/search-results.php?dataname=Vessel+Density+#ID0EAEA
One year one month = 30 MB
can include "service" vessels and underwater opperations vessels

Global fish watch
- https://globalfishingwatch.org/data-download/datasets/public-fishing-effort

# Sea surface temperature

Ocean colour NASA - sea surface temperature from 2002 - 2022
https://oceancolor.gsfc.nasa.gov/l3/

can extract seasonal SST (as per Lousie Rutterford) at 9km resolution within boundary box of your choosing
nc files (easily rasterised)
1 season of data for my study area = approx 240KB. 240*4*13 = 12.5 MB total (they also send nighttime sst so double that) 
WAY better than copernicus for my purposes


# COPERNICUS - Sea surface temperature daily data from 1981 to present derived from satellite observations
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-surface-temperature?tab=form
# one  year one month all days = 511 MB




Offshore infrastructure (not today)
pH?
Green index (primary productivity)
Substrate 

in DATRAS dataset
 - depth
 


