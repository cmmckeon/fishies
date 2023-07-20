# fishies
Analysis of environmental and anthropogenic drivers of fish occurrence across life history strategy in the North Eastern Atlantic

R version 3.6.3 "Holding the Windsock" 

and

R version 4.2.0 "Vigorous Calestetics" (for using the rfishbase package, then back to 3.6.3 for using raster)

There are two sections to this work 

DATRAS data cleaning:
  - cleaning and standardising survey effort (scripts 1 - 7) 
  - cleaning and standardising biodiversity data (scripts 8 - 12)
  
Main analysis pipeline:
  - collating covariates, spatial manipulations, analysis, diagnostics, and plotting and tablulating results

DATRAS cleaning code is adapted from work by Meadhbh Moriarty in 2016 and built on by Ruth Kelly 2021. These scripts made available here are for accessibility purposes, but readers are not expected to run them themsevles from scratch. A robust, clean and public version of the DATRAS cleaning pipeline is the work of a separate project that we hope will be available by the time this study is published. If you want to run this analysis yourself, we recommend you start at script 13.

## scripts we use

Haul data cleaning: 1 - 7

Biological data cleaning: 8 & 9

Covariate wrangling: 13

Analysis: 14a-14e & 15

Plotting: 16 & 17

# DATA NEEDED:

## DATRAS DOWNLOAD 04/2022

## Fishing pressure
- OSPAR spatial data layers of fishing intensity/pressure 2009 - 2020 https://ices-library.figshare.com/articles/dataset/Data_for_OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18601508

## Sea surface temperature
- Ocean colour NASA - sea surface temperature from 2002 - 2022 https://oceancolor.gsfc.nasa.gov/l3/
can extract seasonal SST (as per Lousie Rutterford) at 9km resolution within boundary box of your choosing
nc files (easily rasterised)
1 season of data for my study area = approx 240KB. 240*4*13 = 12.5 MB total (they also send nighttime sst so double that) 
WAY better than copernicus for my purposes

## Depth
- use NOAA website to get bathy map
papoue <- getNOAA.bathy(lon1 = -16, lon2 = 13,
                         lat1 = 36, lat2 = 62, resolution = 1)
saveRDS(papoue, "papoue_bathy_map.rds")

## trait data
- beukhof_fish_traits.csv from Beukhof et al., (2019) https://doi.pangaea.de/10.1594/PANGAEA.900866 



