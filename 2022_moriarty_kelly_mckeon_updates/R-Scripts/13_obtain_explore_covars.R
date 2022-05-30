## 13_obtain_explore_covars

## this script is for obtaining seas surface temperature (sst) and fishing pressure (fp) from the internet

# working with Surveys:
# "BTS", 
# "BTS-VIII", 
# "EVHOE", 
# "FR-CGFS", 
# "IE-IGFS", 
# "NIGFS",  
# "ROCKALL", 
# "SCOROC", 
# "SCOWCGFS",  
# "SWC-IBTS"



### set up ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("ggplot2", "data.table", "reshape2", "arm","car", "DMwR", 
        "lme4", "plyr", "plotrix", "colorspace", "plot3D", "plot3D", "rgl","MuMIn",
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "raster", "ncdf4", "marmap") # "rst")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data --------------

h <- read.csv("~/Desktop/covars/BiologicalInfo_per_species_2009_on_30_05_2022.csv")
## all relevant cleaned DATRAS hauls, biological and taxmonic data from 2009 - 2021
#h <- read.csv("~/Desktop/covars/BiologicalInfo_withlengths_2009_on_30_05_2022.csv")

summary(h)


## cleaned hauls to check some things --------
## ask Ruth about using check_StatRec instead of StNo
hh <- readRDS("clean_HH.rds")
check <- hh[grep("NA", hh$HaulID),] ## 4054 hauls with no station number
length(which(is.na(hh$StNo))) ## 26674 
check <- hh[grep("NA", hh$NewUniqueID2),]

## add check_StatRec from HH to the cleaned Biodiversity data
x <- unique(hh[,which(names(hh) %in% c("check_StatRec", "HaulID"))])
h <- merge(h, x, by = "HaulID")

## end checking ----------------

## get boundary box (lat long) of relevant surveys
box <- h[h$Survey %in% c("EVHOE", "FR-CGFS", "IE-IGFS", "NIGFS",  "ROCKALL", "SCOROC", "SCOWCGFS",  "SWC-IBTS"),]

h <- h[h$check_StatRec %in% box$check_StatRec,]
rm(box, x)

# summary(h$ShootLat)
# summary(h$ShootLong)

## North East Atlantic study area extent
map <- crop(map, extent(-15.797, 2.944, 43.39, 60.23))


## where does that leave us?
require(rgdal)
europe<-readOGR("./Regional Boundaries Maps and Data/shapes//europe.dbf","europe") 
plot(europe, col="lightgrey")
points(h$ShootLong, h$ShootLat, col="green") ## SCOROC and EVHOE are way out




## looking at sea surface temp

sst <- nc_open("~/Desktop/covars/dataset-satellite-sea-surface-temperature-f6c4c630-5f60-46ac-974f-1d78089818ee/20210601120000-C3S-L4_GHRSST-SSTdepth-OSTIA-GLOB_ICDR2.1-v02.0-fv01.0.nc")

aqua_modis <- nc_open("~/Desktop/covars/am_seasonal/am_mar_june.nc")

aqua_modis <- stack("~/Desktop/covars/am_seasonal/am_mar_june.nc", varname="sst")
tmp_raster <- brick(aqua_modis, varname="sst")

plot(aqua_modis)

## looking at OSPAR fishing pressure (2017 - 2021 available)

## https://www.emodnet-humanactivities.eu/search-results.php?dataname=Vessel+Density+#ID0EAEA
## View the vessel density data on an interactive map of Europe by clicking on a cell to retrieve the value 
## (hours per square km per month) by ship type.

ves <- raster("~/Desktop/covars/EMODnet_HA_Vessel_Density_202106/2021_06_st_All.tif")

plot(ves)


