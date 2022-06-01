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
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "raster", "ncdf4", "marmap", "rgdal", "foreign",
        "sf") # "rst")

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
#check <- hh[grep("NA", hh$HaulID),] ## 4054 hauls with no station number
#length(which(is.na(hh$StNo))) ## 26674
#check <- hh[grep("NA", hh$NewUniqueID2),]

## add check_StatRec from HH to the cleaned Biodiversity data
x <- unique(hh[,which(names(hh) %in% c("check_StatRec", "HaulID"))])
h <- merge(h, x, by = "HaulID")

## end checking ----------------

## get boundary box (lat long) of relevant surveys
box <- h[h$Survey %in% c("EVHOE", "FR-CGFS", "IE-IGFS", "NIGFS",  "ROCKALL", "SCOROC", "SCOWCGFS",  "SWC-IBTS"),]

h <- h[h$check_StatRec %in% box$check_StatRec,]
rm(box, x)

## unique hauls locations for extracting sst and fp data
sp_co <- unique(h[, c("ShootLong", "ShootLat")]) 
names(sp_co) <- c("x", "y")

# summary(h$ShootLat)
# summary(h$ShootLong)

## North East Atlantic study area extent
#map <- crop(map, extent(-17, 4, 42, 61))


## SST: sea surface temperature -------------------
Sys.time()
## i downloaded aqua modis at a "9km" resolution; assuming this means 9km at equator and about 5km in northern europe as res of raster is 0.04166667 compared to 
## 0.08333333 for 5 min seconds resolution "10km" bioclim data

## get one season in one year to have a look
tmp_raster <- stack("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021/AQUA_MODIS.20081221_20090320.L3m.SNWI.SST.x_sst.nc", varname="sst")
tmp_raster <- brick(tmp_raster, varname="sst")

#plot(tmp_raster)
#points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1) ## these points are unique locations, we have a good bit more data than this
#points(h$ShootLong, h$ShootLat, col="green") ## all points. SCOROC and EVHOE are responsible for the points way out top left


## get all seasons all years 2009 - 2021

files <- list.files("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021", pattern = "\\.SST") ## daytime only ## pattern = "*SNSP*\\.SST*")) ## summer and daytime only

## make a stack of rasters
rast_list <- list()

for(i in files){
  sst <- stack(paste("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021/", i, sep = "") , varname="sst")
  rast_list[[i]] <- brick(sst, varname="sst")
  gc()}

sst_brick <- brick(rast_list)   

#plot(sst_brick)

## extract sst values for our haul locations
sst_data  <- extract(sst_brick, sp_co)
sst_data <- cbind(sp_co, sst_data)

## names for sst dataset
gone <- c("AQUA_MODIS.", "\\.SST.x_sst.nc")
files <- gsub(paste(gone, collapse = "|"), "", files, ignore.case = TRUE)
files <- gsub("\\.L3m.", "_", files, ignore.case = TRUE)
files <- gsub(".*\\_20", "20", files, ignore.case = TRUE)

names(sst_data) <- c(names(sp_co), files)

## reformat dataset
rat <- data.frame()
for (i in names(sst_data)[which(names(sst_data) %nin% c("x", "y"))]){
  d <- cbind(sst_data[, which(names(sst_data) %in% c("x", "y"))], sst_data[,i])
  d$time <- i
  rat <- rbind(rat, d)
}

## name and give year and season columns
names(rat) <- c("x", "y", "sst", "time")
rat$year <- substr(rat$time,1,4)
rat$season <- str_sub(rat$time, -4, -1)

sst_data <- rat

rm(rat, d, sst, rast_list)

## sst dataset made ---------------------


## FP: fishing pressure ------------------------

## one file test
# fp <- read.dbf("~/Desktop/covars/fp_ICES_2009_2020/shapefiles/total-2020.dbf")
# f <- fp[, c("lon","lat", "kWH_upp")]
# 
# ## subset by North East Atlantic box
# f <- f[f$lon <= max(sp_co$x)  & f$lon >= min(sp_co$x) & f$lat <= max(sp_co$y)  & f$lat >= min(sp_co$y),]
# 
# # plot(aqua_modis)
# # points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1) ## these points are unique locations, we have a good bit more data than this
# # points(f$lon, f$lat, type = "p", col = "green", lwd = 0.1) 
# 
# f <- rasterFromXYZ(f)
# f <- projectRaster(f, tmp_raster)


## get all years 2009 - 2020

files <- list.files("~/Desktop/covars/fp_ICES_2009_2020/shapefiles", pattern = glob2rx("total*dbf$")) 

## make a stack of rasters
rast_list <- list()

for(i in files){
  fp <- read.dbf(paste("~/Desktop/covars/fp_ICES_2009_2020/shapefiles/", i, sep = ""))
  f <- fp[, c("lon","lat", "kWH_upp")]
  ## subset by North East Atlantic box
  f <- f[f$lon <= max(sp_co$x)  & f$lon >= min(sp_co$x) & f$lat <= max(sp_co$y)  & f$lat >= min(sp_co$y),]
  f <- rasterFromXYZ(f, crs = tmp_raster@crs)
  f <- projectRaster(f, tmp_raster) 
  rast_list[[i]] <- f
  gc()
  }

fp_brick <- brick(rast_list)   

#plot(fp_brick)

## extract fp values for our haul locations
fp_data  <- extract(fp_brick, sp_co)
fp_data <- cbind(sp_co, fp_data)

## names for fp dataset
gone <- c("total\\-", "\\.dbf")
files <- gsub(paste(gone, collapse = "|"), "", files, ignore.case = TRUE)
names(fp_data) <- c(names(sp_co), files)

## reformat dataset
rat <- data.frame()
for (i in names(fp_data)[which(names(fp_data) %nin% c("x", "y"))]){
  d <- cbind(fp_data[, which(names(fp_data) %in% c("x", "y"))], fp_data[,i])
  d$year <- i
  rat <- rbind(rat, d)
}

## rename 
names(rat) <- c("x", "y", "fp", "year")
fp_data <- rat

rm(rat, d, rast_list, f)

## fish pressure dataset made -------------------
Sys.time()

covars <- merge(sst_data, fp_data, by = c("x", "y", "year"), all.x = T)

# plot(tmp_raster)
# points(sp_co$x, sp_co$y, type = "p", col = "blue", lwd = 0.1) 
# points(fp_data$x, fp_data$y, type = "p", col = "light blue", lwd = 0.3) 


## Covars extracted -----------------------------






## map beam trawls 
b <- h[h$Survey == "BTS",]
summary(b$StNo)
x <- b[which(is.na(b$StNo)),]

length(unique(x$HaulID)) ## 263
length(unique(x[, c("ShootLong", "ShootLat")])) ## also 263

## all hauls where station number is NA have unique latlons, so i THINK we can use HaulID for 
## location level random effect....









