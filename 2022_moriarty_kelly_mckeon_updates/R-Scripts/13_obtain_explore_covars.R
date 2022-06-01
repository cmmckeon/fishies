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
#check <- hh[grep("NA", hh$HaulID),] ## 4054 hauls with no station number
#length(which(is.na(hh$StNo))) ## 26674
check <- hh[grep("NA", hh$NewUniqueID2),]

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
#map <- crop(map, extent(-15.797, 2.944, 43.39, 60.23))

## where does that leave us?
# require(rgdal)
# europe<-readOGR("./Regional Boundaries Maps and Data/shapes//europe.dbf","europe") 
# plot(europe, col="lightgrey")
# points(h$ShootLong, h$ShootLat, col="green") ## SCOROC and EVHOE are way out


## SST: sea surface temperature -------------------

## i downloaded aqua modis at a "9km" resolution; assuming this means 9km at equator and about 5km in northern europe as res of raster is 0.04166667 compared to 
## 0.08333333 for 5 min seconds resolution "10km" bioclim data

## get one season in one year to have a look
aqua_modis <- stack("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021/AQUA_MODIS.20081221_20090320.L3m.SNWI.SST.x_sst.nc", varname="sst")
tmp_raster <- brick(aqua_modis, varname="sst")

plot(aqua_modis)
points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1) ## these points are unique locations, we have a good bit more data than this
#points(h$ShootLong, h$ShootLat, col="green") ## all points. SCOROC and EVHOE are responsible for the points way out top left


## get all seasons all years 2009 - 2021

files <- list.files("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021", pattern = "\\.SST") ## daytime only ## pattern = "*SNSP*\\.SST*")) ## summer and daytime only

Sys.time()
rast_list <- list()

for(i in files){
  sst <- stack(paste("~/Desktop/covars/sst_aquamodis_seasonal_2009_2021/", i, sep = "") , varname="sst")
  rast_list[[i]] <- brick(sst, varname="sst")
  gc()}
Sys.time()

sst_brick <- brick(rast_list)   

plot(sst_brick)

## extract sst values for our haul locations
sst_data  <- extract(sst_brick, sp_co)
sst_data <- cbind(sp_co, sst_data)

## names for sst dataset
gone <- c("AQUA_MODIS.", "\\.SST.x_sst.nc")
files <- gsub(paste(gone, collapse = "|"), "", files, ignore.case = TRUE)
files <- gsub("\\.L3m.", "_", files, ignore.case = TRUE)
files <- gsub(".*\\_20", "20", files, ignore.case = TRUE)


names(sst_data) <- c(names(sp_co), files)


for (i in names(sst_data)[which(names(sst_data) %nin% c("x", "y"))]){
  d <- cbind(sst_data[, which(names(sst_data) %in% c("x", "y"))], sst_data[,i])
  d$
  rat <- rbind(rat, d)
}


## start
## all i've to do is flip this dataframe and I'm done with sst

rat <- as.data.frame(rep(names(sst_data)[which(names(sst_data) %nin% c("x", "y"))], length(sst_data$x)))

names(rat) <- "time"

for (i in names(sst_data)[which(names(sst_data) %nin% c("x", "y"))]){
  rat$sst[rat$time == i] <- sst_data[,i] 
}

rat$year <- substr(rat$time,1,4)
rat$season <- str_sub(rat$time, -4, -1)

## sst dataset made ---------------------


## FP: fishing pressure ------------------------

fp <- shapefile("~/Desktop/covars/fp_ICES_2009_2020/shapefiles/") 
x <- rasterize(fp, tmp_raster)
plot(x)

plot(fp)

fp <- readOGR(dsn = "~/Desktop/covars/fp_ospar_2009_2017/ICES.2018.Shapefiles-OSPAR-spatial-data-fishing-intensity/2017", layer = "OSPAR_intensity_total_2017")
x <- rasterize(teow, mat)
a <- raster::deratify(x, "ECO_NAME")
writeRaster(a, "Data_08_teow_econame.grd")


## OR 

eco_rast <- raster("Data_08_teow_econame.grd")






f <- system.file('external/test.grd',package = 'raster')

y <- raster("~/Desktop/covars/fp_ospar_2009_2017/ICES.2018.Shapefiles-OSPAR-spatial-data-fishing-intensity/2017/OSPAR_intensity_total_2017.shp")

bio1 <- raster("~/Library/CloudStorage/OneDrive-Personal/PhD/landuse_climate_lifeform/bio1.bil")

# index
r <- raster(f)

# check if in memory

inMemory(r)
#[1] FALSE # output

# this would be an extent from your overlapping shapefile
e <- extent(r,58,68,40,50)

# get cells from extent; either use cells as index directly or convert to rowcol



rowcol <- rowColFromCell(r,cellsFromExtent(r,e))

v <- getValuesBlock(r,row=rowcol[1,1],nrows=(rowcol[nrow(rowcol),1] - rowcol[1,1]),
                    col=rowcol[1,2],ncols=(rowcol[nrow(rowcol),2] - rowcol[1,2]))







