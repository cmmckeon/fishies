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

list<-c("ggplot2", "data.table", "reshape2", "arm","car", "Hmisc", "vegan", "viridis", "ggfortify",
        "lme4", "plyr", "plotrix", "colorspace", "plot3D", "plot3D", "rgl","MuMIn",
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "raster", "ncdf4", "marmap", "rgdal", "foreign",
        "sf", "archetypes", "rnaturalearth") # "rst")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data --------------

h <- readRDS("biodiversity/NE_atlantic_per_species_march_2023.rds")
## all relevant cleaned DATRAS hauls, biological and taxmonic data from 2009 - 2021

## unique hauls locations for extracting sst and fp data
sp_co <- unique(h[, c("ShootLong", "ShootLat")]) 
names(sp_co) <- c("x", "y")
sp_co <- drop_na(sp_co)
gc()

#saveRDS(sp_co, "Data_sp_co.rds")

## SST: sea surface temperature -------------------
Sys.time()
## i downloaded aqua modis at a "9km" resolution; assuming this means 9km at equator and about 5km in northern europe as res of raster is 0.04166667 compared to 
## 0.08333333 for 5 min seconds resolution "10km" bioclim data

## get one season in one year to have a look
tmp_raster <- stack("covars/sst_aquamodis_seasonal_2008_2021_NEA/AQUA_MODIS.20080321_20080620.L3m.SNSP.SST.x_sst.nc", varname="sst")
tmp_raster <- brick(tmp_raster, varname="sst")

## get template raster in equal area projection for correct area aggragating 
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
e <- Europe$geometry
e = st_as_sf(e)
e <- st_transform(e,"+proj=laea +lat_0=53 +lon_0=9 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")

sp_co_laea <- st_as_sf(sp_co, coords = c(1,2), crs = crs(tmp_raster))
sp_co_laea <- st_transform(sp_co_laea, crs = st_crs(e))

tmp_raster_laea <- projectRaster(tmp_raster, crs = crs(e))
# plot(tmp_raster)
# plot(sp_co, add =T)


## aggregating resolutions ---------
r5 <- aggregate(tmp_raster_laea, 5)
r10 <- aggregate(tmp_raster_laea, 10)
r20 <- aggregate(tmp_raster_laea, 20)
r50 <- aggregate(tmp_raster_laea, 50)
r100 <- aggregate(tmp_raster_laea, 100)

res5  <- extract(r5, sp_co_laea)
res10  <- extract(r10, sp_co_laea)
res20  <- extract(r20, sp_co_laea)
res50  <- extract(r50, sp_co_laea)
res100  <- extract(r100, sp_co_laea)

#writeRaster(tmp_raster_laea, "repro_tmp_raster.tif")

res <- cbind(sp_co, res5, res10, res20, res50, res100)
rm(r5, r10, r20, r50, r100, res5, res10, res20, res50, res100)


# plot(tmp_raster)
# points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1) ## these points are unique locations, we have a good bit more data than this
# points(h$ShootLong, h$ShootLat, col="green") ## all points. SCOROC and EVHOE are responsible for the points way out top left


## get all seasons all years 2008 - 2021

files <- list.files("covars/sst_aquamodis_seasonal_2008_2021_NEA", pattern = "\\.SST") ## daytime only ## pattern = "*SNSP*\\.SST*")) ## summer and daytime only

## make a stack of rasters
rast_list <- list()

for(i in files){
  sst <- stack(paste("covars/sst_aquamodis_seasonal_2008_2021_NEA/", i, sep = "") , varname="sst")
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
rat <- sst_data %>%
  pivot_longer(!c(x,y), names_to = "time", values_to = "sst")

## name and give year and season columns
rat$year <- substr(rat$time,1,4)
rat$season <- str_sub(rat$time, -4, -1)
rat <- rat[, c("x", "y", "sst", "year", "season")]

## reformat by quarter
rat <- rat %>%
  pivot_wider(names_from = "season", values_from = "sst")

sst_data <- rat

rm(rat, sst, rast_list)

## sst dataset made ---------------------


## FP: fishing pressure ------------------------

## one file test
fp <- read.dbf("covars/fp_ICES_2009_2020/shapefiles/total-2020.dbf")
f <- fp[, c("lon","lat", "kWH_upp")]
# 
# ## subset by North East Atlantic box
# f <- f[f$lon <= max(sp_co$x)  & f$lon >= min(sp_co$x) & f$lat <= max(sp_co$y)  & f$lat >= min(sp_co$y),]
# 
# plot(aquamodis)
# points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1) ## these points are unique locations, we have a good bit more data than this
# # points(f$lon, f$lat, type = "p", col = "green", lwd = 0.1) 
# 
# f <- rasterFromXYZ(f)
# f <- projectRaster(f, tmp_raster)


## get all years 2008 - 2020

files <- list.files("covars/fp_ICES_2009_2020/shapefiles", pattern = glob2rx("total*dbf$")) 

## make a stack of rasters
rast_list <- list()

for(i in files){
  fp <- read.dbf(paste("covars/fp_ICES_2009_2020/shapefiles/", i, sep = ""))
  f <- fp[, c("lon","lat", "kWH_upp")]
  ## subset by North East Atlantic box
  f <- f[f$lon <= max(sp_co$x)  & f$lon >= min(sp_co$x) & f$lat <= max(sp_co$y) & f$lat >= min(sp_co$y),]
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
rat <- fp_data %>%
  pivot_longer(!c("x", "y"), names_to = "year", values_to = "fp")

## rename 
names(rat) <- c("x", "y", "year", "fp")
fp_data <- rat

rm(rat, rast_list, f)

## fish pressure dataset made -------------------
Sys.time()

covars <- left_join(sst_data, fp_data)
#saveRDS(covars, "Data_covars_sst_fp.rds")

# par(bty = "l", bg = "transparent")
# plot(tmp_raster)
# points(sp_co$x, sp_co$y, type = "p", col = "blue", lwd = 0.1)
# points(fp_data$x, fp_data$y, type = "p", col = "light blue", lwd = 0.3)


## Covars extracted -----------------------------

covars <- readRDS("Data_covars_sst_fp.rds")

## map beam trawls 
b <- h[h$Survey == "BTS",]
summary(b$StNo)
x <- b[which(is.na(b$StNo)),]

length(unique(x$HaulID)) ## 1435
y <- unique(x[,c("ShootLong", "ShootLat")]) ## also 1435
## all hauls where station number is NA have unique latlons, so we can use HaulID for location level random effect

## data handling -----------

## combine biodiversity and covars dataframes
covars$m <- paste(covars$y, covars$x, sep = "_")
covars <- covars %>%  rename(Year = year)
h$m <- paste(h$ShootLat, h$ShootLong, sep = "_")


h$Quarter <- as.character(h$Quarter)
h$Quarter[h$Quarter == "1"] <- "SNSP"
h$Quarter[h$Quarter == "2"] <- "SNSU"
h$Quarter[h$Quarter == "3"] <- "SNAU"
h$Quarter[h$Quarter == "4"] <- "SNWI"

modeldf <- merge(h, covars, by = c("m", "Year"), all.x = T)

## handle names and nestedness of random effects
## station numbers (stNo) are missing for BTS surverys, so using their co-ordinates to give unique locations
modeldf$gear_ship <- paste(modeldf$Gear, modeldf$Ship, sep = "_")
modeldf$location_re <- as.character(modeldf$StNo)
modeldf$location_re[is.na(modeldf$StNo)] <- modeldf$m[is.na(modeldf$StNo)]
modeldf$gear_ship_loc <- paste(modeldf$gear_ship, modeldf$location_re, sep = "_")

modeldf <- unique(modeldf[, which(names(modeldf) %in% c("Year", "Gear", "HaulID",
                                                  "Quarter", "DepthNew", "SciName", "ShootLat", 
                                                  "ShootLong", "Total_DensAbund_N_Sqkm", 
                                                  "SNSP", "SNWI", "SNSU",
                                                  "season", "fp", "gear_ship",  "gear_ship_loc"))])

## get seasonal variation in temp
modeldf$sst_var <- modeldf$SNSU - modeldf$SNWI
modeldf <- unique(modeldf[modeldf$Quarter %nin% c("SNSU", "SNAU"),])

modeldf <- drop_na(modeldf)

## traits -------------------------

traits <- read.csv("covars/traits/beukhof_fish_traits.csv")
#theirfish <- unique(traits$taxon) 
#myfish <- unique(modeldf$SciName) ## only missing seven species from my list
traits <- traits[traits$taxon %in% modeldf$SciName,]
traits <- unique(traits[traits$LME %in% c(22,24),]) ## trait values for celtic-biscay shelf and north sea LMEs

## come back for these few if you can
#setdiff(modeldf$SciName, x$taxon)

traits <- unique(traits[, c("taxon", "habitat","feeding.mode", 
                     "tl", "body.shape", "offspring.size", "spawning.type",
                     "age.maturity", "fecundity",
                     "growth.coefficient", "length.max", "age.max")])

list <- c()
for (i in names(traits)){
  list[i] <-length(which(is.na(traits[,i])))}
print(list) ## all variables should be zero 

traits <- drop_na(traits)

## trait PCA ---------------

a <- unique(traits[,c("tl",  "offspring.size", #"body.shape", "spawning.type","feeding.mode", 
                      "age.maturity", "fecundity", "growth.coefficient", 
                         "length.max", "age.max", "taxon")])

## get trait average over the two Large Marine Ecosystems where there are slight differences in the values
a <- unique(a %>% group_by(taxon) %>% mutate(growth.coefficient = mean(growth.coefficient)) %>% 
  mutate(length.max = mean(length.max)) %>% mutate(fecundity = mean(fecundity)) %>% 
    mutate(age.max = mean(age.max)) %>% mutate(age.maturity = mean(age.maturity)) %>% 
    mutate(offspring.size = mean(offspring.size)))

par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, a))) {
  hist(a[,i], breaks = 1000, main = paste(i))
  hist(log(a[,i]), breaks = 1000, main = paste("log",i))
  gc()
}

for (i in c("offspring.size", "age.maturity", "fecundity", "growth.coefficient", 
            "length.max", "age.max")) {
  a[, i] <- c(log(a[,i]))}

for (i in c("tl", "offspring.size", "age.maturity", "fecundity", "growth.coefficient", 
            "length.max", "age.max")) {
  a[, i] <- c(scale(a[,i]))}

# pairs(a[, which(names(a) %in% c("tl",  "offspring.size", 
#                                 "age.maturity", "fecundity", "growth.coefficient", 
#                                 "length.max", "age.max"))], lower.panel = NULL, upper.panel = upper.panel)



## run pca
pc <- prcomp(a[,c("tl", "offspring.size","age.maturity", "fecundity", "growth.coefficient", "length.max", "age.max")])
print(pc)
summary(pc)
#saveRDS(a, "Data_pca_traits.rds")

a$PC1 <- scale(pc[["x"]][,1])
a$PC2 <- scale(pc[["x"]][,2])
a$PC3 <- scale(pc[["x"]][,3])

modeldf <- merge(modeldf, a, by.x = "SciName", by.y = "taxon", all.x = T)

## explore data 

# Quick look at model dataframe
# Numeric variables
par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, modeldf))) {
  hist(modeldf[,i], breaks = 1000, main = paste(i))
  hist(log(modeldf[,i]), breaks = 1000, main = paste("log",i))
  gc()
}

## transform

abundance <- drop_na(modeldf)

l <- c("DepthNew","SNSP", "sst_var", "fp")

for (i in l) {
  abundance[, i] <- c(log(abundance[,i]))
}

abundance$abund <- log(abundance$Total_DensAbund_N_Sqkm)

abundance <- droplevels(abundance)

abundance$Quarter <- factor(abundance$Quarter, levels = c("SNWI", "SNSP"))
abundance$Quarter <- as.numeric(factor(abundance$Quarter))

## scale

for (i in c("Year", "Quarter", "DepthNew",
             "SNSP", "SNWI", 
            "fp", "sst_var", "abund")) {
  abundance[, i] <- c(scale(abundance[,i]))
}

## add in the template for aggregating commmunties to coarser resolutions
abundance <- merge(abundance, res, by.x = c("ShootLat", "ShootLong"), by.y = c("y", "x"))


## save abundance modeling dataframe --------------
#saveRDS(abundance, "Data_modeldf_abundance.rds")

## cwm ------------

abundance <- readRDS("Data_modeldf_abundance.rds")

cwm <- abundance[, c("SciName", "HaulID", "Year","Quarter", "Total_DensAbund_N_Sqkm", "PC1", "PC2", "PC3", 
                     "res5", "res10", "res20", "res50","res100")]

# Calculating CWM using dplyr and tidyr functions
cwm1 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(HaulID) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
    )

cwm5 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res5, Year, Quarter) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm5 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm5 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm5 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm10 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res10, Year, Quarter) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm10 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm10 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm10 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm20 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res20, Year, Quarter) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm20 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm20 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm20 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm50 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res50, Year, Quarter) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm50 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm50 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm50 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )



cwm <- merge(cwm1, abundance, by = "HaulID")
gc()
cwm <- merge(cwm5, cwm, by = c("res5", "Year", "Quarter"))
gc()
cwm <- merge(cwm10, cwm, by = c("res10", "Year", "Quarter"))
gc()
cwm <- merge(cwm20, cwm, by = c("res20", "Year", "Quarter"))
gc()
cwm <- merge(cwm50, cwm, by = c("res50", "Year", "Quarter"))

#saveRDS(cwm, "Data_cwm_PCA.rds")

par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, cwm))) {
  hist(cwm[,i], breaks = 1000, main = paste(i))
  gc()
}


## run models ----------------------

## run in terminal

# source("R_Scripts/14a_1cwm_analysis.R")
# Sys.sleep(60)
# gc()
# source("R_Scripts/14b_5scale_analysis.R")
# Sys.sleep(60)
# gc()
# source("R_Scripts/14c_10scale_analysis.R")
# Sys.sleep(60)
# gc()
# source("R_Scripts/14d_20scale_analysis.R")
# Sys.sleep(60)
# gc()
# source("R_Scripts/14e_50scale_analysis.R")


## end ----------------

