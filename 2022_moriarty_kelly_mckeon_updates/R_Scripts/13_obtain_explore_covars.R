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

list<-c("ggplot2", "data.table", "reshape2", "arm","car", "DMwR", "Hmisc", "vegan", "viridis", "ggfortify",
        "lme4", "plyr", "plotrix", "colorspace", "plot3D", "plot3D", "rgl","MuMIn",
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "raster", "ncdf4", "marmap", "rgdal", "foreign",
        "sf") # "rst")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## pairs function
upper.panel<-function(x, y){
  points(x,y, pch=21, col=c("grey"), cex = 0.5)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.8, 0.9, txt, cex =0.7)
}

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

## aggregating resolutions ---------
r5 <- aggregate(tmp_raster, 5)
r10 <- aggregate(tmp_raster, 10)
r10 <- aggregate(tmp_raster, 10)
r20 <- aggregate(tmp_raster, 20)
r50 <- aggregate(tmp_raster, 50)
r100 <- aggregate(tmp_raster, 100)

res5  <- extract(r5, sp_co)
res10  <- extract(r10, sp_co)
res20  <- extract(r20, sp_co)
res50  <- extract(r50, sp_co)
res100  <- extract(r100, sp_co)

res <- cbind(sp_co, res5, res10, res20, res50, res100)
rm(r5, r10, r20, r50, r100, res5, res10, res20, res50, res100)


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

## reformat by quarter
for(i in levels(factor(rat$season))){
  rat[,paste(i)] <- NA
  rat[,paste(i)][rat$season == i] <- rat$sst[rat$season == i]}

x <- unique(rat[, c("x", "y", "year")])
for(i in levels(factor(rat$season))){
  x <- merge(x, 
             rat[,c("x", "y", "year", i)][!is.na(rat[,i]),], 
             by = c("x", "y", "year"), all.x = T)}

sst_data <- x

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
#saveRDS(covars, "Data_covars_sst_fp.rds")

par(bty = "l", bg = "transparent")
plot(tmp_raster)
points(sp_co$x, sp_co$y, type = "p", col = "blue", lwd = 0.1)
points(fp_data$x, fp_data$y, type = "p", col = "light blue", lwd = 0.3)


## Covars extracted -----------------------------

covars <- readRDS("Data_covars_sst_fp.rds")

c <- c()
#plot(sum(covars$fp) ~ covars$year)
for(i in unique(covars$year)){
  c <- c(c, sum(covars$fp[covars$year == i], na.rm = T))}
plot(c ~ unique(covars$year))

d <- c()
for(i in unique(covars$year)){
  d <- c(d, sum(covars$SNSU[covars$year == i], na.rm = T))}
plot(d[-15] ~ unique(covars$year[covars$year != "2022"]))

# d <- c()
# for(i in unique(covars$year)){
#   d <- c(d, sum(covars$sst[covars$year == i & covars$season == "SNSU"], na.rm = T))}
# plot(d[-15] ~ unique(covars$year[covars$year != "2022"]))


## map beam trawls 
b <- h[h$Survey == "BTS",]
summary(b$StNo)
x <- b[which(is.na(b$StNo)),]

length(unique(x$HaulID)) ## 263
length(unique(x[, c("ShootLong", "ShootLat")])) ## also 263

## all hauls where station number is NA have unique latlons, so i THINK we can use HaulID for 
## location level random effect....

## data handling -----------

## combine biodiversity and covars dataframes
covars$m <- paste(covars$y, covars$x, sep = "_")
covars <- covars %>%  rename(Year = year)
h$m <- paste(h$ShootLat, h$ShootLong, sep = "_")

#covars <- covars %>%  rename(Quarter = season)

h$Quarter <- as.character(h$Quarter)
h$Quarter[h$Quarter == "1"] <- "SNSP"
h$Quarter[h$Quarter == "2"] <- "SNSU"
h$Quarter[h$Quarter == "3"] <- "SNAU"
h$Quarter[h$Quarter == "4"] <- "SNWI"

## only one year has summer data - drop this quarter
h <- h[h$Quarter %nin% c("SNSU", "SNAU"),]

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
                                                  "SNAU", "SNSP", "SNSU", "SNWI", 
                                                  "season", "fp", "gear_ship",  "gear_ship_loc"))])

## get seasonal variation in temp
modeldf$sst_var <- modeldf$SNSU - modeldf$SNWI

## traits -------------------------

traits <- read.csv("~/Desktop/covars/traits/beukhof_fish_traits.csv")
#theirfish <- unique(traits$taxon) 
#myfish <- unique(modeldf$SciName) ## only missing seven species from my list
traits <- traits[traits$taxon %in% modeldf$SciName,]
traits <- unique(traits[traits$LME %in% c(24),]) ## trait values for celtic-biscay shelf LME

## come back for these few if you can
#setdiff(modeldf$SciName, x$taxon)

traits <- traits[, c("taxon", "habitat","feeding.mode", 
                     "tl", "body.shape", "offspring.size", "spawning.type",
                     "age.maturity", "fecundity",
                     "growth.coefficient", "length.max", "age.max")]

list <- c()
for (i in names(traits)){
  list[i] <-length(which(is.na(traits[,i])))}
print(list) ## all variables should be zero 

traits <- drop_na(traits)

## trait PCA ---------------



rcorr(as.matrix(abundance[, which(names(abundance) %in% c("Year", "DepthNew",  "SNSP", "SNWI", "fp", "sst_var", 
                                                          "tl",  "offspring.size",  "age.maturity", "fecundity", "growth.coefficient", 
                                                          "length.max", "age.max", "fp_yn", "abund", "resp_total", "ab_haul_total", "rel_ab"))]))

rcorr(as.matrix(traits[, which(names(traits) %in% c("tl",  "offspring.size", 
                                                    "age.maturity", "fecundity", "growth.coefficient", 
                                                    "length.max", "age.max"))]))

rcorr(as.matrix(abundance[, which(names(abundance) %in% c("Year",  "DepthNew", "SNAU", "SNSP", "SNSU", "SNWI", "fp",  "sst_var", 
                                                          "ab"))]))


a <- unique(traits[,c("tl",  "offspring.size", "body.shape", "spawning.type",
                     # "spawning.type",
                         "feeding.mode", "age.maturity", "fecundity", "growth.coefficient", 
                         "length.max", "age.max", "taxon")])

par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, a))) {
  hist(a[,i], breaks = 1000, main = paste(i))
 # hist(log(a[,i]), breaks = 1000, main = paste("log",i))
  gc()
}


#range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (i in c("tl",  "offspring.size", "age.maturity", "fecundity", "growth.coefficient", 
            "length.max", "age.max")) {
  a[, i] <- c(log(a[,i]))
  a[, i] <- c(scale(a[,i]))
 # a[,i] <- range01(a[,i]) -0.5
}

# for(i in names(a)){
#   a[,i] <- a[,i] - min(a[,i])}

pairs(a[, which(names(a) %in% c("tl",  "offspring.size", 
                                "age.maturity", "fecundity", "growth.coefficient", 
                                "length.max", "age.max"))], lower.panel = NULL, upper.panel = upper.panel)



## run pca
pc <- prcomp(a[,c("tl", "offspring.size","age.maturity", "fecundity", "growth.coefficient", "length.max", "age.max")])
print(pc)
summary(pc)

pairs.panels(pc$x,
             gap=0,
             bg = cl[a$body.shape],
             pch=21)

par(mfrow = c(2,1))
autoplot(pc, data = a, #colour = cl[a$body.shape], 
         loadings = TRUE, loadings.label = TRUE, loadings.colour = "dark grey", 
         loadings.label.size = 4, loadings.label.colour = "black")

# a$PC1 <- (pc[["x"]][,1])
# a$PC2 <- (pc[["x"]][,2])

a$PC1 <- scale(pc[["x"]][,1])
a$PC2 <- scale(pc[["x"]][,2])
a$PC3 <- scale(pc[["x"]][,3])

modeldf <- merge(modeldf, a, by.x = "SciName", by.y = "taxon", all.x = T)

## explore data 

# Quick look at model dataframe
# Factors
# for (i in names(Filter(is.factor, modeldf))) {
#   plot(modeldf[,i],
#        main = paste(i))
#   gc()
# }

# Numeric variables
par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, modeldf))) {
  hist(modeldf[,i], breaks = 1000, main = paste(i))
  hist(log(modeldf[,i]), breaks = 1000, main = paste("log",i))
  gc()
}

## transform
## make a presense absense of fishing pressure column
modeldf$fp_yn[is.na(modeldf$fp)] <- 0 
modeldf$fp_yn[!is.na(modeldf$fp)] <- 1 

abundance <- drop_na(modeldf)

l <- c("DepthNew","SNSP", "SNAU", "sst_var", "fp")

for (i in l) {
  abundance[, i] <- c(log(abundance[,i]))
}

abundance$abund <- log(abundance$Total_DensAbund_N_Sqkm)

abundance <- droplevels(abundance)

abundance$Quarter <- factor(abundance$Quarter, levels = c("SNWI", "SNSP"))
abundance$Quarter <- as.numeric(factor(abundance$Quarter))

## scale

for (i in c("Year", "Quarter", "DepthNew",
            "SNAU", "SNSP", "SNSU", "SNWI", 
            "fp", "sst_var", "abund")) {
  abundance[, i] <- c(scale(abundance[,i]))
}

## make the response variable
abundance$resp_total <- abundance$abund-min(abundance$abund)


## abundance by location
for(i in unique(abundance$HaulID)){
  print(i)
  abundance$ab_haul_total[abundance$HaulID == i] <- sum(abundance$Total_DensAbund_N_Sqkm[abundance$HaulID == i])
}
abundance$rel_ab <-  abundance$Total_DensAbund_N_Sqkm/abundance$ab_haul_total

abundance <- drop_na(abundance) 

abundance$ab <- abundance$abund-min(abundance$abund)
abundance$rel_ab <- abundance$rel_ab - 1.727584e-08

par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, abundance))) {
  hist(abundance[,i], breaks = 1000, main = paste(i))
  #hist(log(modeldf[,i]), breaks = 1000, main = paste("log",i))
  gc()
}

abundance <- merge(abundance, res, by.x = c("ShootLat", "ShootLong"), by.y = c("y", "x"))


## save abundance modeling dataframe --------------
#saveRDS(abundance, "Data_modeldf_abundance.rds")


## ordiantation object
or <- metaMDS(a[,c("tl", "offspring.size","age.maturity", "fecundity", 
                   "growth.coefficient", "length.max", "age.max")], distance = "jaccard")

## Nonmetric Multidimensional Scaling of compositional dissimilarity by treatment
ordiplot(pc, type = "n", main = NULL)
ordiellipse(pc, groups = b$feeding.mode, draw = "polygon", lty = 1, 
            col = cl,
            alpha = 0.2)
points(pc[["x"]], display = "sites", 
       pch = c(16, 8, 17, 18, 20)[as.numeric(b$feeding.mode)], 
       col = cl[as.numeric(b$feeding.mode)], 
       cex =1.2)

legend("topright", legend = levels(b$feeding.mode), pch = c(16, 8, 17, 18, 20), 
       col = cl,
       bty = "n", cex = 1) # displays symbol and colour legend
legend("topleft", legend = "A", bty = "n")


## archetype analysis -------------------

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

a$PC1 <- (pc[["x"]][,1])
a$PC2 <- (pc[["x"]][,2])

a$PC1 <- scale(pc[["x"]][,1])
a$PC2 <- scale(pc[["x"]][,2])
a$PC3 <- scale(pc[["x"]][,3])

# for(i in names(a[, c("tl", "offspring.size",  
#                      "age.maturity", "fecundity", "growth.coefficient", "length.max", 
#                      "age.max" ,
# "PC1", "PC2")])){ 
# a[,i] <- range01(a[,i]) -0.5
#}


library("archetypes")

arch2 <- archetypes(a[,c("tl", "offspring.size",  
                         "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                         "age.max")],  2)

arch3 <- archetypes(a[,c("tl", "offspring.size",  
                          "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                          "age.max")], 3)

arch4 <- archetypes(a[,c("tl", "offspring.size",  
                         "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                         "age.max")], 4)

xyplot(arch2, a[,c("tl", "offspring.size",  
                  "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                  "age.max")], chull = chull(a[,c("tl", "offspring.size",  
                                                  "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                                                  "age.max")]))

par(mfrow=c(4,4))
for (i in names(Filter(is.numeric, a))) {
  hist(a[,i], breaks = 1000, main = paste(i))
  #hist(log(modeldf[,i]), breaks = 1000, main = paste("log",i))
  gc()
}

save <- a
par(mfrow =c(1,1))
xyplot(arch4, a[, c("PC1", "PC3")]) #, chull = chull(a[, c("PC1", "PC2")]))
xyplot(arch4, a[, c("PC2", "PC3")]) #, chull = chull(a[, c("PC1", "PC2")]))
xyplot(arch4, a[, c("age.maturity", "fecundity")]) #, chull = chull(a[, c("PC1", "PC2")]))

xyplot(arch4, a[, c("PC1", "PC2")], adata.show = TRUE)

as <- stepArchetypes(data = a[,c("tl", "offspring.size",  
                                 "age.maturity", "fecundity", "growth.coefficient", "length.max", 
                                 "age.max")], k = 1:10)
rss(as)
screeplot(as)

ternaryplot(coef(arch4, 'alphas'))


## cwm ------------

cwm <- abundance[, c("SciName", "HaulID",  "Total_DensAbund_N_Sqkm", "PC1", "PC2", "PC3", 
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
  group_by(res5) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm5 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm5 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm5 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm10 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res10) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm10 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm10 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm10 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm20 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res20) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm20 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm20 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm20 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )

cwm50 <-   # New dataframe where we can inspect the result
  cwm %>%   # First step in the next string of statements
  group_by(res50) %>%   # Groups the summary file by HaulID number
  summarize(           # Coding for how we want our CWMs summarized
    PC1_cwm50 = weighted.mean(PC1, Total_DensAbund_N_Sqkm),   # Actual calculation of CWMs
    PC2_cwm50 = weighted.mean(PC2, Total_DensAbund_N_Sqkm),
    PC3_cwm50 = weighted.mean(PC3, Total_DensAbund_N_Sqkm)
  )



cwm <- merge(cwm1, abundance, by = "HaulID")

cwm <- merge(cwm5, cwm, by = "res5")
cwm <- merge(cwm10, cwm, by = "res10")
cwm <- merge(cwm20, cwm, by = "res20")
cwm <- merge(cwm50, cwm, by = "res50")

#saveRDS(cwm, "Data_cwm_PCA.rds")









## presense absense ---------

# length(unique(modeldf$HaulID))
# 
# occ <- modeldf[, c("SciName", "Total_DensAbund_N_Sqkm", "HaulID")]
# 
# occ <- as.data.frame(cbind(rep(unique(modeldf$SciName), length(unique(modeldf$SciName)))),
#                      rep(1:length(unique(modeldf$SciName))))
# 
# ##  get occurrence for all species in each Haul
# occ <- as.data.frame(rep(1:250, length(unique(modeldf$HaulID))))
# occ <- as.data.frame(occ[order(occ[,1]),])
# occ$HaulID <- rep(unique(modeldf$HaulID), length(unique(modeldf$SciName)))
# occ <- as.data.frame(occ[order(occ[,2]),])
# occ$SciName <- rep(unique(modeldf$SciName), length(unique(modeldf$HaulID)))
# 
# occ$pres_abs <- 0
# 
# for(i in unique(modeldf$SciName)){
#   for(j in unique(modeldf$HaulID)){
#     if(is.numeric(modeldf$Total_DensAbund_N_Sqkm[modeldf$SciName == i & modeldf$HaulID == j])){
#     occ$pres_abs[occ$SciName == i & occ$HaulID == j] <- 1}
#     else {occ$pres_abs[occ$SciName == i & occ$HaulID == j] <- 0}
#   }
# }
# 
# save <- occ
# 
# occ <- occ[,-1]
# occ <- merge(occ, modeldf, by = c("HaulID"), all.x =T)
# occ$pres_abs[!is.na(occ$Total_DensAbund_N_Sqkm)] <- 1
# occ$pres_abs[is.na(occ$Total_DensAbund_N_Sqkm)] <- 0
# 
# names(occ) <- c("SciName", "HaulID", "order", "pres_abs")
# 
# occ <- merge(occ, modeldf, by = c("SciName", "HaulID"), all.x =T)
# 




# pc <- prcomp(abundance[,c("SNSP", "SNWI","sst_var")])
# print(pc)
# summary(pc)
# 
# pairs.panels(pc$x,
#              gap=0,
#              bg = cl[a$body.shape],
#              pch=21)
