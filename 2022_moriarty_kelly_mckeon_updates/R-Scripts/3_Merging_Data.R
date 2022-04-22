# title: "3_Merging_Data.R"
# last updated: "08/04/2022"

# This is Script 3 of 7
# The purpose of this script is to merge the data into appropriate groupings for 
# data checking and processing


## set up
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

load("./script2_output.rda")
# load("./script3_output.rda")

## OR

gc()

## Survey data (HL)------------

# cut each dataset to the start date used by Moriarty et al.2017 - RK 2021
HH_NIGFS<-as.data.table(HH_NIGFS)

HH_EVHOE<-as.data.table(HH_EVHOE)
table(HH_EVHOE$Year)

HH_FRCGFS<-as.data.table(HH_FRCGFS)
table(HH_FRCGFS$Year)

HH_IGFS<-as.data.table(HH_IGFS)
table(HH_IGFS$Year)

HH_SWC<-as.data.table(HH_SWC)
table(HH_SWC$Year)

HH_ROCK<-as.data.table(HH_ROCK)
table(HH_ROCK$Year)

HH_BTS <- as.data.table(HH_BTS[HH_BTS$Year > 1986,])
x2 <- which(HH_BTS$Year <2002 & HH_BTS$Country == "DE") ## do include
HH_BTS <- HH_BTS[-x2,]
## Remove DE prior to 2002 - this wasn't on Datras yet when 
# Moriarty did there work. It does appear to be the same ship, 
# but the gear type and other params would need to be error checked 
# before inclusion

HH_NSIBTS <- as.data.table(HH_NSIBTS[HH_NSIBTS$Year >1982,])
table(HH_NSIBTS$Year, HH_NSIBTS$Quarter)
x1 <- which(HH_NSIBTS$Year <1998 & HH_NSIBTS$Quarter %in% c(3,4))
HH_NSIBTS <- HH_NSIBTS[-x1,]
## remove earlier years in Q 3/4

HH_SNS <- as.data.table(HH_SNS)
table(HH_SNS$Year)

HH_BTS8 <- as.data.table(HH_BTS8)
table(HH_BTS8$Year)

HH_DYFS <- as.data.table(HH_DYFS)
table(HH_DYFS$Year)

# merge HH data files --------------------

HH <- unique(rbind(HH_NIGFS,HH_EVHOE, HH_FRCGFS, HH_IGFS, HH_SWC, HH_ROCK, HH_BTS,
          HH_NSIBTS, HH_SNS, HH_BTS8, HH_DYFS))

# check - should return TRUE
nrow(HH) == nrow(HH_EVHOE)+nrow(HH_FRCGFS)+nrow(HH_IGFS)+
  nrow(HH_NSIBTS)+nrow(HH_ROCK)+nrow(HH_SWC)+
  nrow(HH_NIGFS)+nrow(HH_SNS)+nrow(HH_BTS)+nrow(HH_BTS8)+nrow(HH_DYFS)  
rm(HH_NIGFS, HH_EVHOE, HH_FRCGFS, HH_IGFS, HH_SWC, HH_ROCK, HH_BTS, HH_NSIBTS, HH_SNS, HH_BTS8, HH_DYFS)

## Biological data (HH) -------------

# cut each dataset to the start date used by Moriarty et al.2017 - RK 2021

HL_NIGFS<-as.data.table(HL_NIGFS)

HL_EVHOE<-as.data.table(HL_EVHOE)

HL_FRCGFS<-as.data.table(HL_FRCGFS)

HL_IGFS<-as.data.table(HL_IGFS)

HL_SWC<-as.data.table(HL_SWC)

HL_ROCK<-as.data.table(HL_ROCK)

HL_BTS<-as.data.table(HL_BTS[HL_BTS$Year> 1986,])
x4 <- which(HL_BTS$Year <2002 & HL_BTS$Country == "DE")
HL_BTS <- HL_BTS[-x4,]
names(HL_BTS)[names(HL_BTS) == 'ValidAphiaID'] <- 'Valid_Aphia'
HL_BTS <- HL_BTS[, c("RecordType", "Survey", "Quarter", "Country", "Ship", "Gear", 
                     "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", 
                     "SpecCodeType", "SpecCode", "SpecVal", "Sex", "TotalNo", "CatIdentifier", 
                     "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", 
                     "HLNoAtLngt", "DevStage", "LenMeasType", "DateofCalculation", "Valid_Aphia")]

HL_NSIBTS<-as.data.table(HL_NSIBTS[HL_NSIBTS$Year >1982,])
x3 <- which(HL_NSIBTS$Year <1998 & HL_NSIBTS$Quarter %in% c(3,4))
HL_NSIBTS <- HL_NSIBTS[-x3,]
names(HL_NSIBTS)[names(HL_NSIBTS) == 'ValidAphiaID'] <- 'Valid_Aphia'
HL_NSIBTS <- HL_NSIBTS[, c("RecordType", "Survey", "Quarter", "Country", "Ship", "Gear", 
                     "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", 
                     "SpecCodeType", "SpecCode", "SpecVal", "Sex", "TotalNo", "CatIdentifier", 
                     "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", 
                     "HLNoAtLngt", "DevStage", "LenMeasType", "DateofCalculation", "Valid_Aphia")]


HL_SNS <-as.data.table(HL_SNS[HL_SNS$Year> 1986,])

HL_BTS8 <-as.data.table(HL_BTS8)

HL_DYFS <-as.data.table(HL_DYFS)

## merge HL data files ----------------------
HL <- unique(rbind(HL_NIGFS, HL_EVHOE, HL_FRCGFS, HL_IGFS,HL_SWC, HL_ROCK, 
          HL_BTS,HL_NSIBTS,HL_SNS, HL_BTS8, HL_DYFS))

# check - should return true
nrow(HL) == nrow(HL_NIGFS)+nrow(HL_EVHOE)+nrow(HL_FRCGFS)+nrow(HL_IGFS)+
  nrow(HL_SWC)+nrow(HL_ROCK)+nrow(HL_BTS)+nrow(HL_NSIBTS)+nrow(HL_SNS)+
  nrow(HL_BTS8)+nrow(HL_DYFS)


# Remove the intermediate files to free up space
rm(HL_NIGFS, HL_EVHOE, HL_FRCGFS, HL_IGFS,HL_SWC,HL_ROCK,
   HL_BTS,HL_NSIBTS, HL_SNS, HL_BTS8, HL_DYFS)
gc()



# Add unique Hauls ID fields to each of the datasets to allow combination and changing
# This takes the structure Survey_YEAR_Quarter_Ship_HaulNo_Gear
HH$UniqueID<-paste(HH$Survey,HH$Year,HH$Quarter,HH$Ship, 
                   HH$HaulNo, HH$Gear, sep="_")
HL$UniqueID<-paste(HL$Survey,HL$Year,HL$Quarter,HL$Ship, 
                   HL$HaulNo, HL$Gear, sep="_")


## add old ship ID --------------------
### add ship_old field to make reference to previous codes easier. -June 2021
Ship_match <- read.csv("Raw_data/ShipC_TS_Ship.csv")

HH$Ship_old <- Ship_match$TS_Ship[match(HH$Ship,Ship_match$ShipC)]
HL$Ship_old <- Ship_match$TS_Ship[match(HL$Ship,Ship_match$ShipC)]

# Change -9's to NA -------------------
# All -9 should be NA as -9 represents a missing value in DATRAS

system.time(replace_function(HH))
system.time(replace_function(HL))

## HH strucutre ----------------

# Change appropriate cols to numeric and have data tables set up properlly
numCols <- c("SweepLngt", "HaulNo", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", 
             "Netopening",  "Distance", "Warplngt", "Warpdia", "WarpDen",
             "DoorSurface", "DoorWgt","DoorSpread", "WingSpread",
             "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", 
             "GroundSpeed" , "SpeedWater", "SurCurDir", "SurCurSpeed",
             "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
             "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal")

HH <- as.data.frame(HH)
for(i in names(HH)[which(names(HH) %in% numCols)]){
   HH[,i] <- as.numeric(HH[,i])
}

## HL structure --------------

numCols <- c("SweepLngt","HaulNo","Year", "SpecCode","SpecVal",
             "TotalNo","CatIdentifier","NoMeas","SubFactor", "SubWgt",
             "CatCatchWgt","LngtClass", "HLNoAtLngt","Valid_Aphia")

HL <- as.data.frame(HL)
for(i in names(HL)[which(names(HL) %in% numCols)]){
  HL[,i] <- as.numeric(HL[,i])
}


str(HH)
str(HL)

summary(HH)
summary(HL)



save(list=ls(all=T), file = "./script3_output.rda")
#load("./script3_output.rda")



