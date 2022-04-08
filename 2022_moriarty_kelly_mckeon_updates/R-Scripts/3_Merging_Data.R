# title: "3_Merging_Data.R"
# last updated: "08/04/2022"

# This is Script 3 of x 
# The purpose of this script is to merge the data into appropriate groupings for 
# data checking and processing

# load("./script2_output.rda")
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
x2 <- which(HH_BTS$Year <2002 & HH_BTS$Country == "DE")
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

HH <- rbind(HH_NIGFS,HH_EVHOE, HH_FRCGFS, HH_IGFS, HH_SWC, HH_ROCK, HH_BTS,
          HH_NSIBTS, HH_SNS, HH_BTS8, HH_DYFS)

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

HL_SNS <-as.data.table(HL_SNS[HL_SNS$Year> 1986,])

HL_BTS8 <-as.data.table(HL_BTS8)

HL_DYFS <-as.data.table(HL_DYFS)

## merge HL data files ----------------------
HL<-rbind(HL_NIGFS, HL_EVHOE, HL_FRCGFS, HL_IGFS,HL_SWC, HL_ROCK, HL_NSIBTS, 
          HL_BTS,HL_SNS, HL_BTS8, HL_DYFS)

# check - should return true
nrow(HL) == nrow(HL_EVHOE)+nrow(HL_FRCGFS)+nrow(HL_IGFS)+
  nrow(HL_NSIBTS)+nrow(HL_ROCK)+nrow(HL_SWC)+  
  nrow(HL_NIGFS)+nrow(HL_PT)+nrow(HL_BTS)+nrow(HL_SP_ARSA) + 
  nrow(HL_SP_PORC) + nrow(HL_SP_NORTH)

# Remove the intermediate files to free up space
rm(HH_EVHOE,HH_FRCGFS,HH_IGFS,HH_NSIBTS,HH_ROCK,HH_SWC,
   HH_NIGFS,HH_BTS,HH_PT,HH_SP_ARSA, HH_SP_NORTH, HH_SP_PORC,
   HL_EVHOE,HL_FRCGFS,HL_IGFS,HL_NSIBTS,HL_ROCK,HL_SWC,
   HL_NIGFS,HL_BTS,HL_PT, HL_SP_NORTH, HL_SP_PORC, HL_SP_ARSA)
gc()

# Add unique Hauls ID fields to each of the datasets to allow combination and changing
# This takes the structure Survey/YEAR/Quarter/Ship/HaulNo/Gear
##################
HH$UniqueID<-paste(HH$Survey,HH$Year,HH$Quarter,HH$Ship, 
                   HH$HaulNo, HH$Gear, sep="/")
HL$UniqueID<-paste(HL$Survey,HL$Year,HL$Quarter,HL$Ship, 
                   HL$HaulNo, HL$Gear, sep="/")


### add ship_old field to make reference to previous codes easier. -June 2021

Ship_match <- read.csv("Raw_data/ShipC_TS_Ship.csv")

head(Ship_match)

HH$Ship_old <- Ship_match$TS_Ship[match(HH$Ship,Ship_match$ShipC)]
HL$Ship_old <- Ship_match$TS_Ship[match(HL$Ship,Ship_match$ShipC)]
    
head(HH)                              
head(HL)    

#####################
# Change -9's to NA #
####################
# All -9 should be NA as -9 represents a missing value in DATRAS
# Funtion to replace multiple -9 across lots of data tables
replace_function=function(DT){
  cnames <- colnames(DT)
  for(cname in cnames) {
    set(DT, j = cname, value = gsub("[[:space:]]", "", DT[[cname]]))
  }
  for(cname in cnames){
    set(DT, i = which(DT[[cname]] == -9), j = cname, value = NA)
  }
}
system.time(replace_function(HH))
system.time(replace_function(HL))

####################################
## Sort out data frame structure HH#
####################################
cnames <- colnames(HH)
for(cname in cnames) {
  set(HH, j = cname, value = gsub("[[:space:]]", "", HH[[cname]]))
}
str(HH)

names(HH)

# Change appropriate cols to numeric and have data tables set up properlly
numCols <- c("SweepLngt", "HaulNo", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", 
             "Netopening",  "Distance", "Warplngt", "Warpdia", "WarpDen",
             "DoorSurface", "DoorWgt","DoorSpread", "WingSpread",
             "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", 
             "GroundSpeed" , "SpeedWater", "SurCurDir", "SurCurSpeed",
             "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
             "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal",
             "ThermoCline", "ThClineDepth", "DateofCalculation")
HH[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]

####################################
## Sort out data frame structure HL#
####################################
# Change appropriate cols to numeric and have data tables set up properlly
cnames <- colnames(HL)
for(cname in cnames) {
  set(HL, j = cname, value = gsub("[[:space:]]", "", HL[[cname]]))
}
str(HL)
names(HL)
names(HL)[29] <- "ValidAphiaID" ### in previous versions this is what the column seems to have been called (RK0)

numCols <- c("SweepLngt","HaulNo","Year", "SpecCode","SpecVal",
             "TotalNo","CatIdentifier","NoMeas","SubFactor", "SubWgt",
             "CatCatchWgt","LngtClass", "HLNoAtLngt","ValidAphiaID")

#check - should return 0
setdiff(numCols, names(HL))

HL[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols] ### 

## if the above doesn't run, you can force it to inelegantly like so ;)
# HL$SweepLngt <- as.numeric(HL$SweepLngt)
# HL$HaulNo <- as.numeric(HL$HaulNo)
# HL$Year <- as.numeric(HL$Year)
# HL$SpecCode <- as.numeric(HL$SpecCode)
# HL$SpecVal<- as.numeric(HL$SpecVal)
# HL$TotalNo <- as.numeric(HL$TotalNo)
# HL$CatIdentifier <- as.numeric(HL$CatIdentifier)
# HL$NoMeas <- as.numeric(HL$NoMeas)
# HL$SubFactor <- as.numeric(HL$SubFactor)
# HL$SubWgt <- as.numeric(HL$SubWgt)
# HL$CatCatchWgt <- as.numeric(HL$CatCatchWgt)
# HL$LngtClass <- as.numeric(HL$LngtClass)
# HL$HLNoAtLngt <- as.numeric(HL$HLNoAtLngt)
# HL$ValidAphiaID <- as.numeric(HL$ValidAphiaID)


# all offending -9 should now be gone from all my data tables
# check summarys and insure the data sturcture is sound


str(HH)
str(HL)

summary(HH)

### one record with crazy negative distance - set to NA 
# HH$Distance[which(HH$Distance ==-16668)] <- NA

summary(HL)

## God is a DJ; love is a graph
par(mfrow = c(3,3))


# HHx <- as.data.frame(HH)
# ## Numeric variables
# for (i in names(Filter(is.numeric, HHx))) {
#   hist(HHx[,i],
#        breaks = 3000,
#        main = paste(i),
#        xlab = paste(i))
# }
# 
# HLx <- as.data.frame(HL)
# ## Numeric variables
# for (i in names(Filter(is.numeric, HLx))) {
#   hist(HLx[,i],
#        breaks = 3000,
#        main = paste(i),
#        xlab = paste(i))
# }

# if all is good- move on, if not rerun numeric col stuff again after applying the
# NA replace_function as this might mess with the structure.

### all -9's are gone! Hooray! but some odd 0's in the  measures.. deal with in later scripts..

# ## Ruth's function for removing rows where all values are NA. CM
# rem_na_df <- function(dat_fr = mydata) {
#   
#   # Create function which counts the number of NA's in each row. 
#   countNAs <- function(x) {length(which(is.na(x)))}
#   numNAs <- apply(dat_fr,1,countNAs)
#   # apply this function to each row (2nd argument = 1) in the dataset using apply, 
#   # and store this in a new vector called numNAs 
#   
#   nrow(dat_fr[numNAs == ncol(dat_fr),])
#   
#   #  Delete all blank rows by Deleting rows where all cells are NA's 
#   #  These will be rows where numNAs is equal to the number col of the dataframe
#   dat_fr <- dat_fr[numNAs != ncol(dat_fr),]
#   
# }
# 
# HH <- rem_na_df(HH)
# HL <- rem_na_df(HL)

save(list=ls(all=T), file = "./script3_output.rda")
#load("./script3_output.rda")



