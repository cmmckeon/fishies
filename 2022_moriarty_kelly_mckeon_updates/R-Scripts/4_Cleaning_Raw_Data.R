# title: "4_Cleaning_Raw_Data.R"
# last updated: "08/04/2022"

# This is Script 4 of 7
# The purpose of this script is to check the merged datasets and insure all changes  
# mentioned in the error trapping documentation are transfered into the data product

## set up
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

#load("./script3_output.rda")


## Add in the Corrections sent directly to Meadhbh 
# The first step in the correction procedure is to add in any missing data
# these are the datasets that were stored loaded up during script 2_Loading_data


# Correction 1: Northern Ireland Data 1992 - 2007 ------------
# this data is not currently available on DATRAS

# First split HH and HL
NI_HH<-subset(NI_extra, V1=="HH", )
NI_HL<-subset(NI_extra, V1=="HL", )

## HH ----------------

# Sort out the Col headings
names(NI_HH)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                "SweepLngt", "GearEx", "DoorType", "StNo" , "HaulNo" , 
                "Year", "Month", "Day", "TimeShot", "DepthStratum" ,"HaulDur",
                "DayNight", "ShootLat", "ShootLong","HaulLat", "HaulLong",
                "StatRec", "Depth","HaulVal","HydroStNo","StdSpecRecCode",
                "BySpecRecCode","DataType", "Netopening","Rigging","Tickler",
                "Distance","Warplngt","Warpdia","WarpDen","DoorSurface","DoorWgt",
                "DoorSpread","WingSpread","Buoyancy","KiteDim","WgtGroundRope","TowDir",
                "GroundSpeed","SpeedWater","SurCurDir","SurCurSpeed","BotCurDir",
                "BotCurSpeed","WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp",
                "BotTemp","SurSal","BotSal","ThermoCline", "ThClineDepth")

NI_HH$Survey<-"NIGFS"
NI_HH$DateofCalculation<-"NA"


summary(NI_HH)

# Haul Durations recorded as decimal hour - change to mins (*60)
NI_HH$HaulDur<-NI_HH$HaulDur*60

# Now look at HL file
summary(NI_HL)
names(NI_HL)<-c("RecordType", "Quarter","Country",          
                "Ship", "Gear","SweepLngt", "GearEx",         
                "DoorType", "StNo","HaulNo","Year",        
                "SpecCodeType","SpecCode","SpecVal","Sex",              
                "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                "HLNoAtLngt")
keepers<-c("RecordType", "Quarter","Country",          
           "Ship", "Gear","SweepLngt", "GearEx",         
           "DoorType", "StNo","HaulNo","Year",        
           "SpecCodeType","SpecCode","SpecVal","Sex",              
           "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
           "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
           "HLNoAtLngt")
NI_HL<-NI_HL[keepers]
NI_HL$Survey<-"NIGFS"
NI_HL$DateofCalculation<-"NA"
NI_HL$Valid_Aphia <-NI_HL$SpecCode 

# Add unique ID code
NI_HH$UniqueID<-paste(NI_HH$Survey, NI_HH$Year,NI_HH$Quarter, 
                      NI_HH$Ship, NI_HH$HaulNo, NI_HH$Gear, sep="_")
NI_HL$UniqueID<-paste(NI_HL$Survey, NI_HL$Year, NI_HL$Quarter, 
                      NI_HL$Ship, NI_HL$HaulNo, NI_HL$Gear, sep="_")

# Add ship column
NI_HH$Ship_old <- NI_HH$Ship
NI_HL$Ship_old <- NI_HL$Ship

HH1 <- unique(rbind.fill(HH, NI_HH))

### Co is the corystes vessel code should be 
HH1$Ship <- as.character(HH1$Ship) ## making sure we don't generate NAs with unknown factor levels. CM
HH1$Ship[HH1$Ship == "CO"] <- "74RY" 
HH1$Ship[HH1$Ship == "7.4e+10"] <- "74E9"
HH1$Ship[HH1$Ship == "LF"] <- "74LG"
HH1$Ship <- factor(HH1$Ship)

HH1$UniqueID<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship, 
                    HH1$HaulNo, HH1$Gear, sep="_")

## HL -------------

setdiff(names(HL), names(NI_HL)) ## new fields in Datras not in NI
setdiff(names(NI_HL),names(HL)) ## nothing in NI that's not in Datras data

HL1<- unique(rbind.fill(HL,NI_HL))

table(HL1$Ship)

### Co is the corystes vessel code should be 
HL1$Ship <- as.character(HL1$Ship) ## making sure we don't generate NAs with unknown factor levels. CM
HL1$Ship[HL1$Ship == "CO"] <- "74RY" 
HL1$Ship[HL1$Ship == "7.4e+10"] <- "74E9"
HL1$Ship[HL1$Ship == "LF"] <- "74LG"
HL1$Ship <- factor(HL1$Ship)

HL1$UniqueID<-paste(HL1$Survey,HL1$Year,HL1$Quarter,HL1$Ship, 
                   HL1$HaulNo, HL1$Gear, sep="/")

# remove intermediate datasets
rm(NI_extra, NI_HH, NI_HL, keepers)


# Correction 2: DENMARK IBTS CORRECTIONs/Additions ------------
# Denmark data for the earlier IBTS for 1983, 1984, 1995, 1986

NS_DEN<-rbind(NS_DEN_sp_1986,NS_DEN_sp_1985,
              NS_DEN_sp_1984,NS_DEN_sp_1983, fill=TRUE)

NS_DEN_add_HH<-subset(NS_DEN, V1=="HH",)
NS_DEN_add_HL<-subset(NS_DEN, V1=="HL",)

# remove the intermediate datasets
rm(NS_DEN_sp_1983, NS_DEN_sp_1984, NS_DEN_sp_1985, NS_DEN_sp_1986)

## HH ---------------
summary(NS_DEN_add_HH)

# Sort out the Col headings
names(NS_DEN_add_HH)<-c("RecordType", "Quarter", "Country" , "Ship" , "Gear" , 
                        "SweepLngt", "GearEx",  "DoorType", "StNo" , "HaulNo" , "Year",
                        "Month"    ,         "Day"           ,    "TimeShot"     ,     "DepthStratum"          ,
                        "HaulDur"   ,        "DayNight"      ,    "ShootLat"      ,    "ShootLong"        ,
                        "HaulLat"    ,       "HaulLong"      ,    "StatRec"        ,   "Depth"            ,
                        "HaulVal"     ,      "HydroStNo"     ,    "StdSpecRecCode" ,   "BySpecRecCode"   ,
                        "DataType"     ,     "Netopening"    ,    "Rigging"         ,  "Tickler"          ,
                        "Distance"      ,    "Warplngt"      ,    "Warpdia"        ,   "WarpDen"          ,
                        "DoorSurface"    ,   "DoorWgt"       ,    "DoorSpread"     ,   "WingSpread"       ,
                        "Buoyancy"        ,  "KiteDim"       ,    "WgtGroundRope"  ,   "TowDir"           ,
                        "GroundSpeed"      , "SpeedWater"    ,    "SurCurDir"      ,   "SurCurSpeed"      ,
                        "BotCurDir"         ,"BotCurSpeed"     ,  "WindDir"        ,   "WindSpeed"        ,
                        "SwellDir",         "SwellHeight"    ,   "SurTemp"        ,   "BotTemp"          ,
                        "SurSal",          "BotSal"        ,    "ThermoCline"    ,   "ThClineDepth"     )

NS_DEN_add_HH$Survey<-"NS-IBTS"
NS_DEN_add_HH$DateofCalculation<-"NA"
names(NS_DEN_add_HH)

NS_DEN_add_HH$Ship_old <- "DAN2"
NS_DEN_add_HH$Ship <-"26D4"


setdiff(names(HH), names(NS_DEN_add_HH)) ## new fields in Datras
setdiff(names(NS_DEN_add_HH),names(HH)) ## nothing that's not in Datras data

## HL -----------------
names(NS_DEN_add_HL)<-c("RecordType", "Quarter","Country",          
                        "Ship", "Gear","SweepLngt", "GearEx",         
                        "DoorType", "StNo","HaulNo","Year",        
                        "SpecCodeType","SpecCode","SpecVal","Sex",              
                        "TotalNo", "CatIdentifier" ,"NoMeas", "SubFactor",        
                        "SubWgt","CatCatchWgt", "LngtCode","LngtClass" ,       
                        "HLNoAtLngt","x1", "x1", "x1", "x1", "x1", "x1", 
                        "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", "x1", "x1","x1", "x1",
                        "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", "x1", "x1","x1", "x1", 
                        "x1", "x1", "x1", "x1", "x1")

NS_DEN_add_HL$Survey<-"NS-IBTS"
NS_DEN_add_HL$DateofCalculation<- "NA"
NS_DEN_add_HL$Valid_Aphia <- NS_DEN_add_HL$SpecCode 
NS_DEN_add_HL <- NS_DEN_add_HL[,which(names(NS_DEN_add_HL) != "x1")]

NS_DEN_add_HL$Ship_old <- NS_DEN_add_HL$Ship
NS_DEN_add_HL$Ship <- "26D4"

# Add unique ID code
NS_DEN_add_HH$UniqueID<-paste(NS_DEN_add_HH$Survey,
                              NS_DEN_add_HH$Year,NS_DEN_add_HH$Quarter, 
                              NS_DEN_add_HH$Ship, NS_DEN_add_HH$StNo, 
                              NS_DEN_add_HH$Gear, sep="_")
NS_DEN_add_HL$UniqueID<-paste(NS_DEN_add_HL$Survey,
                              NS_DEN_add_HL$Year,NS_DEN_add_HL$Quarter, 
                              NS_DEN_add_HL$Ship, NS_DEN_add_HL$StNo, 
                              NS_DEN_add_HL$Gear, sep="_")
unique(HH1$Survey)
unique(HH1$Country)


Den_ibts<-subset(HH1, Survey=="NS-IBTS"&Country=="DK"&Year<1987&Year>1982,)
# 168 observations
# NS_Den_add_HH has only got 168 obs
remove<-Den_ibts$UniqueID
# # lets check if they are different
replace_function(NS_DEN_add_HH)
test<-compare_function(Den_ibts, NS_DEN_add_HH)
test1<-compare_function(NS_DEN_add_HH, Den_ibts)

list_a <- list()
list_b <- list()
for(i in names(NS_DEN_add_HH)){
        list_a[[i]] <- setdiff(Den_ibts[,i], NS_DEN_add_HH[,i])
        list_b[[i]] <- setdiff(NS_DEN_add_HH[,i], Den_ibts[,i])
}

# in the file directly from Denmark there are problems with the distance and
# and lat long measurements so stick with the DATRAS version of the haul Chron File

# lets look at the HL file too
Den_ibts_fish<-subset(HL1, UniqueID%in%remove,)
# 12356 obs in Datras file
# 13822 obs in denmarks file
# lest see haw they differ
replace_function(NS_DEN_add_HL)
names(NS_DEN_add_HL)
test<-compare_function(Den_ibts_fish, NS_DEN_add_HL)
test1<-compare_function(NS_DEN_add_HL, Den_ibts_fish)

list_a <- list()
list_b <- list()
for(i in names(NS_DEN_add_HL)){
        list_a[[i]] <- setdiff(Den_ibts_fish[,i], NS_DEN_add_HL[,i])
        list_b[[i]] <- setdiff(NS_DEN_add_HL[,i], Den_ibts_fish[,i])
}

# clear differences in SpecCodeType, SpecCOde, SubWght, LenghtCode/class
# remove these to see if the spp list of additiona spp shines through


copy1<-subset(Den_ibts_fish,
                 select=c(StNo,  Year, UniqueID))
copy_den<-subset(NS_DEN_add_HL,
                 select=c(StNo, Year, UniqueID))
list_a <- list()
list_b <- list()
for(i in names(copy_den)){
        list_a[[i]] <- setdiff(copy1[,i], copy_den[,i])
        list_b[[i]] <- setdiff(copy_den[,i], copy1[,i])
}

test<-compare_function(copy_den, copy1)
test1<-compare_function(copy1, copy_den)
# now only have extra data in the test, not the test1,
# all stations and unique ID StNo relapces Haul No in the DEN_add 
# datasets. They are the same except for Unique ID NS-IBTS/1986/1/DAN2/30/GOV which 
# is only in the DATRAS copy not the Denmark copy 
# neither copy is perfect - retain DATRAS
# then find the records of species that are missing from DATRAS 
names(Den_ibts_fish)
copy1<-subset(Den_ibts_fish,
                 select=c(StNo,  Year, UniqueID, Valid_Aphia))
copy_den<-subset(NS_DEN_add_HL,
                 select=c(StNo, Year, UniqueID, Valid_Aphia))
test<-compare_function(copy_den, copy1)
test1<-compare_function(copy1, copy_den)
# 637 records in denmark not in DATRAS - this makes sense
# 198 records in DATRAS not in denmark!?!
# complete mis-match
summary(as.factor(test$Year))
summary(as.factor(test1$Year))
summary(as.factor(test$Valid_Aphia))

# 56 spp missing from records
summary(as.factor(test1$Valid_Aphia))

setdiff(Den_ibts_fish$Valid_Aphia, NS_DEN_add_HL$Valid_Aphia)
# 0 in Datras not in supplement

length(setdiff(NS_DEN_add_HL$Valid_Aphia, Den_ibts_fish$Valid_Aphia))
# 62 in supplement not on Datras

# total mess!
# Gonna add in the missing records in the Denmark files and flag them
Den_ibts_fish$match<-paste(Den_ibts_fish$UniqueID,Den_ibts_fish$Valid_Aphia, sep="_")
NS_DEN_add_HL$match<-paste(NS_DEN_add_HL$UniqueID,NS_DEN_add_HL$Valid_Aphia, sep="_")
list<-Den_ibts_fish$match
Den_HL_additions<-subset(NS_DEN_add_HL, match%in%list)

# these are my additions for the HL-gov file
# leave the match field as a flag for now - issues with these data that 
# remain unresolved
HL2<-rbind.fill(HL1, Den_HL_additions)
summary(as.factor(HL2$match))

#remove intermediate datasets
rm(Den_HL_additions, Den_ibts,Den_ibts_fish, test, copy_den, copy1, test1, remove, list_a, list_b,
   NS_DEN, NS_DEN_add_HL, NS_DEN_add_HH, HH, HL, HL1)

### end denmark check -----------


### check country codes

table(HH1$Survey, HH1$Country, useNA = "ifany")

HH1[is.na(HH1$Country),] ## Ship code AA36 also refers to unspecified vessel ? Remove?


### add old IDs for reference.. 2021

HH1$UniqueID<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship, 
                    HH1$HaulNo, HH1$Gear, sep="_")

HH1$UniqueIDP<-paste(HH1$Survey,HH1$Year,HH1$Quarter,HH1$Ship_old, 
                    HH1$HaulNo, HH1$Gear, sep="_")



## Appendix Error Trapping Q and A ------------------

# To check that each error/question raised is addressed, everything listed in the
# Appendix must be rechecked and verified as corrected at source(already on DATRAS)
# or alternatively fixed here with a line of code

# 1.1.1	The First Quarter International Bottom Trawl Survey (GNSIntOT1) 

## NORWAY NS-IBTS Q1 ------------------

# Station inconsistent with other areas sampled in that year, 
# is this the correct Latitude and Longitude. 
# JD: it is possible this was incorrectly recorded and should 
# be 57.24.00 instead.
HH1$ShootLat[HH1$UniqueID=="NS-IBTS_1986_1_58EJ_41_GOV"] <-57.24



## EnglandNS-IBTS Q1 ---------------------

# The following stations have outlying net opening values, 
# can you verify that these outliers are true values (figure 1.1.1.13/14).
# response was to use all but one - changed below
HH1$Netopening[HH1$UniqueID=="NS-IBTS_1998_3_CIR_66_GOV"]#<-NA ## no ship with this name
HH1$Netopening[HH1$UniqueID=="NS-IBTS_1998_3_74CZ_66_GOV"]#  <- NA  
#value is currently 3.4, doesn't seem like an outlier now


## Denmark NS-IBTS Q1 -------------------

# In figure 1.1.1.15 can you explain why the sweep length 
# varies so much in the early 1990’s? 
# check has this been removed
plot(HH1$Depth[HH1$Country=="DK"], HH1$SweepLngt[HH1$Country=="DK"],
     pch=19)
# warp values are still recorded in place of sweeps
# great craic, lets fix this
den<-HH1[HH1$Country=="DK"& HH1$SweepLngt>111,]
summary(as.factor(den$Year))
# gotta catch them all - range from 1991 to 1995 425obs
# want to move the warp lengths to the right spot too
list<-den$UniqueID
HH1$Warplngt[HH1$UniqueID%in%list]<-HH1$SweepLngt[HH1$UniqueID%in%list]
HH1$SweepLngt[HH1$UniqueID%in%list]<-NA

# Question: In 1998 the doorspread is have quite large values 
# in depth range of 74m to 137m. Are these values the same as in 
# your dataset, and do you know why this has occurred?
# KW: Incorrect data make -9 (NA)

list<- c( 'NS-IBTS_1998_1_26D4_14_GOV','NS-IBTS_1998_1_26D4_15_GOV',
          'NS-IBTS_1998_1_26D4_16_GOV','NS-IBTS_1998_1_26D4_46_GOV',
          'NS-IBTS_1998_1_26D4_9_GOV','NS-IBTS_1998_1_26D4_13_GOV',
          'NS-IBTS_1998_3_26D4_24_GOV','NS-IBTS_1998_3_26D4_49_GOV',
          'NS-IBTS_1998_3_26D4_45_GOV','NS-IBTS_1998_3_26D4_48_GOV')

HH1$DoorSpread[HH1$UniqueID%in%list]<-NA

# outliers removed


## Netherlands IBTS Q1  ----------------------

# For station with unique ID 2004/1/TRI2/33/GOV the door spread 
# is 118m at a depth of 64m. Is the door spread value correct?
# IdB: We agree it is an outlier, but we cannot find the hard 
# copies for hauls 1-38 in 2004 so we’re not able to check
# Decision - remove outlier
HH1$DoorSpread[HH1$UniqueID=="NS-IBTS_2004_1_64T2_33_GOV"] <-NA

# Question 1: 1984/1/TRI/4/GOV
# NetOpening is 8m at depth of 78m. This is an outlier, 
# but is it a possible value?	
# IdB: Change into 6, the hard copy is not too clear 
HH1$Netopening[HH1$UniqueID=="NS-IBTS_1984_1_64T0_4_GOV"] <-6


## GERMANY NS-IBTS -----------------------

# Question: sweep of 80m for H18 gear 
funnysweep<-subset(HH1, c(HH1$Country=="DE"& HH1$Survey=="NS-IBTS"& Year=="1984" & Gear == "H18"),)
# no need to worry as H18 gear will be removed in the standard gear type.
funnysweep$UniqueID
HH1$SweepLngt[HH1$UniqueID%in%funnysweep$UniqueID]<-NA

# Question: Doorspread is not consistent with depth, 
# can you verify that these outliers are the correct values?
# MK: Failures in the scanmar 
list<-c("NS-IBTS_2011_1_06NI_14_GOV", "NS-IBTS_2014_1_06NI_11_GOV", 
        "NS-IBTS_2014_1_06NI_7_GOV", "NS-IBTS_2014_1_06NI_8_GOV")
HH1$DoorSpread[HH1$UniqueID%in%list]<-NA

# Question: Why was the ship SOL used in Q1, 1992, 
# and why was the area surveyed so different in this year?        
# MK: SOL shouldn't be here. GOV is non standard and the data
# shouldn't be included in DATRAS. 

table(HH1$Ship[HH1$Country == "DE" & HH1$Year == 1992])

delete_ship<-subset(HH1, HH1$Year=="1992"&
                      HH1$Country=="DE"
                    & HH1$Ship=="06S1",)
list<-(delete_ship$UniqueID)

HH1<-subset(HH1, !HH1$UniqueID%in%list, )

## France NS-IBTS Q1 -----------------

#Wingspread is not consistent with depth, 
#can you verify that these outliers are true values. 
# YV: Values are too hight, delete and estimate based on model.
# Wingspread normally 15m 

list<-c("NS-IBTS_1993_3_THA_53_GOV", "NS-IBTS_1995_1_THA_29_GOV")
HH1$WingSpread[HH1$UniqueIDP%in%list]<-NA
# 1995 record changed to 16
# remove 1993 record only
HH1$WingSpread[HH1$UniqueIDP=="NS-IBTS_1995_1_THA_29_GOV"]<-16

# Netopening is not consistent with depth, 
# can you verify that these outliers are true values. 
# YV/FC: Incorrect use -9 
list<-c("NS-IBTS_1993_3_THA_9_GOV", "NS-IBTS_1994_1_THA_31_GOV")
HH1$Netopening[HH1$UniqueIDP%in%list] <- NA

# Doorspread is not consistent with depth, 
# can you verify that these outliers are true values. 
# YV/FC: Need to be recalculated
list<-c( 'NS-IBTS_1996_3_THA2_21_GOV','NS-IBTS_1996_3_THA2_20_GOV',
         'NS-IBTS_1997_1_THA2_1_GOV','NS-IBTS_1997_1_THA2_6_GOV',
         'NS-IBTS_1997_1_THA2_8_GOV','NS-IBTS_1997_1_THA2_41_GOV',
         'NS-IBTS_2000_1_THA2_56_GOV','NS-IBTS_2002_1_THA2_29_GOV',
         'NS-IBTS_2010_1_THA2_21_GOV','NS-IBTS_2010_1_THA2_26_GOV',
         'NS-IBTS_2010_1_THA2_29_GOV','NS-IBTS_2015_1_THA2_38_GOV')
HH1$DoorSpread[HH1$UniqueIDP%in%list] <- NA

# Depth of 61m netopening 9.8m. This is an outlier,
# can you check that the value is correct.
# YV/FC: Incorrect value
HH1$Netopening[HH1$UniqueIDP=='NS-IBTS_2011_1_THA2_74_GOV']<-NA


## Third Quarter International Bottom Trawl Survey (GNSIntOT3) ------------------

## NORWAY NS-IBTS Q3 --------------------

# Question 3: haul duration
# JD: 2008/3/JHJ/259/GOV: this is not an IBTS haul. 
# Was made for another purpose (should be coded HaulVal=I).         
# 2008/3/JHJ/269/GOV: it looks like tow time was calculated 
# incorrectly. Distance =2.4 km, speed at 3.4 n mi = 6.297 km/hr: 
# 2.4/6.297*60 = 22.9 minutes
HH1$HaulVal[HH1$UniqueIDP=="NS-IBTS_2008_3_JHJ_259_GOV"]<-"I"
HH1$HaulDur[HH1$UniqueIDP=="NS-IBTS_2008_3_JHJ_269_GOV"]<-23
# One sample has a 60 min duration, is there a reason for this?
# JD: looks to be an error. Distance/speed*60 = 32.4 minutes
HH1$HaulDur[HH1$UniqueIDP=="NS-IBTS_1999_3_MIC_619_GOV"]<-32

# 2011/3/JHJ/242/GOV	Netopening is 8.8, at a depth of 64m, 
# this net opening is an outlier, is the value correct?	
# JD: Unlikely to be correct. No correction (NA).
# 1999/3/MIC/591/GOV	Netopeing is 7.7m depth 47m, this net opening
# is a meter more than the next largest value. 
# Is this an acceptable value for this ship?	
# Unlikely to be correct. No correction (NA).
list<-c("NS-IBTS_2011_3_JHJ_242_GOV", "NS-IBTS_1999_3_MIC_591_GOV")
HH1$Netopening[HH1$UniqueIDP%in%list] <- NA


## England NS-IBTS Q3 --------------

# At station with unique ID 2000/3/CIR/17/GOV the shoot and haul lat
# and long are the same values, can you check this please?
# G.B  Original data available. Haul long Input incorrectly. Change posn
# Shot: 54.639; 5.501
# Haul: 54.641; 5.564
check<-HH1[HH1$UniqueIDP=="NS-IBTS_2000_3_CIR_17_GOV",]

# need to change all postional data
 HH1$ShootLat[HH1$UniqueIDP=="NS-IBTS_2000_3_CIR_17_GOV"]<-54.639
 HH1$ShootLong[HH1$UniqueIDP=="NS-IBTS_2000_3_CIR_17_GOV"]<-5.501
 HH1$HaulLat[HH1$UniqueIDP=="NS-IBTS_2000_3_CIR_17_GOV"]<-54.641
 HH1$HaulLong[HH1$UniqueIDP=="NS-IBTS_2000_3_CIR_17_GOV"]<-5.564
 
## Fourth Quarter French Channel Groundfish Survey  (GNSFraOT4) ------------

# quick check from Appendix 1 Figure 1.1.3.3 which highlights a haul duration of
# 90 minutes in the haul with unique ID 1995/4/GWD/49/GOV. Is this correct?
# Y.V & F.C:  This should be 30 mins not 90 mins.
# haul not corrected

 HH1$HaulDur[HH1$UniqueIDP=="FR-CGFS_1995_4_GWD_49_GOV"]<-30

# Figure 1.1.3.4, shows stations in 2012 which have been recorded with
# shoot positions on land. What is the correct shoot positions?
# Y.V & F.C:  This is clearly an error; I will check the real values and 
# send them to you.
plot(HH1$ShootLong[HH1$Survey=="FR-CGFS"], 
     HH1$ShootLat[HH1$Survey=="FR-CGFS"],
     pch=19)
abline(h=49.5, col="grey")
abline(h=50, col="grey")
abline(h=50.5, col="grey")
abline(h=51, col="grey")
abline(v=-1, col="grey")
abline(v=0, col="grey")
abline(v=1, col="grey")
abline(v=2, col="grey")

# check Stat Rec is right?
landlocked<-HH1[HH1$StatRec=="27F0",]
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_49_GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_49_GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_50_GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_50_GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_51_GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_51_GOV"],
       pch=19, col="red")
landlocked<-HH1[HH1$StatRec=="28F0",]
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_52_GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_52_GOV"],
       pch=19, col="red")
points(HH1$ShootLong[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_53_GOV"], 
       HH1$ShootLat[HH1$UniqueIDP=="FR-CGFS_2012_4_GWD_53_GOV"],
       pch=19, col="red")
list<-c("FR-CGFS_2012_4_GWD_49_GOV", "FR-CGFS_2012_4_GWD_50_GOV",
        "FR-CGFS_2012_4_GWD_51_GOV", "FR-CGFS_2012_4_GWD_52_GOV",
        "FR-CGFS_2012_4_GWD_53_GOV")
HH1$StatRec[HH1$UniqueIDP%in%list]<-c("27E9", "27E9", "28E9", "28E9" ,"28E9")
# find stations on land
HH1$HaulLong[HH1$UniqueIDP%in%list]<-as.numeric(HH1$HaulLong[HH1$UniqueIDP%in%list])*-1
HH1$ShootLong[HH1$UniqueIDP%in%list]<-as.numeric(HH1$ShootLong[HH1$UniqueIDP%in%list])*-1


# The Celtic Seas  --------------

## First Quarter Northern Irish Groundfish Survey (CSNIrOT1)#

# In figure 1.2.4.1 there are outliers in the following door spread values:
# •	Unique ID: 2008_1_COR_15_ROT, Depth 48 m Door spread 26.2 m
# •	Unique ID: 2009_1_COR_37_ROT, Depth 62 m Door spread 31.3 m
# Can you check these please?
# M.L: Unique ID: 2008_1_COR_15_ROT, Door spread corrected to 34.2 m 
# and Unique ID: 2009_1_COR_37_ROT, Door spread corrected to 38.3 m
check<-HH1[HH1$UniqueIDP=="NIGFS_2009_1_COR_15_ROT",]
## these values are now 37.1 and 38.5 repectively ....CM



# The Celtic Seas BTS --------------

cols<-rainbow(19)
summary(as.factor(HH1$Year[HH1$Ship=="74RY"]))
summary(as.factor(HH1$Country))
plot(HH1$ShootLong[HH1$Ship=="74RY"], HH1$ShootLat[HH1$Ship=="74RY"],
     col=cols[as.factor(HH1$Year[HH1$Ship=="74RY"])], pch=19)
# some beams not being used at this stage 

# the inshore survey which normally occured on the CAR was on the COR 
# on a couple of occasions, this needs to be removed - not a full survey series.
abline(v=c(-8:3), col="lightgrey")
abline(h=c(48:55), col="lightgrey")
abline(h=c(48.5:54.5), col="lightgrey")
# 6 ices rectangles have data that shouldn't be part of survey time series.
list<-c("28E5","28E6","29E6","29E7","30E6","30E7")
points(HH1$ShootLong[HH1$StatRec%in%list], 
       HH1$ShootLat[HH1$StatRec%in%list],
       col=cols[as.factor(HH1$Year[HH1$StatRec%in%list])], pch=19)
names(HH1)
summary(as.factor(HH1$Survey))

# remove_stations<-subset(HH1, StatRec%in%list & Ship=="74RY" & Survey=="BTS",) ### this needs to be double checked, as time series for 
# ### "BTS-VII" is no longer separate from "BTS" - RK 2021 .... CM needs to check

HH2 <- HH1
# list<-(remove_stations$UniqueID)
# HH2<-subset(HH1, !UniqueID%in%list,)
# nrow(HH1)-nrow(HH2)
#removes 2 hauls from time series
plot(HH2$ShootLong, HH2$ShootLat,
     col=cols[as.factor(HH2$Ship)], pch=19)


# ALL HAUL CORRECTION CHECKED AND COMPELTED  
# NOW HAVE BEST AVAILABLE DATA ACCORDING TO  
# NATIONAL DATA PROVIDERS ACROSS ALL SURVEYS 


## end haul checks --------------------



# Check out all biological data to insure corrections#
# are in datras/fix for product only base on answers #

HL2$UniqueIDP<-paste(HL2$Survey,HL2$Year,HL2$Quarter,HL2$Ship_old, 
                     HL2$HaulNo, HL2$Gear, sep="_")



## Appendix Error Trapping Q and A --------------

# To check that each error/question raised is addressed, everything 
# listed in the Appendix must be rechecked and verified as corrected at 
# source(already on DATRAS)
# or alternatively fixed here with a line of code

# First Quarter International Bottom Trawl Survey (GNSIntOT1) ------------

# Bathyraja brachyurops (Fowler, 1910) # AphiaID: 271509 
# mismapped  should be Raja brachyura (Lafont, 1871) # AphiaID: 367297  
HL2$Valid_Aphia[HL2$Valid_Aphia=="271509"]<-"367297"

# Scomber japonicus Houttuyn, 1782 # AphiaID: 127022 
# mis id of Scomber colias Gmelin, 1789 # AphiaID: 151174 
HL2$Valid_Aphia[HL2$Valid_Aphia=="127022"]<-"151174"
names(HL2)
summary(as.factor(HL2$Valid_Aphia))



# EnglandNS-IBTS Q1 --------------

# Figure 1.1.1.40 shows a record of the gulper shark (Valid Aphid: 105899,
#Species: Centrophorus granulosus) station with unique ID 2006/3/END/106/GOV 
# The gulper shark is not traditionally found in North Sea, can you confirm
# this record please.
summary(as.factor(HL2$Valid_Aphia))
        
check<-subset(HL2, Valid_Aphia==105899,)
check
# still there as an observed only record!

rm(landlocked, den, belgium, Car_ship, check, delete_ship, find, funnysweep, HH1)

# ALL BIOLOGICAL CORRECTIONS ADDRESSED        
# NOW HAVE BEST AVAILABLE DATA ACCORDING TO  
# NATIONAL DATA PROVIDERS ACROSS ALL SURVEYS 


save(list=ls(all=T), file = "./script4_output.rda")
#load("./script4_output.rda")

