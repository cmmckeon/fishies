# title: "5_Define_Survey_Standards.R"
# last updated: "08/04/2022"

# This is Script 5 of 7
# The next step is to 'define' the surveys - after the genuine errors have been 
# checked/changes e.g. a haul recorded as 90mins but was actually 30mins would be
# deleted if this step occurred befor the checks are done.

## set up
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

#load("./script4_output.rda")


# Select Columns ------------

# manually remove the individual countries data sets and work on data combined by gear, 
# see script 3_merge_datasets for details on how this is done 
# check data table names

# 2 data tables 1 HH and 1 HL, 
# we will focus on HH for the moment
## check the structure of these data tables
# note numeric cols are correct


## Overview of the M & A Process

# In section 1.2  (Moriarty & Greenstreet, 2016)
# We show the processes undertaken to standardise/exclude hauls from the data set
# Is the haul valid? yes retain, no remove


invalidhauls<-subset(HH2, HaulVal=="I", ) # 1477 obs
write.csv(invalidhauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/invalid_removed.csv")

names(HH2)

summary(as.numeric(factor(HH2$TimeShot)))

summary(as.factor(HH2$HaulVal))
hauls <- subset(HH2, HH2$HaulVal=='V',)

# check this has deleted all the Invalid hauls
summary(as.factor(hauls$HaulVal))
summary(as.numeric(factor(hauls$TimeShot)))

# we can uneversially drop cols that are not of use to us going forward
# RecordType is HH for all - delete
# other cols deleted: Warpdia, WarpDen, DoorSurface, DoorWgt, Buoyancy,
# KiteDim, WgtGroundRope, TowDir, SurCurDir, SurCurSpeed, BotCurDir, BotCurSpeed,
# WindDir, WindSpeed, SwellDir, SwellHeight, SurTemp, BotTemp, SurSal, BotSal,
# ThermoCline, ThClineDepth, DateofCalculation

keepers<-c("Survey","Quarter","Country", "Ship","Gear", "SweepLngt","GearEx",          
           "DoorType","StNo","HaulNo","Year","Month", "Day","TimeShot","DepthStratum",
           "HaulDur","DayNight","ShootLat","ShootLong", "HaulLat","HaulLong",
           "StatRec","Depth","HaulVal","StdSpecRecCode","BySpecRecCode",  
           "DataType","Netopening","Rigging","Tickler", "Distance","Warplngt",             
           "DoorSpread","WingSpread","GroundSpeed","SpeedWater","UniqueID", "UniqueIDP")
setdiff( keepers, names(hauls))
setdiff(names(hauls), keepers)

others <- colnames(hauls)[!colnames(hauls) %in% keepers]
hauls <- hauls[, which(names(hauls) %in% keepers)]

str(hauls)

### add more unique ID that includes the station number and country (2021)
hauls$NewUniqueID2<-paste(hauls$Survey,hauls$Year,hauls$Quarter,hauls$Ship, 
                          hauls$HaulNo, hauls$Gear, hauls$StNo, hauls$Country,
                          sep="_")

# Gears differ between regions and Surveys we retain several gear types 
# BT3  BT4A BT4AI  BT4P  BT4S   BT6   BT7   BT8   GOV   H18   ROT 
# 6849  4432  3079  2028  2141  4500  1044  4571 39280    63  4648           
summary(as.factor(hauls$Gear))

table(hauls$Gear, hauls$Survey)

list<-c("BT3", "BT4A", "BT4AI", "BT4P", "BT4S", "BT6", "BT7", "BT8", 
        "GOV", "H18", "ROT")
non_standard_gear<-subset(hauls, !Gear%in%list,) ### 0 hauls
hauls <- subset(hauls, Gear%in%list, )
summary(hauls$Gear)
#write.csv(non_standard_gear, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/deleted_non_standard_gear_hauls.csv")


# What is the standard tow duration
# Delete hauls that are outside of the standard time 
hauls$HaulDur<-(as.numeric(hauls$HaulDur))
hist(hauls$HaulDur)
summary(hauls$HaulDur)
smallhauls<- subset(hauls, HaulDur<12 , ) ## how are these values decided?
hauls<-subset(hauls, HaulDur>12 , )
write.csv(smallhauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/deleted_too_short_hauls.csv")
largehauls<- subset(hauls, HaulDur>67 ,)
hauls<- subset(hauls, HaulDur<67 , )
write.csv(largehauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/deleted_too_long_gear_hauls.csv")
summary(as.numeric(hauls$HaulDur))
# Is the Survey Coordinated eg Q1 NS-IBTS
levels(as.factor(hauls$Survey))



# Delete NS-IBTS Quarter 2 and 4 surveys - related to the years of the stomach data collection
# in these years there was no overlap (ie all q 1 and q 3 suveys were within correct months)
check <- hauls[hauls$Survey == "NS-IBTS" & hauls$Quarter %in% c("2", "4") & hauls$Year %in% 
                 c( "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                    "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
                    "2015", "2016", "2017", "2018", "2019", "2020", "2021"),]
hauls <- hauls[hauls$NewUniqueID2 %nin% check$NewUniqueID2,]


# check quaters by survey
cols<-rainbow(13)
plot(hauls$Quarter, col=cols[as.factor(hauls$Survey)], pch=20)
quaterscheck<-ddply(hauls, c("Survey", "Quarter"),
                    summarise,
                    count=length(StNo))

### reducndant due to code above which removes Q2 from NS-IBTS trawls - rk
# hauls$QuarterCheck[hauls$Survey=="NS-IBTS" &
#                      hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
#                    & hauls$Year>="1998"] <-"changed to 3"
# hauls$Quarter[hauls$Survey=="NS-IBTS" &
#                 hauls$Quarter=="2"& hauls$Year>="1998"|hauls$Survey=="NS-IBTS" & hauls$Quarter=="4"
#               & hauls$Year>="1998"] <-3
hauls$QuarterCheck[hauls$Survey=="BTS" &
                     hauls$Quarter=="4"] <-"changed to 3"
hauls$Quarter[hauls$Survey=="BTS" &
                hauls$Quarter=="4"] <-3
# hauls$QuarterCheck[hauls$Survey=="BTS-VIIa" &
#                      hauls$Quarter=="4"] <-"changed to 3"
# hauls$Quarter[hauls$Survey=="BTS-VIIa" &
#                 hauls$Quarter=="4"] <-3
hauls$QuarterCheck[hauls$Survey=="IE-IGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="IE-IGFS" &
                hauls$Quarter=="3"] <- 4
hauls$QuarterCheck[hauls$Survey=="SWC-IBTS" &
                     hauls$Quarter=="2"] <-"changed to 1"
hauls$Quarter[hauls$Survey=="SWC-IBTS" &
                hauls$Quarter=="2"] <-1

hauls$QuarterCheck[hauls$Survey=="NIGFS" &
                     hauls$Quarter=="3"] <-"changed to 4"
hauls$Quarter[hauls$Survey=="NIGFS" &
                hauls$Quarter=="3"] <-4

##### Looks like all part of the same surveys from the dates in the table. 
## assign Q3 as Q4 - check this with Fran V - 2021

plot(hauls$Quarter, col=cols[as.factor(hauls$Survey)], pch=20)
table(hauls$Quarter, hauls$Survey)

table(hauls$QuarterCheck, hauls$Quarter)

#
# Standardizing survey area now seems to now happen at the end of the HH process instead - Script 8 - RK 2021
#

write.csv(hauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_HH_file_12-06-2021.csv")
## Check Unique IDS are unique
check<-unique(hauls$NewUniqueID2)

### remove hauls no longer in the header data from the HL data
HL2$NewUniqueID2 <- paste(HL2$Survey,HL2$Year, HL2$Quarter,HL2$Ship, 
                        HL2$HaulNo, HL2$Gear, HL2$StNo, HL2$Country,
                        sep="_")


## Check Unique IDS are unique
nrow(hauls[duplicated(hauls$New_UniqueID2),]) ## should return 0


checkhl<-unique(HL2$NewUniqueID2)
check <- unique(hauls$NewUniqueID2)

length(setdiff(checkhl, check)) ## expected records attached to hauls no longer in HH
length(setdiff(check, checkhl)) ## odd ids in the HH not in the HL 

HHcheck <- hauls[hauls$NewUniqueID2 %in% setdiff(check, checkhl),]

table(HHcheck$Survey, HHcheck$Year)
#           1984 1985 1986 1991 1992 1998 2000 2001 2005 2008 2009 2010 2011 2012 2014 2015 2019 2020
# BTS        0    0    0    0    0    0    1    0    0    0    1   16    1    0    1    2    0    0
# DYFS       0    0    0    0    0    0    0    0    2    1    0    0    0    0    0    0    0    0
# EVHOE      0    0    0    0    0    0    0    0    0    0    0    0    0    1    0    0    0    1
# FR-CGFS    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    0    0    0
# NIGFS      0    0    0    0    0    0    0    1    0    0    0    0    0    0    0    0    0    0
# NS-IBTS    1    2    0   70   65    1    3    0    0    0    0   48    0    0    1    4    1    0
# SNS        0   76   81    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0


# now select the hauls and HL files that match up given the new unique IDS
HL3 <- HL2[HL2$NewUniqueID2 %in% hauls$NewUniqueID2, ]
checkhl1<-unique(HL3$NewUniqueID2)

### 
setdiff(checkhl1, check)
length(setdiff(check, checkhl1))

### Following Moriarty et al. I will also drop any Hauls info header info and no
# HL data - there are not very many and they may be invalid for unknown reasons..
# RK - June 2021

hauls<-subset(hauls, NewUniqueID2%in% HL3$NewUniqueID2)

check<-unique(hauls$NewUniqueID2)

checkhl1<-unique(HL3$NewUniqueID2)
setdiff(checkhl1, check)
setdiff(check, checkhl1)
# hopefully this solves some problems of the not so unique ids :)


# difference in Valid_Aphia
Valid_AphiaHL <- unique(HL3$Valid_Aphia)
names(HL3)

Valid_AphiaHL2 <- unique(HL2$Valid_Aphia)
setdiff(Valid_AphiaHL, Valid_AphiaHL2)
write.csv(Valid_AphiaHL2, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Standard_Survey_Species_list.csv")
write.csv(Valid_AphiaHL, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Full_Species_list.csv")

summary(as.numeric(HL3$Valid_Aphia))


HL3$Valid_Aphia[HL3$Valid_Aphia == 0] ### hmmmmm.... check this at a later point..

## set to NA
HL3$Valid_Aphia[which(HL3$Valid_Aphia == 0)] <- NA

### Sort out any lingering -9's in key HH columns - RK 2021

summary(hauls)

hauls$SweepLngt[which(hauls$SweepLngt == -9)] <- NA
hauls$Depth[which(hauls$Depth == -9)] <- NA
hauls$Netopening[which(hauls$Netopening == -9)] <- NA  
hauls$Warplngt[which(hauls$Warplngt == -9)]<- NA
hauls$DoorSpread[which(hauls$DoorSpread == -9)]<- NA        
hauls$WingSpread[which(hauls$WingSpread == -9)]<- NA 
hauls$GroundSpeed[which(hauls$GroundSpeed  == -9)]<- NA 
hauls$SpeedWater[which(hauls$GroundSpeed  == -9)]<- NA  

write.csv(hauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_HH_file.csv")

head(HL3)

write.csv(HL3, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_HL_file.csv")

rm(hauls_sub, haulsx, haulsx2, HH_BTS_DE, HH_save, 
               HH1, HH, HLcheck, invalidhauls, largehauls,
               HL2, landlocked, non_standard_gear, pre1983,
               quater2or4, quaterscheck ,remove_stations,SOL_ship ,
               SP_north, check, checkhl,checkhl1, 
               HH2, add_SP, bad_data, 
               delete_ship, den, find, findduplicates, 
               HHcheck, sco, smallhauls, x)

save(list=ls(all=T), file = "./script5_output.rda")
#load("./script5_output.rda")


