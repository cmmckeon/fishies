# title: "7_Haul_DP.R"
# last updated: "08/04/2022"



# This is Script 7 of 7 
# The purpose of this script is to seperate the surveys and 
# define the final data products structure for the haul files

# load("./script6_output.rda")

# Prep Data --------------

# Here we assign the column names to the values that will appear in the top end 
# of the data product to insure transparency and it is user friendly

h <- hauls
# check that the final list of hauls match in both groups
list <- unique(h$NewUniqueID2)


#  Add Survey Acronyms from Data products -----------------------
# See table 2.1 in Moriarty et al (in prep) for details 
names(h)

h$Survey <- droplevels(h$Survey)
unique(h$Survey)

h$Survey_Acronym <- as.character(h$Survey)

h$Survey_Acronym[h$Survey=="NS-IBTS"&h$Quarter=="1"]<-"GNSIntOT1"
h$Survey_Acronym[h$Survey=="NS-IBTS"&h$Quarter=="3"]<-"GNSIntOT3"
h$Survey_Acronym[h$Survey=="FR-CGFS"&h$Quarter=="4"]<-"GNSFraOT4"
h$Survey_Acronym[h$Survey=="SWC-IBTS"&h$Quarter=="1"]<-"CSScoOT1"
h$Survey_Acronym[h$Survey=="SWC-IBTS"&h$Quarter=="4"]<-"CSScoOT4"
h$Survey_Acronym[h$Survey=="SCOWCGFS"&h$Quarter=="1"]<-"CSScoOT1"
h$Survey_Acronym[h$Survey=="SCOWCGFS"&h$Quarter=="4"]<-"CSScoOT4"
h$Survey_Acronym[h$Survey=="IE-IGFS"&h$Quarter=="4"]<-"CSIreOT4"
h$Survey_Acronym[h$Survey=="NIGFS"&h$Quarter=="1"]<-"CSNIrOT1"
h$Survey_Acronym[h$Survey=="NIGFS"&h$Quarter=="2"]<-"CSNIrOT2" 
h$Survey_Acronym[h$Survey=="NIGFS"&h$Quarter=="4"]<-"CSNIrOT4"
h$Survey_Acronym[h$Survey=="EVHOE"&h$Quarter=="4"]<-"CSBBFraOT4"
h$Survey_Acronym[h$Survey=="ROCKALL"&h$Quarter=="3"]<-"WAScoOT3"
h$Survey_Acronym[h$Survey=="SCOROC"&h$Quarter=="3"]<-"WAScoOT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&h$Country=="NL"]<-"GNSNetBT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&h$Country=="DE"]<-"GNSGerBT3"
h$Survey_Acronym[h$Survey=="BTS"&h$Quarter=="3"&h$Country=="GB"]<-"GNSEngBT3"

summary(as.factor(h$Survey_Acronym))

# add a gear type filer column -------------------
#h$GearType[h$Survey=="BTS"]<-"BT"
h$GearType[h$Gear %in% c("BT3", "BT4A", "BT4AI", "BT4P", "BT4S", "BT6", "BT7", "BT8")] <- "BT"
h$GearType[is.na(h$GearType)]<-"OT"
# new haul unique identifer Survey Acronym/ship/year/haul No
# read ship table
ships <- read.csv("Raw_Data/ShipC_TS_Ship.csv")

h$Ship_old <- ships$TS_Ship[match(h$Ship,ships$ShipC)]

table(h$Ship, h$Ship_old)

h$HaulID<-paste(h$Survey_Acronym,h$Quarter, h$Ship_old,h$Year,h$HaulNo, h$Country, h$StNo, sep="_")
length(which(duplicated(h$HaulID)))

list <- h$HaulID[which(duplicated(h$HaulID))]
check <- h[which(h$HaulID %in% list),] ## why are the GB surveys from two different days? Does this matter?

h$YearShot<-h$Year                
h$MonthShot<-h$Month                  
h$DayShot<-h$Day

table(h$GearType, h$Gear)

names(hauls)
summary(as.factor(hauls$Month))
h$HaulDur_min <- h$HaulDur
h$ICESStSq <- h$check_StatRec
h$SurvStratum <- h$DepthStratum
summary(as.factor(h$SurvStratum))

# Sort out strata ---------------------
names(h)
h$SurvStratum <- as.character(h$SurvStratum)
h$SurvStratum[h$Survey_Acronym=="GNSIntOT1"] <- h$ICESStSq[h$Survey_Acronym=="GNSIntOT1"]
h$SurvStratum[h$Survey_Acronym=="GNSIntOT3"] <- h$ICESStSq[h$Survey_Acronym=="GNSIntOT3"]
h$SurvStratum[h$Survey_Acronym=="GNSGerBT3"] <- h$ICESStSq[h$Survey_Acronym=="GNSGerBT3"]
h$SurvStratum[h$Survey_Acronym=="GNSNetBT3"] <- h$ICESStSq[h$Survey_Acronym=="GNSNetBT3"]
h$SurvStratum[h$Survey_Acronym=="GNSEngBT3"] <- h$ICESStSq[h$Survey_Acronym=="GNSEngBT3"]
find<-subset(h, is.na(SurvStratum),)
summary(as.factor(find$Survey_Acronym))
h$SurvStratum[is.na(h$SurvStratum)] <- "not_recorded"
# we don't believe the recorded wingspreads of 10 on the 
# CGFS - using modelled data instead. 

h <- as.data.frame(h)
## Quick look at dataframe
par(mfrow = c(3,3))
# Factors
for (i in names(Filter(is.factor, h))) {
  plot(h[,i],
       main = paste(i))
}

## Numeric variables
for (i in names(Filter(is.numeric, h))) {
  hist(h[,i],
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}

## drop door spread with a value of 1....
h <- h[h$`Wing/Door(Ratio)` < 10,]

h <- rem_na_df(h)

h$Use_WingSpread[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"] <- h$mod1_wingspread_gov[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"]
h$QualityWing[h$Survey_Acronym=="GNSFraOT4"&h$Year=="2014"]<-"mod1_wing_gov"
h$Depth_m <- h$DepthNew
h$Distance_km <- h$newDist/1000
h$WingSpread_m <- h$Use_WingSpread
h$DoorSpread_m <- h$Use_DoorSpread
h$NetOpen_m <- h$Use_Netopening
summary(h$SweptArea_wing_km_sqrd) 

h$SweptArea_wing_m_sqrd <- h$Use_WingSpread*h$newDist
h$SweptArea_wing_km_sqrd <- h$SweptArea_wing_m_sqrd/1000/1000
h$WingSwpArea_sqkm <- h$SweptArea_wing_km_sqrd
h$WingSwpVol_CorF <- 1/(h$Use_Netopening/1000)
h$DoorSwptArea_CorF <- (h$Use_WingSpread/1000)/(h$Use_DoorSpread/1000)
h$DoorSwptVol_CorF <- (h$Use_WingSpread/1000)/((h$Use_DoorSpread/1000)*(h$Use_Netopening/1000))
summary(h$WingSwpVol_CorF)
summary(h$DoorSwptArea_CorF)
summary(h$DoorSwptVol_CorF)
h$ShootLat_degdec <- h$ShootLat
h$ShootLong_degdec <- h$ShootLong

check <- droplevels(h[which(is.na(h$SweptArea_wing_m_sqrd)),])

#check duplicate times in h file
h$date_time_check <- paste(h$Ship, h$Year, h$Quarter, h$month, h$Day, h$TimeShot, sep="_")
list <- h[duplicated(h$date_time_check),] # 2120 ids are duplicate 
# Meadhbh says this is a case of time not been updated on from the previous haul.
# an estimated time will be added into the dataset

check <- length(unique(h$HaulID))

h$TimeShot[h$UniqueIDP=="IE-IGFS_2003_4_CEXP_73_GOV"&h$TimeShot=="1651"]<-1851
h$TimeShot[h$UniqueIDP=="IE-IGFS_2004_4_CEXP_11_GOV"&h$TimeShot=="813"]<-2013
h$TimeShot[h$UniqueIDP=="IE-IGFS_2004_4_CEXP_6_GOV"&h$TimeShot=="802"]<-1000
h$TimeShot[h$UniqueIDP=="IE-IGFS_2015_4_CEXP_102_GOV"&h$TimeShot=="745"]<-1400
h$TimeShot[h$UniqueIDP=="NS-IBTS_1995_1_WAH3_4_GOV"&h$TimeShot=="801"]<-2001
h$TimeShot[h$UniqueIDP=="NS-IBTS_1998_1_THA2_9_GOV"&h$TimeShot=="754"]<-1954
h$TimeShot[h$UniqueIDP=="NS-IBTS_2013_1_DAN2_37_GOV"&h$TimeShot=="632"]<-932
h$TimeShot[h$UniqueIDP=="NS-IBTS_2013_3_SCO3_243_GOV"&h$TimeShot=="1130"]<-1430
h$TimeShot[h$UniqueIDP=="NS-IBTS_2016_1_SCO3_38_GOV"&h$TimeShot=="1244"]<-1444
h$TimeShot[h$UniqueIDP=="ROCKALL_2015_3_SCO3_328_GOV"&h$TimeShot=="1054"]<-1300
h$TimeShot[h$UniqueIDP=="ROCKALL_2015_3_SCO3_346_GOV"&h$TimeShot=="926"]<-1130
h$TimeShot[h$UniqueIDP=="SWC-IBTS_2015_1_SCO3_62_GOV"&h$TimeShot=="1010"]<-1320
h$TimeShot[h$UniqueIDP=="SPNGFS_2006_4_CDS_78_BAK"&h$TimeShot=="614"]<-1814
h$TimeShot[h$UniqueIDP=="BTS_2003_3_ISI_11_BT8" &h$TimeShot=="925"]<-1125
h$TimeShot[h$UniqueIDP=="NS-IBTS_2016_3_SCO3_281_GOV" &h$TimeShot=="1424"]<-1705


# wash and repeat
h$date_time_check <- paste(h$Ship, h$Year, h$Quarter, h$Month, h$Day, h$TimeShot, sep="_")
list<-h[duplicated(h$date_time_check),] ## 2063 ids are the same...

check <- droplevels(h[duplicated(h$date_time_check),])
levels(check$Survey) ## offending surveys = "BTS" "DYFS" "IE-IGFS"  "NS-IBTS"  "SCOROC"   "SCOWCGFS"
table(check$Survey) ## 2023 from BTS

table(h$Survey, h$Year)


list <- (unique(h$HaulID))
list <- h[duplicated(h$HaulID),]
summary(h$WingSwpArea_sqkm)
summary(h$Depth_m)
summary(h$DoorSwptVol_CorF)

h$SurveyDATRAS <- h$Survey

haul_dat <- unique(subset(h, 
                 select=c(HaulID,Survey_Acronym,Ship,GearType,Gear, 
                          YearShot,MonthShot,DayShot,TimeShot, 
                          HaulDur_min,ShootLat_degdec,ShootLong_degdec,ICESStSq,
                          SurvStratum,Depth_m,Distance_km,WingSpread_m, 
                          DoorSpread_m, NetOpen_m,WingSwpArea_sqkm,
                          WingSwpVol_CorF, DoorSwptArea_CorF,DoorSwptVol_CorF,
                          SurveyDATRAS)))

require(rgdal)
europe<-readOGR("./Regional Boundaries Maps and Data/shapes//europe.dbf","europe") 


check <- haul_dat[which(!is.na(haul_dat$WingSwpArea_sqkm)),]

for (cat in unique(check$Survey_Acronym)){
  mypath <- file.path(paste("Data_QA_Process_V5_2022/Diagnostics/Haul Diagnostics", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  par(mfrow=c(2,3))
  d <- subset(haul_dat, Survey_Acronym == cat)
  plot(d$ShootLong_degdec, d$ShootLat_degdec, 
       main=unique(d$cat), pch=19, xlab="Longitude", 
       ylab="Latitude",cex=1.9)
  plot(europe, col="lightgrey", add=T)
  title(unique(d$cat))
  plot(d$HaulDur_min, d$Distance_km, pch=19, xlab="Time (min)", 
       ylab="Distance (km)", cex=1.9)
  x<-c(13:66)
  points(x, x*4*1.852/60, type="l", col="red", lwd=3)
  points(x, x*2*1.852/60, type="l", col="red", lty=2, lwd=2)
  points(x, x*6*1.852/60, type="l", col="red", lty=2, lwd=2)
  plot(d$Distance_km, d$WingSwpArea_sqkm, pch=19, xlab="Distance (km)", 
       ylab="Wing Spread (km2)", cex=1.9)
  plot(d$Depth_m, d$WingSpread_m, pch=19, xlab="Depth (m)", 
       ylab="Wing Spread (m)", cex=1.9)
  plot(d$Depth_m, d$DoorSpread_m, pch=19, xlab="Depth (m)", 
       ylab="Door Spread (m)", cex=1.9)
  plot(d$Depth_m, d$NetOpen_m, pch=19, xlab="Depth (m)", 
       ylab="Net Opening (m)", cex=1.9)
    dev.off()
}
summary(haul_dat)

write.csv(haul_dat, "Data_QA_Process_V5_2022/Sampling_info_all_surveysV5.csv", row.names = FALSE)
write.csv(haul_dat, "Data_QA_Process_V5_2022/Sampling_info_all_surveysV5.txt",  row.names = FALSE)

write.csv(table(haul_dat$YearShot,haul_dat$SurveyDATRAS), "Data_QA_Process_V5_2022/Hauls_per_year_final_prod.csv")

## Looking at which varibles still contain NAs
list <- c()
for (i in names(haul_dat)){
  list[i] <-length(which(is.na(haul_dat[,i])))
}

print(list) ## no NAs

saveRDS(h, "clean_HH.rds")

save(list=ls(all=T), file = "./script7_output.rda")

