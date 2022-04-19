# title: "6_Haul_QA.R"
# last updated: "08/04/2022"



# This is Script 6 of X
# This Script follows the guidlines outlined in Section 4.1 HH Data - Haul Summary Information
# from: Moriarty and Greenstreet 2016 
# The next step is to model missing data in the haul data of the surveys, and plot 
# lots of graphs to check all the haul stuff is believable now!

# load("./script5_output.rda")

require(rgdal)
library(marmap)

# Load Data Frame Again -----------------------

HH<-read.csv("Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_HH_file.csv", row.names = "X")
HL<-read.csv("Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_HL_file.csv", row.names = "X")

HH <- rem_na_df(HH)
HL <- rem_na_df(HL)

rm(HL3, hauls) 

hauls<-(HH) 
summary(hauls)


# Sample Location ------------------


# check shoot and haul match ICES Stat Rec
summary(as.factor(hauls$DepthStratum))

# set -9's to NA
hauls$DepthStratum[which(hauls$DepthStratum == -9)] <- NA
summary(as.factor(hauls$DepthStratum)) ## many NAs

table(hauls$StatRec, useNA = "ifany") ## some NAs
# ices<-read.csv("./Regional Boundaries Maps and Data/ICES_rectangles_statistics/Ices_rect_table.csv")
# names(ices)
# shoot longs that are NA should be -9 , silly DATRAS -9 thing messing stuff up here!
summary(hauls$ShootLong)
### not changing these as making them -9 seems like telling R it's a real location... RK 2021
# long_error<-hauls[is.na(hauls$ShootLong),]
# list<-long_error$UniqueID
# hauls$ShootLong[hauls$UniqueID%in%list]<--9

table(hauls$Survey[is.na(hauls$ShootLong)])

#  SWC-IBTS 
#  20 


# check haul positions match ICES Stats
names(hauls)
summary(hauls$ShootLat)
lat<-hauls$ShootLat
hauls$check_StatRec<- ices.rect2(hauls$ShootLong, hauls$ShootLat)
summary(as.factor(hauls$check_StatRec))
summary(as.factor(hauls$Survey))
check<-subset(hauls, !(hauls$check_StatRec%in%hauls$StatRec))
check<-hauls[which(hauls$check_StatRec!=hauls$StatRec),]
check<-subset(hauls, is.na(hauls$DepthStratum))
summary(as.factor(check$Survey))
summary(as.factor(hauls$check_StatRec))

hauls$StatRec[is.na(hauls$ShootLong)] ### those with ShootLats did have statistical rectangles use these - RK

hauls$check_StatRec[is.na(hauls$ShootLong)] <- hauls$StatRec[is.na(hauls$ShootLong)]

# use Check StatRec col = mistakes in actual Stat Rec.
# some mis-matches - use check_StatRec for correct Stat Rec
# need to assign stratum based on survey!
# need maps
# New BaseMap


#load maps -------------

#set directory to working folder and insure subfolders are availible
europe<-readOGR("./Regional Boundaries Maps and Data/shapes//europe.dbf","europe")
contour_1000m<-readOGR("./Regional Boundaries Maps and Data/shapes//1000m.dbf","1000m")
NWW_boundary<-readOGR("./Regional Boundaries Maps and Data/shapes//NWW_boundary.dbf","NWW_boundary")
gc()
contour_200m<-readOGR(".//Regional Boundaries Maps and Data/shapes//200m.dbf","200m")
ospar<-readOGR("./Regional Boundaries Maps and Data/ospar_boundaries//OSPAR_Inner_Boundary.dbf", "OSPAR_Inner_Boundary") ## capitalised "Inner" in file path. CM feb 2022
sco1<-readOGR("./Regional Boundaries Maps and Data/SWCQ1.WGS84//SWC_Q1.dbf", "SWC_Q1")
sco3<-readOGR("./Regional Boundaries Maps and Data/SWCQ4.WGS84//SWC_Q4.dbf", "SWC_Q4")
gc()
ices<-readOGR("./Regional Boundaries Maps and Data/ICES_rectangles_statistics/ICES_rectangles_statistics.dbf", "ICES_rectangles_statistics")
ire<-readOGR("./Regional Boundaries Maps and Data/IGFS.WGS84//IGFS.WGS84.dbf", "IGFS.WGS84")
gc()
ni<-readOGR("./Regional Boundaries Maps and Data/NI-IBTS.WGS84//NI_IBTS.WGS84.dbf", "NI_IBTS.WGS84")
evhoe<-readOGR("./Regional Boundaries Maps and Data/Fr-EVHOE.WGS84//EVHOE.WGS84.dbf", "EVHOE.WGS84")
rock<-readOGR("./Regional Boundaries Maps and Data/SWC-RockQ3.WGS84//SWC_Q3.dbf", "SWC_Q3")
gc()


summary(hauls$ShootLong)
par(mai=c(0,0,0,0),bg='white' )
#plot(contour_200m, add=TRUE, col="lightblue3")
#plot(contour_1000m, add=TRUE, col="lightblue4")
plot(NWW_boundary, border="white", xlim = c(-16,13), ylim = c(36, 62))
plot(europe,add=TRUE, xlim = c(-16, 13), ylim = c(36, 62), asp = 1, col=c('gray81'), 
     border='gray3')
gc()
# plot(contour_200m, add=TRUE, border="lightblue3")
# plot(contour_1000m, add=TRUE, border="lightblue4")
plot(ire, add=TRUE, lty=2, lwd=3, border="green")
plot(sco3, add=TRUE, lty=1, lwd=1, border="blue")
plot(evhoe, add=T, lty=1, lwd=1, border="red")
plot(ni, add=T, border="lightblue")
plot(rock, add=T, border="yellow")
plot(NWW_boundary, border="white", xlim = c(-16,13), ylim = c(36, 62))
plot(ices, add=TRUE)
plot(europe,add=TRUE, xlim = c(-16, 13), ylim = c(36, 62), asp = 1, col=c('gray81'), 
     border='gray3')
#text(ices$Centr_X, ices$Centr_Y, ices$ICESNAME)
plot(ospar, col="red", add=T, lwd=4)
check<-subset(hauls, !(hauls$check_StatRec%in%hauls$StatRec))
points(check$ShootLong, check$ShootLat, col="red")
points(hauls$ShootLong, hauls$ShootLat, pch=19, col="black")
unique(check$Survey)
### issues are with SWC-IBTS



# Depth -----------------------

hauls$EstDepth<-NA

check <- hauls[is.na(hauls$Depth),]

length(check$ShootLat[is.na(check$ShootLat)])
length(check$ShootLong[is.na(check$ShootLong)])
length(check$HaulLat[is.na(check$HaulLat)])
length(check$HaulLong[is.na(check$HaulLong)])

summary(hauls$ShootLat[is.na(hauls$Depth)])
summary(hauls$ShootLong[is.na(hauls$Depth)])

# For observations with no depth AND no haul positional data 
# estimate depth on shoot position is the best we can do

summary(hauls$ShootLat)
summary(as.numeric(hauls$ShootLong))


# use NOAA website to get bathy map
 # papoue <- getNOAA.bathy(lon1 = -16, lon2 = 13,
 #                         lat1 = 36, lat2 = 62, resolution = 1)
 # saveRDS(papoue, "./Regional Boundaries Maps and Data/papoue_bathy_map.rds")
 
papoue <- readRDS("./Regional Boundaries Maps and Data/papoue_bathy_map.rds")
summary(papoue)

# make a pretty map of all the stations
png(file="Data_QA_Process_V5_2022/surveydepthmap.png")
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.99), grey(0.95), grey(0.85))
plot(papoue, image = TRUE, land = FALSE, lwd = 0.05,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)))
cols<-heat_hcl(13, c = c(80, 30), l=c(30,90), power=c(1/5, 1.5))
hauls$Survey<-(as.factor(hauls$Survey))
points(hauls$ShootLong, hauls$ShootLat, pch=19, 
       cex=0.3, col=cols[hauls$Survey])
dev.off()

check <- hauls[is.na(hauls$ShootLong),]
check <- hauls[is.na(hauls$Depth),]
## all hauls without ShootLongs have depths
### calculate Depths only for hauls where Shoot Longs are known RK


NOAA_Depth<-get.depth(papoue, x=hauls$ShootLong[!is.na(hauls$ShootLong)], y=hauls$ShootLat[!is.na(hauls$ShootLong)], locator=FALSE)

hauls$EstDepth[!is.na(hauls$ShootLong)] <- NOAA_Depth$depth*-1

hauls$DepthNew<-hauls$Depth
hauls$DepthNew[is.na(hauls$Depth)]<-hauls$EstDepth[is.na(hauls$Depth)] 
summary(hauls$DepthNew)
hauls$DepthNew[hauls$Depth==4]<-18
hauls$DepthNew[hauls$Depth==-9]<-37
summary(hauls$DepthNew)

dev.new()

# make a graph of all the difference between estimated and recorded depths
png(file = "Data_QA_Process_V5_2022/Diagnostics/depth_differences.png", bg = "transparent")
plot(hauls$Depth, hauls$EstDepth, pch=19, xlab="Recorded Depth (m)",
     ylab="Estimated point depth (m)", ylim=c(0,800))
abline(a=0, b=1, col="red")
dev.off()
dev.new()
write.csv(papoue, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Bathy_map_13_04_22.csv")

# add some diagnostics to check if the differences in depth are acceptable 
png(file = "Data_QA_Process_V5_2022/Diagnostics/map_depth_differences.png", bg = "transparent")
plot(papoue, image = TRUE, land = FALSE, lwd = 0.05,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)))
cols<-heat_hcl(13, c = c(80, 30), l=c(30,90), power=c(1/5, 1.5))
hauls$Survey<-factor(hauls$Survey)
hauls$Diff_dep <-sqrt((hauls$Depth- hauls$EstDepth)^2)
#hauls$Diff_dep[which(is.na(hauls$Diff_dep))] <- 0
summary(hauls$Diff_dep)
radius<-sqrt(hauls$Diff_dep/pi)
symbols(hauls$ShootLong, hauls$ShootLat, circles=radius, inches=0.2, fg="white",
        bg=cols[hauls$Survey], xlab="Longitude", ylab="Latitude", add=T)
legend.text<-c(1,10,100,1000, 2000)
x<-c(9,9,9,9,9)
y<-c(50,49.8, 49.5, 48.5,47.5)
symbols(x,y, circles=sqrt(legend.text/pi), inches=0.2, fg="black",
        bg="grey", add=T)
text(9.9, 50, "1m")
text(9.9, 49.8, "10m")
text(9.9, 49.5, "100m")
text(9.9, 48.5, "1000m")
text(9.9, 47.5, "2000m")
dev.off()
dev.new()

png(file = "Data_QA_Process_V5_2022/Diagnostics/box_plot_depth_differences_survey.png", bg = "transparent")
plot(hauls$Survey,hauls$Diff_dep, col=cols[hauls$Survey], pch=19, main = "Difference in depth")
dev.off()

find<-subset(hauls, hauls$DepthNew<5,)

hist(sqrt((hauls$Depth- hauls$DepthNew)^2))
## there a problems - a couple of depth differences over 1000, many over 200, a few negative depths.


# Sweep Length ----------------

summary(factor(hauls$SweepLngt)) ##

# Sweep Lenght Values are sometimes incorrect or missing, 
# delete incorrect values 
hauls$EstSweepLngt<-hauls$SweepLngt
hauls$EstSweepLngt[hauls$SweepLngt>121&hauls$Gear=="GOV"]<-NA #RK edit - i don't like -9's!
summary(as.factor(hauls$EstSweepLngt))


hauls$Gear[which(hauls$SweepLngt == 0)] ## There should not be 0's in GOV gear sweep lengths.. RK #BT3 BT4A BT4AI BT4P BT4S BT6 BT7 BT8 GOV H18 ROT
hauls$EstSweepLngt[which(hauls$EstSweepLngt == 0)]  <-NA # RK 2021

# find out extent of issue
sweepsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepLngt"),
                    summarise, N=length(EstSweepLngt))
sum(sweepsummary$N[which(is.na(sweepsummary$EstSweepLngt))])
summary(factor(hauls$Survey))
# BTS Surveys don't record a sweep - should be NA
# ROT surveys - no sweep - should be NA
# NCT surveys - no sweeps 
hauls$EstSweepLngt[hauls$Survey=="BTS"]<-NA
hauls$EstSweepLngt[hauls$Survey=="NIGFS"]<-NA

# in 1983/1984 no country recorded sweep, but in 1985+ countries 
# reported using "Recommended Sweeps" so gonna apply the 60/110 rule to these 
# years
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Year=="1983" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1983" &hauls$DepthNew>75] <-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1984" &hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Year=="1984" & hauls$DepthNew>75]<-110
# Denmark: Apply standard as in Manual
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DK" & hauls$DepthNew<76] <-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DK" & hauls$DepthNew>75]<-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Year<2005 &
                     hauls$Quarter=="3" & hauls$Country=="DK"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Year>2004 &
                     hauls$Quarter=="3" & hauls$Country=="DK"& 
                     hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="3"  
                   & hauls$Country=="DK" & hauls$Year>2004 &
                     hauls$DepthNew>75]<-110

# Germany next - Q1 and Q 3 rules differ
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"& 
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DE" & hauls$DepthNew<76] <-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Quarter=="1" &
                     hauls$Country=="DE" & hauls$DepthNew>75]<-110
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt)& hauls$Year<2004 &
                     hauls$Quarter=="3" & hauls$Country=="DE"]<-60

#Netherlands next only 2 years missing data in quarter 1
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="1" & hauls$Country=="NL"]<-60

#Norway next
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="3" & hauls$Country=="NO"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="NO" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="NO" & hauls$DepthNew>75]<-110
# Sweden
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="3" & hauls$Country=="SE"]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="SE" & hauls$DepthNew<76]<-60
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Quarter=="1" &
                     hauls$Country=="SE" & hauls$DepthNew>75]<-110
# Scotland
hauls$EstSweepLngt[hauls$Survey=="NS-IBTS"&
                     is.na(hauls$EstSweepLngt) &
                     hauls$Quarter=="1" & hauls$Country=="GB-SCT"]<-60

# SWC Survey
hauls$EstSweepLngt[hauls$Survey=="SWC-IBTS"&
                     is.na(hauls$EstSweepLngt) & hauls$Year<2011] <-60

# Rockall
hauls$EstSweepLngt[hauls$Survey=="ROCKALL"&
                     is.na(hauls$EstSweepLngt) & hauls$Year<2011]<-60
# Check all new sweeps
sweepsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepLngt"),
                    summarise, N=length(EstSweepLngt))
sum(sweepsummary$N[which(is.na(sweepsummary$EstSweepLngt))])

#Assign cat of short or long to sweep length
summary(as.factor(hauls$EstSweepLngt))
hauls$EstSweepCat<-hauls$EstSweepLngt
hauls$EstSweepCat[hauls$EstSweepLngt<61]<-"short"
hauls$EstSweepCat[hauls$EstSweepLngt==97|hauls$EstSweepLngt==110|
                    hauls$EstSweepLngt==100|hauls$EstSweepLngt==120]<-"long"

sweepcatsummary<-ddply(hauls, c("Survey","Country", "Year", "Quarter", "EstSweepCat"),
                       summarise, N=length(EstSweepCat))



# Haul Duration ------------------------

# Check range >66 and <13 not within bounds
summary(hauls$HaulDur)


# Ground Speed --------------------------

### Draw some plots to look at data
#make a plot to look at the current groundspeed*time against distance 
# if perfect we expect an intercept of 0 and a slope of 1

hist(hauls$Distance)

hauls[which(hauls$Distance == 45074),] ## belgian BTS being off!
png(file = "Data_QA_Process_V5_2022/Diagnostics/distance_speed_time_differences.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls$GroundSpeed*1852/60*hauls$HaulDur, hauls$Distance, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
dev.off()
png(file="Data_QA_Process_V5_2022/Diagnostics/groundspeed_diagnostics.png", bg="transparent")
plot(hauls$HaulDur, hauls$GroundSpeed, pch=19, xlab="Time", ylab="Speed (knots)")
abline(h=6, col="red")
abline(h=2, col="red")
dev.off()
# Change the confidence interval fill color
png(file = "Data_QA_Process_V5_2022/Diagnostics/distance_speed_time_differences_with_CI.png", bg = "transparent")
p1<-ggplot(hauls, aes(x=GroundSpeed*1852/60*HaulDur,
                      y=Distance)) + 
  geom_point(shape=18, color="black")+
  geom_smooth(method=lm,  linetype="dashed",
              color="lightgrey", fill="darkgrey", se=TRUE, fullrange=FALSE, level=.95)
p1 + scale_color_grey()+theme_classic()
dev.off()
# check ground speed
png(file="Data_QA_Process_V5_2022/Diagnostics/groundspeed_boxplot.png", bg="transparent")
plot(hauls$Survey, hauls$GroundSpeed, pch=19, xlab="Survey", 
     ylab="GroundSpeed (knots)", col="lightgrey")
dev.off()
dev.new()
# outliers in Groundspeed found
check <- hauls[which(!is.na(hauls$GroundSpeed)),]
check<-check[check$GroundSpeed<3|check$GroundSpeed>5, ]

png(file="Data_QA_Process_V5_2022/Diagnostics/groundspeed_distance_comparision.png", bg="transparent")
plot(check$GroundSpeed*check$HaulDur*1852/60, check$Distance, pch=19, col="black")
dev.off()

# In Ns-IBTS 1995 it seems some of the french ground speeds are in the Speed Water Column
# and the Speed water is in the Ground Speed Col.
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/2/GOV"]<-4
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/2/GOV"]<-2
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/3/GOV"]<-4
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/3/GOV"]<-2
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/6/GOV"]<-4
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/9/GOV"]<-3.8
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/9/GOV"]<-1.9
hauls$GroundSpeed[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/11/GOV"]<-3.9
hauls$SpeedWater[hauls$UniqueIDP=="NS-IBTS/1995/1/THA/11/GOV"]<-1.9

# GROUNDSPEED NOT RECORDED BUT DISTANCE AND DURATION RECORDED
hauls$Realised_groundspeed<-hauls$Distance/hauls$HaulDur/1852*60
dev.new()
png(file="Data_QA_Process_V5_2022/Diagnostics/groundspeed_predictedVdistance_divided_by_time.png", bg="transparent")
plot(hauls$Realised_groundspeed, hauls$GroundSpeed, 
     pch=19, col="lightgrey",
     xlim=c(1,6), ylim=c(1,6))
abline(0,1, col="black")
dev.off()
png(file = "Data_QA_Process_V5_2022/Diagnostics/RecordedVRealisedGS.png", bg = "transparent")
plot(hauls$GroundSpeed, hauls$Distance/hauls$HaulDur/1852*60,
     pch=19, col="black", cex=1, xlab="Recorded Groundspeed (knots)", 
     ylab="Realised Groundspeed (knots)", xlim=c(0,9), ylim=c(0,9))
abline(a=0, b=1, col="lightgrey", lwd=2)
draw.circle(4,7.5, 1.2, border="red", lwd=2)
abline(h=6, col="lightgrey", lwd=1, lty=2)
abline(v=6, col="lightgrey", lwd=1, lty=2)
abline(h=2, col="lightgrey", lwd=1, lty=2)
abline(h=1, col="lightgrey", lwd=1, lty=2)
draw.circle(8.4, 8.4, 0.1, border="red", lty=4, lwd=2)
dev.off()

## predict groound speed --------------
hist(hauls$GroundSpeed, breaks = 1000)

plot(hauls$GroundSpeed ~ hauls$Ship)
plot(hauls$GroundSpeed ~ hauls$Quarter)
plot(hauls$GroundSpeed ~ hauls$Gear)

## zero groundspeed data for some ships and gears 

summary(hauls$GroundSpeed)
needSpeed <- droplevels(hauls[is.na(hauls$GroundSpeed),])

needSpeedGear <- factor(levels(droplevels(needSpeed$Gear[is.na(needSpeed$GroundSpeed)]))) ## gear with missing ground speed
needSpeedShip <- factor(levels(droplevels(needSpeed$Ship[is.na(needSpeed$GroundSpeed)]))) ## ships with missing ground speed
withSpeedShip <- factor(levels(droplevels(hauls$Ship[!is.na(hauls$GroundSpeed)]))) ## ships with some ground speed
speed_none <- factor(setdiff(speed_missing, speed_some)) ## ships with no ground speed at all

# ID ships that have some missing GroundSpeed records
canSpeedShip <- droplevels(needSpeedShip[needSpeedShip %in% withSpeedShip]) ## all ships with both missing and recorded ground speed
canSpeed <- droplevels(hauls[hauls$Ship %in% canSpeedShip, ])

gs_model1<-lm(GroundSpeed~Quarter:Ship:Gear, data= droplevels(hauls))
gs_model2<-lm(GroundSpeed~Quarter:Gear, data= droplevels(hauls))

par(mfrow = c(2,2))
plot(gs_model1)
plot(gs_model2)
AIC(gs_model1, gs_model2)
anova(gs_model1,gs_model2)

# allmissing observations
#pred_ship <- droplevels(subset(hauls, hauls$Ship %nin% speed_none)) ## why can't I do this?
pred_ship <- droplevels(subset(canSpeed, canSpeed$Ship %nin% speed_none))
pred_ship <- droplevels(subset(pred_ship, pred_ship$Gear %nin% c("ROT", "BT8", "H18")))

hist(pred_ship$GroundSpeed, breaks = 100)
summary((pred_ship$GroundSpeed))

pred_ship$predicted_groundspeed_gs1 <- predict(gs_model1, pred_ship, allow.new.levels=T)

summary(pred_ship$predicted_groundspeed_gs1)
plot(pred_ship$predicted_groundspeed_gs1, pred_ship$GroundSpeed, pch=19, col='grey' )
pred_gear <- subset(hauls, hauls$Gear %nin% c("ROT", "BT8", "H18")) 

pred_gear$predicted_groundspeed_gs2 <- predict(gs_model2, pred_gear, allow.new.levels=T)

summary(pred_gear$predicted_groundspeed_gs2)
summary(pred_ship$predicted_groundspeed_gs1)

# attach predictions to the hauls dataset
list<-pred_ship$NewUniqueID2
hauls$predicted_groundspeed_gs1[hauls$NewUniqueID2%in%list] <- pred_ship$predicted_groundspeed_gs1[hauls$NewUniqueID2%in%list]

list<-pred_gear$NewUniqueID2
hauls$predicted_groundspeed_gs2[hauls$NewUniqueID2%in%list] <- pred_gear$predicted_groundspeed_gs2[hauls$NewUniqueID2%in%list]

# If real data available use that
hauls$GroundSpeed_Used<-hauls$GroundSpeed
hauls$GroundSpeed_Used[hauls$GroundSpeed_Used < 2] <- NA
hauls$GroundSpeed_Used[hauls$GroundSpeed_Used > 6] <- NA
hauls$GroundSpeed_Used[hauls$UniqueIDP=="IE-IGFS_2015_4_CEXP_100_GOV"] <- NA
hauls$GroundSpeed_Quality_Code[!is.na(hauls$GroundSpeed_Used)]<-"Recorded_Groundspeed"

# If gear is ROT then no data ever available
hauls$GroundSpeed_Quality_Code[hauls$Gear=="ROT"] <- "Manual_Speed"
hauls$GroundSpeed_Used[hauls$Gear=="ROT"] <- 4

# If ground speed available for Ship and Gear - use that
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)] <- "model_1"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)] <- hauls$predicted_groundspeed_gs1[is.na(hauls$GroundSpeed_Used)]

# if ground speed available for Gear
summary(hauls$predicted_groundspeed_gs2)
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)] <- "model_2"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)] <- hauls$predicted_groundspeed_gs2[is.na(hauls$GroundSpeed_Used)]

# If no model fits use Manual speed
hauls$GroundSpeed_Quality_Code[is.na(hauls$GroundSpeed_Used)] <- "Manual_Speed"
hauls$GroundSpeed_Used[is.na(hauls$GroundSpeed_Used)]<-4

summary(hauls$GroundSpeed_Used)
summary(as.factor(hauls$GroundSpeed_Quality_Code))

hauls$GroundSpeed_meters_per_min <- hauls$GroundSpeed_Used * 1852 / 60
summary(hauls$GroundSpeed_meters_per_min)


summary(hauls$GroundSpeed_Used[hauls$Gear=="BT7"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT8"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4A"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="GOV"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT3"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4A"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="ROT"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4AI"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4P"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT4S"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="BT6"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="GOV"])
summary(hauls$GroundSpeed_Used[hauls$Gear=="H18"])

#get equation for gs1
formula(gs_model1)
summary(gs_model1)
coeffs=(coefficients(gs_model1))
formula(gs_model2)
coeffs=(coefficients(gs_model2))

# draw a graph showing predicted v actual ground speeds.
png(file="groundspeed_predictedVactual.png", bg="transparent")
predict1 <- predict(gs_model1)
gs<-subset(hauls, !is.na(hauls$GroundSpeed),)
plot(predict1, gs$GroundSpeed, xlab="Predicted GroundSpeed (knots)",
     ylab="Recorded Groundspeed (knots)", pch=19)
predict2 <- predict(gs_model2)
points(predict2, gs$GroundSpeed, pch=19, col="lightgrey")
abline(0,1, col="red")
dev.off()



# Towed Distance -----------------------

## Calculate haversine distance with shoot and haul coordinates ##
names(hauls)
hauls$LatLongDistance <- (earth.dist(long1 = hauls$ShootLong,
                                      lat1 = hauls$ShootLat,
                                      long2 = hauls$HaulLong,
                                      lat2 = hauls$HaulLat) * 1000)
summary(hauls$LatLongDistance)

hauls1 <- as.data.table(hauls)

## RAW DISTANCE ##
hauls1[!is.na(Distance), c("newDist", "qualityDistance") :=
        list(Distance, "rawDistance")]

## HAVERSINE DISTANCE ##
# if haul and shoot coordinates are the same (i.e., equal to zero) then leave as NA
# Also ingore hauls with funny implied speed.
hauls1[is.na(Distance) & !is.na(LatLongDistance) & LatLongDistance > 1 &
        LatLongDistance/HaulDur>61.73 & LatLongDistance/HaulDur<185.2 ,
      c("newDist", "qualityDistance") :=
        list(LatLongDistance, "LatLongDistance")]

## DURATION X SPEED ##
# HaulDur x GroundSpeed
hauls1[, SpeedTimeDist := hauls1$GroundSpeed_meters_per_min*HaulDur]

hauls1[ !is.na(SpeedTimeDist) &is.na(newDist)&
         SpeedTimeDist/HaulDur>61.73 & SpeedTimeDist/HaulDur<185.2,
       c("newDist", "qualityDistance") :=
         list( SpeedTimeDist, "SpeedHaulDur")]

# Hauls that don't have raw distance, shoot/haul coordinates, or GroundSpeed can be estimated
# using linear models to predict GroundSpeed. Two different types of missing data are found:
# 1) ships that have no GroundSpeed records and we need to estimate from other ships, and
# 2) ships that have only a few missing GroundSpeed records and we can use ship as a factor in the lm
# Model for estimating Ground Speed has been reviewed based on feedback 
# from the IBTS working Group (V.T)
summary(as.numeric(hauls1$newDist))

# check new distances relationships
png(file="Data_QA_Process_V5_2022/Diagnostics/distance_speed_time_differences.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls1$SpeedTimeDist, hauls1$newDist, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
dev.off()
# Check distances within bounds
png(file="Data_QA_Process_V5_2022/Diagnostics/distance_speed_time_differences_bounds.png", bg = "transparent")
par(xpd=FALSE)
plot(hauls1$SpeedTimeDist, hauls1$newDist, 
     pch=19, col="black", cex=0.5, xlab="Speed X Time", ylab="Distance")
abline(a=0, b=1, col="lightgrey", lwd=2)
abline(a=0, b=.5, col="red", lwd=2, lty=2)
abline(a=0, b=1.5, col="red", lwd=2, lty=2)
dev.off()

plot(hauls1$HaulDur, hauls1$newDist, pch=19, cex=0.5, xlab="Time", ylab="Distance")
# perfect speed - 4 knots, bounds 2- 6 knots

dist <- hauls[hauls$Distance > 10000,]
dist <- dist[!is.na(dist$Distance),]

x<-c(13:66)
points(x, x*4*1852/60, type="l", col="red", lwd=3)
points(x, x*2*1852/60, type="l", col="red", lty=2, lwd=2)
points(x, x*6*1852/60, type="l", col="red", lty=2, lwd=2)

# Set up extra check col
hauls1$manual_speed_distance<-hauls1$HaulDur*4*1852/60
# distance checks required as some of them are really odd.
hauls1$LatLongDistance[hauls1$LatLongDistance==0] <- NA
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance > 1.50|
                         hauls1$newDist/hauls1$manual_speed_distance < 0.5, )

points(outside_bounds$HaulDur,outside_bounds$newDist, col="red", pch=19)
# only 177 to check :) - RK - Meadhbh had 127
# now 293


# if haversine is available use that
list <- outside_bounds$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list] < -hauls1$LatLongDistance[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list] <- "LatLongDistance"

# recheck 
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance>1.50|
                         hauls1$newDist/hauls1$manual_speed_distance<.5, )

points(outside_bounds$HaulDur,outside_bounds$newDist, col="blue", pch=19)

# still not gone, use speed*time
list <- outside_bounds$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list]<-hauls1$SpeedTimeDist[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list]<-"SpeedHaulDur"

# recheck
outside_bounds<-subset(hauls1, hauls1$newDist/hauls1$manual_speed_distance>1.50|
                         hauls1$newDist/hauls1$manual_speed_distance<.5, )

# all within bounds now

# next refine estimates
# is distance used within 20% of lat long distance 
check__dist_haversine_match <- subset(hauls1, hauls1$newDist/hauls1$LatLongDistance>1.2|
                                      hauls1$newDist/hauls1$LatLongDistance<.8,)
points(check__dist_haversine_match$HaulDur, check__dist_haversine_match$newDist, col="green", pch=19)
# so 1743 points more than 20% away - all rest are fine
check_dist_speed_match<-subset(check__dist_haversine_match, 
                               check__dist_haversine_match$newDist/check__dist_haversine_match$SpeedTimeDist>1.2|
                                 check__dist_haversine_match$newDist/check__dist_haversine_match$SpeedTimeDist<.8,)
# 245 not okay with speed
points(check_dist_speed_match$HaulDur,check_dist_speed_match$newDist, col="yellow", pch=19)

check_lat_speed_match<-subset(check_dist_speed_match, 
                              check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist<1.2&
                                check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist>.8,)
# so 97 of the haversine and speed X time are within 20% - use the lat long rather than the 
# recorded distance
list<-check_lat_speed_match$NewUniqueID2
hauls1$newDist[hauls1$NewUniqueID2%in%list]<-hauls1$LatLongDistance[hauls1$NewUniqueID2%in%list]
hauls1$qualityDistance[hauls1$NewUniqueID2%in%list]<-"LatLongDistance"
# So whats left
check_no_match<-subset(check_dist_speed_match, 
                       check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist>1.2|
                         check_dist_speed_match$LatLongDistance/check_dist_speed_match$SpeedTimeDist<.8,)

points(check_no_match$HaulDur,check_no_match$newDist, col="black", pch=19)
# Does value lie within +/- 25% of Man Speed
check_within_bounds<-subset(check_no_match, 
                            check_no_match$newDist/check_no_match$manual_speed_distance>1.25|
                              check_no_match$newDist/check_no_match$manual_speed_distance<.75, )
# check all distances available?
summary(hauls1$newDist)
# problem with some lat long distances not available
# Use manual speed 
hauls1$qualityDistance[is.na(hauls1$newDist)]<-"SpeedHaulDur"
hauls1$newDist[is.na(hauls1$newDist)]<-hauls1$SpeedTimeDist[is.na(hauls1$newDist)]
summary(hauls1$newDist)

# all vaules within accepted bounds.
png(file="Data_QA_Process_V5_2022/Diagnostics/distances_cleaned.png", bg = "transparent")
plot(hauls1$HaulDur, hauls1$newDist, pch=19, cex=0.5, xlab="Time", ylab="Distance")
# perfect speed - 4 knots, bounds 2- 6 knots
x<-c(13:66)
points(x, x*4*1852/60, type="l", col="red", lwd=3)
points(x, x*2*1852/60, type="l", col="red", lty=2, lwd=2)
points(x, x*6*1852/60, type="l", col="red", lty=2, lwd=2)
dev.off()
# all distances in newDist are withing acceptable ranges the best estimate is applied in
# each situation
write.csv(hauls1, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/Working_hauls_12-06-2021.csv")
# remove all the intermediate files 
summary(as.factor(hauls1$qualityDistance))




# The GOV Otter Trawl (Model 1) ------------------------

hauls<-hauls1
# remove outlier from irish data
summary(hauls$WingSpread[hauls$Country=="IE"])
# 38 too big
hauls$WingSpread[hauls$Country=="IE" & hauls$WingSpread==38]<-NA
#Centered data for model
summary(hauls$WingSpread)
summary(hauls$DoorSpread)
summary(hauls$Netopening)
# all -9 or 0 values need to be NA
hauls$WingSpread[which(hauls$WingSpread %in% c(0,-9))]<-NA
hauls$DoorSpread[hauls$DoorSpread%in% c(0,-9)]<-NA
hauls$Netopening[hauls$Netopening%in% c(0,-9)]<-NA
summary(hauls$WingSpread)
summary(hauls$DoorSpread)
summary(hauls$Netopening)
summary(hauls$DepthNew)
str(hauls)
# Add Sweep Cats - See Est Sweep Cats above.
hauls[!is.na(Year), "Yearfac":=
        as.factor(Year)]
hauls[!is.na(Quarter), "Qfac":=
        as.factor(Quarter)]
hauls[!is.na(Ship), "Shipfac":=
        as.factor(Ship)]
summary(as.factor(hauls$EstSweepCat[hauls$Gear=="GOV"]))
options(show.signif.stars = T)
library(lmerTest)

#Center data to give model stability 
hauls[!is.na(DepthNew), c("meanDepth") :=
        mean(DepthNew)]
hauls[!is.na(WingSpread), c("meanWingSpread") :=
        mean(WingSpread)]
hauls[!is.na(Netopening), c("meanNetopening") :=
        mean(Netopening)]
hauls[!is.na(DoorSpread), c("meanDoorSpread"):=
        mean(DoorSpread)]
hauls[!is.na(Depth), c("DepthCenter") :=
        Depth-meanDepth]

## DepthNew causing problems as there are a couple of negative estimated depths.
#temporary subset to get around this before talking to Ruth
hauls <- hauls[hauls$DepthNew >= 0,]

hauls[!is.na(DepthNew), c("LogDepthCenter") :=
        log(DepthNew)-log(meanDepth)] 

hauls[!is.na(WingSpread), c("WingSpreadCenter") :=
        WingSpread-meanWingSpread]
hauls[!is.na(DoorSpread), c("DoorSpreadCenter") :=
        DoorSpread-meanDoorSpread]
hauls[!is.na(WingSpread), c("LogWingSpreadCenter") :=
        log(WingSpread)-log(meanWingSpread)]
hauls[!is.na(DoorSpread), c("LogDoorSpreadCenter") :=
        log(DoorSpread)-log(meanDoorSpread)]
hauls[!is.na(Netopening), c("NetopeningCenter") :=
        Netopening-meanNetopening]
hauls[!is.na(Netopening), c("LogNetopeningCenter") :=
        log(Netopening)-log(meanNetopening)]
hauls$SweepCat<-as.factor(hauls$SweepLngt)
summary(as.factor(hauls$Ship))
summary(hauls)
train <- subset(hauls, Gear=="GOV"& (!is.na(WingSpread)) & (!is.na(DoorSpread))
              &(!is.na(Netopening)),)

library(lme4)
ws_model1<- lmer(log(WingSpread) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat),
           data=train, REML=FALSE) 

r.squaredGLMM(ws_model1)
unique(train$Ship)
summary(ws_model1)
cols<-rainbow(6)

data<-predict(ws_model1)
summary(as.factor(train$Survey))
summary(train$Survey)
cols<-rainbow(13)
png(file="Data_QA_Process_V5_2022/Diagnostics/GOV_Wingspreads_model1.png", bg="transparent")
plot(train$DepthNew, train$WingSpread, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="WingSpread(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$WingSpread~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="red", lwd=3, lty=2)
legend(300, 40, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=3, cex=1, bty="o")
dev.off()

# set up user data using selected model
# subset all GOV gear
hauls$EstSweepCat<-as.factor(hauls$EstSweepCat)
hauls$Ship<-as.factor(hauls$Ship)
the_gov <- subset(hauls, Gear=="GOV")
summary(the_gov$WingSpread)
summary(the_gov$LogDoorSpreadCenter)
str(the_gov) # 38963 obs

the_gov$mod1_wingspread_gov <- exp(predict(ws_model1, the_gov, allow.new.levels=T))
summary(the_gov$mod1_wingspread_gov)
# If real values are available use these
hauls$Use_WingSpread[!is.na(hauls$WingSpread)&
                       hauls$Gear=="GOV"]<-hauls$WingSpread[!is.na(hauls$WingSpread)&
                                                              hauls$Gear=="GOV"]
hauls$QualityWing[!is.na(hauls$WingSpread)] <- "raw_wingspread"

list <- the_gov$UniqueID
hauls$mod1_wingspread_gov[hauls$UniqueID%in%list]<-the_gov$mod1_wingspread_gov[the_gov$UniqueID%in%list]

hauls$Use_WingSpread[is.na(hauls$WingSpread)&
                       hauls$Gear=="GOV"]<-hauls$mod1_wingspread_gov[is.na(hauls$WingSpread)&
                                                                       hauls$Gear=="GOV"]
hauls$QualityWing[is.na(hauls$WingSpread)&!is.na(hauls$mod1_wingspread_gov)&
                    hauls$Gear=="GOV"] <- "mod1_wing_gov"

#check wing spreads 
summary((hauls$Use_WingSpread[hauls$Gear=="GOV"]))
summary(hauls$Use_WingSpread)
# get model outputs
formula(ws_model1)
summary(ws_model1)
coeffs=(coefficients(ws_model1))


# The NCT Gear --------------------------

# WingSpread = 15.1
hauls$WingSpread[hauls$Gear=="NCT"]<-15.1
hauls$Use_WingSpread[hauls$Gear=="NCT"]<-15.1
hauls$QualityWing[hauls$Gear=="NCT"]<-"mean_wingspread"


# The GOV Otter Trawl (Model 1) -----------------------

# for model election set up training data set
train <- subset(hauls, Gear=="GOV"& (!is.na(WingSpread)) & (!is.na(DoorSpread))
              &(!is.na(Netopening)),)

model1_ds <- lmer(log(DoorSpread) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat), 
           data=train, REML=FALSE)
summary(model1_ds)
r.squaredGLMM(model1_ds)
cols<-rainbow(13)
data<-predict(model1_ds)
summary(as.factor(train$Survey))
png(file="Data_QA_Process_V5_2022/Diagnostics/GOV_Doorspreads_model1.png", bg="transparent")
plot(train$DepthNew, train$DoorSpread, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="WingSpread(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$DoorSpread~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="lightgrey", lwd=3, lty=2)
legend(530, 80, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=2, cex=.9, bty="o")
dev.off()

# note - less compex limear model can't explain as much variance as mixed model
# the random effects are accounting for quite a lot of variance
# set up user data using selected mode # subset all GOV gear
the_gov<-subset(hauls, Gear=="GOV") # 38973 obs

the_gov$mod1_doorspread_gov<-exp(predict(model1_ds, the_gov, allow.new.levels=T))
summary(the_gov$mod1_doorspread_gov)
# If real values are available use these
# predict results for spains net opening data
hauls$Use_DoorSpread[!is.na(hauls$DoorSpread)&
                       hauls$Gear=="GOV"]<-hauls$DoorSpread[!is.na(hauls$DoorSpread)&
                                                              hauls$Gear=="GOV"]
hauls$QualityDoor[!is.na(hauls$DoorSpread)]<-"raw_doorspread"

list < -the_gov$UniqueID
hauls$mod1_doorspread_gov[hauls$UniqueID%in%list]<-the_gov$mod1_doorspread_gov[the_gov$UniqueID%in%list]

hauls$Use_DoorSpread[is.na(hauls$DoorSpread)&
                       hauls$Gear=="GOV"]<-hauls$mod1_doorspread_gov[is.na(hauls$DoorSpread)&
                                                                       hauls$Gear=="GOV"]
hauls$QualityDoor[is.na(hauls$DoorSpread)&!is.na(hauls$mod1_doorspread_gov)&
                    hauls$Gear=="GOV"]<-"mod1_door_gov"
#check door spreads
summary((hauls$Use_DoorSpread[hauls$Gear=="GOV"]))
summary(as.factor(hauls$QualityDoor[hauls$Gear=="GOV"]))
summary(hauls$Use_DoorSpread)
summary(model1_ds)
coeffs=coefficients(model1_ds);coeffs


# The ROT Trawl (Model 2) --------------------------------

summary(hauls$DoorSpread[hauls$Gear=="ROT"])
# Doorspread can be sorted first
# DoorSpread only has 955 missing values to be estimated

length(hauls$DoorSpread[is.na(hauls$DoorSpread)]) ## now 40308....?

png(file = "Data_QA_Process_V5_2022/Diagnostics/doorspread_ROT.png", bg = "transparent")
plot(hauls$Depth[hauls$Gear=="ROT"], hauls$DoorSpread[hauls$Gear=="ROT"], 
     pch=19, xlab="Depth (m)",
     ylab="Door Spread (m)")
dev.off()
x<-hauls$Depth[hauls$Gear=="ROT"]
y<-hauls$DoorSpread[hauls$Gear=="ROT"]

plot(y~x,type="n")
m = lm(y~x)
wx = par("usr")[1:2]
new.x = seq(wx[1],wx[2],len=100)
pred = predict(m, new=data.frame(x=new.x), interval="conf", level=.95)
lines(new.x,pred[,"fit"],lwd=2)
lines(new.x,pred[,"lwr"],lty=3)
lines(new.x,pred[,"upr"],lty=3)
points(x,y,pch=16,col="steelblue")

# raw data looks good - no worrying outliers
corrhaul_rot<-subset(hauls, Gear=="ROT",
                     select=c(Year, Depth, Distance,
                              DoorSpread))
summary(corrhaul_rot)
require(corrgram)
png(file = "Data_QA_Process_V5_2022/Diagnostics/corrhaul_ROT.png", bg = "transparent")
corrgram(corrhaul_rot, order="PCA", lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Hauls Data NI") 
dev.off()

# the only variable available to estimate doorspread is depth for ROT ship
# no sweep, no changes in gear , no wing or netopening
model2<-lm(log(DoorSpread[hauls$Gear=="ROT"])~log(Depth[hauls$Gear=="ROT"]), data=hauls)
AIC(model2)
summary(model2)
coeff=coefficients(model2); coeff
png(file = "logged_doorspread_ROT_29-09-2016.png", bg = "transparent")
plot(log(hauls$DepthNew[hauls$Gear=="ROT"]), log(hauls$DoorSpread[hauls$Gear=="ROT"]), pch=19, xlab="logged Depth (m)",
     ylab="logged Doorspread (m)")
abline(a=(2.558093), b=(0.261106) , col="red", lwd=2)
dev.off()

hauls$mod_doorspread_rot[hauls$Gear=="ROT"] <- exp(coeff[1]+coeff[2]*log(hauls$DepthNew[hauls$Gear=="ROT"]))
# set up user values for doorspread
hauls$Use_DoorSpread[!is.na(hauls$DoorSpread)&
                       hauls$Gear=="ROT"]<-hauls$DoorSpread[!is.na(hauls$DoorSpread)&
                                                              hauls$Gear=="ROT"]
hauls$QualityDoor[!is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-"raw_doorspread"
hauls$Use_DoorSpread[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-hauls$mod_doorspread_rot[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]
hauls$QualityDoor[is.na(hauls$DoorSpread)&hauls$Gear=="ROT"]<-"model_doorspread_rot"

summary(hauls$mod_doorspread_rot[hauls$Gear=="ROT"])
summary(hauls$Use_DoorSpread[hauls$Gear=="ROT"])
summary(hauls$Use_DoorSpread)
summary(as.factor(hauls$QualityDoor))


# The ROT Trawl (Model 2) -------------------------------

summary(hauls$WingSpread[hauls$Gear=="ROT"])
# 2333 estimated values needed # 2842 in 2021 #4327 in 2022
# Matt sent me some trail wingspreads to help to model these data
# but data now available in DATRAS file 20 values available
# lets look at these data first
png(file = "Data_QA_Process_V5_2022/Diagnostics/wingspread_ROT.png", bg = "transparent")
plot(hauls$DepthNew[hauls$Gear=="ROT"],hauls$WingSpread[hauls$Gear=="ROT"],
     pch=19, xlab="Depth (m)",
     ylab="Wingspread (m)" )
abline(lm(hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]~
            hauls$DepthNew[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]),
       col="lightgrey")
dev.off()
# given how data poor the situation is this must be kept very simple
ws_dat<-subset(hauls, !is.na(hauls$WingSpread) & hauls$Gear=="ROT", )
summary(ws_dat)
# need doorspread first
# Great we can keep this really simple and have a really strong
# Adjusted R sqd of 0.952
# has the best AIC score (-153.47.6)
hauls$mod2_wingspread_rot[hauls$Gear=="ROT"] <- exp(0.3798356+0.6489731*log(hauls$Use_DoorSpread[hauls$Gear=="ROT"]))

# set up user values for doorspread
#RAW WINGSPREAD
hauls$Use_WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"] <- hauls$WingSpread[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"]
hauls$QualityWing[!is.na(hauls$WingSpread)&hauls$Gear=="ROT"] <- "raw_wingspread"
hauls$Use_WingSpread[is.na(hauls$WingSpread)&hauls$Gear=="ROT"] < -hauls$mod2_wingspread_rot[is.na(hauls$WingSpread)&hauls$Gear=="ROT"]
hauls$QualityWing[is.na(hauls$WingSpread)&hauls$Gear=="ROT"] <- "model_wingspread_rot"

summary(hauls$mod2_wingspread_rot[hauls$Gear=="ROT"])
summary(hauls$Use_WingSpread[hauls$Gear=="ROT"])


# The NCT Gear ------------------------------------

# No sensors on this gear so mean is applied as supplied by ICES 2010
# DoorSpread 45.7m
hauls$DoorSpread[hauls$Gear=="NCT"]<- 45.7
# set up user values
hauls$Use_DoorSpread[hauls$Gear=="NCT"]<-45.7
hauls$QualityDoor[hauls$Gear=="NCT"]<-"mean_doorspread"


# Net Opening GOV -----------------------

plot(train$DepthNew,train$Netopening )
plot(train$WingSpread,train$Netopening )

cols<-rainbow(6)
summary(as.factor(train$Survey))
png(file="Data_QA_Process_V5_2022/Diagnostics/GOV_Netopening_model1.png", bg="transparent")
plot(train$DepthNew, train$Netopening, col=cols[as.factor(train$Survey)], 
     pch=15, xlab="Depth (m)", ylab="Netopening(m)")
points(train$DepthNew, exp(data), pch=21, col=cols[as.factor(train$Survey)],
       bg="black")
lines(lowess(train$Netopening~train$DepthNew), col="black", lwd=2)
lines(lowess(exp(data)~train$DepthNew), col="lightgrey", lwd=3, lty=2)
legend(600, 9, levels(as.factor(hauls$Survey[hauls$Gear=="GOV"])), 
       col=cols, pch=15, ncol=1, cex=1, bty="n")
dev.off()
# note - less compex model can't explain as much variance as mixed model
# the random effects are accounting for quite a lot of variance
# set up user data using selected model
# subset all GOV gear
the_gov<-subset(hauls, Gear=="GOV")
# 30885 obs
nm1<- lmer(log(Netopening) ~ LogDepthCenter:EstSweepCat
           + (1|Ship:EstSweepCat), 
           data=train, REML=FALSE)
summary(nm1)
r.squaredGLMM(nm1)
formula(nm1)
coeffs=coefficients(nm1);coeffs
the_gov$mod1_net_gov<-exp(predict(nm1, the_gov, allow.new.levels=T))
summary(the_gov$mod1_net_gov)
# If real values are available use these
# predict results for spains net opening data
hauls$Use_Netopening[!is.na(hauls$Netopening)&
                       hauls$Gear=="GOV"]<-hauls$Netopening[!is.na(hauls$Netopening)&
                                                              hauls$Gear=="GOV"]
hauls$QualityNet[!is.na(hauls$Netopening)]<-"raw_netopening"

list<-the_gov$NewUniqueID2

hauls$mod1_net_gov[hauls$NewUniqueID2%in%list]<-the_gov$mod1_net_gov[the_gov$NewUniqueID2%in%list]

hauls$Use_Netopening[is.na(hauls$Netopening)&
                       hauls$Gear=="GOV"]<-hauls$mod1_net_gov[is.na(hauls$Netopening)&
                                                                hauls$Gear=="GOV"]
hauls$QualityNet[is.na(hauls$Netopening)&!is.na(hauls$mod1_net_gov)&
                   hauls$Gear=="GOV"]<-"mod1_net_gov"

#check door spreads
summary((hauls$Use_Netopening[hauls$Gear=="GOV"]))
summary(as.factor(hauls$QualityNet[hauls$Gear=="GOV"]))

summary((hauls$Use_Netopening))
summary(as.factor(hauls$QualityNet))
summary((hauls$Use_DoorSpread))
summary(as.factor(hauls$QualityDoor))
summary((hauls$Use_WingSpread))
summary(as.factor(hauls$QualityWing))



# The ROT Trawl (Model 2) --------------------------

# Not enough data to model this attribute - either use mean value or don't do 
# vol estimates with ROT gear - similar to Beam gears!
# Matt sent me the Standard Gear Specs- IBTS Report 2013
hauls$Use_Netopening[!is.na(hauls$Netopening)&
                       hauls$Gear=="ROT"]<-hauls$Netopening[!is.na(hauls$Netopening)&
                                                              hauls$Gear=="ROT"]
hauls$QualityNet[!is.na(hauls$Netopening)&
                   hauls$Gear=="ROT"]<-"raw_netopening"
hauls$Use_Netopening[is.na(hauls$Netopening)&hauls$Gear=="ROT"]<-3
hauls$QualityNet[is.na(hauls$Netopening)&hauls$Gear=="ROT"]<-"mean_netopening"
summary(hauls$Use_Netopening[hauls$Gear=="ROT"])


# The NCT Gear -------------------------------

# netopening 4.6m
hauls$Netopening[hauls$Gear=="NCT"]<-4.6
hauls$Use_Netopening[hauls$Gear=="NCT"]<-4.6
hauls$QualityNet[hauls$Gear=="NCT"]<-"mean_netopening"
summary(hauls$Use_Netopening[hauls$Gear=="NCT"])


# Beam Trawl DoorSpread, WingSpread, and Netopening ----------------------

# Beams have a set "wing spread" so set wing and door as the set width
# net opening will be set to NA
summary(as.factor(hauls$Gear))
# Netherlands
# DoorSpread 8m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT8"]<- 8
# WingSpread = 8
hauls$WingSpread[hauls$Gear=="BT8"]<-8
# netopening NA
hauls$Netopening[hauls$Gear=="BT8"]<-.8
# Germany 
# DoorSpread 7.2m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT7"]<-7.2
# WingSpread = 7.2
hauls$WingSpread[hauls$Gear=="BT7"]<-7.2
# netopening NA
hauls$Netopening[hauls$Gear=="BT7"]<-.6
# UK  (BTS manual 2009)
# DoorSpread 7.2m (BTS manual 2009)
hauls$DoorSpread[hauls$Gear=="BT4A"]<-4
# WingSpread = 7.2
hauls$WingSpread[hauls$Gear=="BT4A"]<-4
# netopening NA
hauls$Netopening[hauls$Gear=="BT4A"]<-.525
# setup user values 
hauls$Use_DoorSpread[hauls$Gear=="BT4A"]<-4
hauls$QualityDoor[hauls$Gear=="BT4A"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT4A"]<-4
hauls$QualityWing[hauls$Gear=="BT4A"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT4A"]<-.525
hauls$QualityNet[hauls$Gear=="BT4A"]<-"raw_netopening"

hauls$Use_DoorSpread[hauls$Gear=="BT7"]<-7.2
hauls$QualityDoor[hauls$Gear=="BT7"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT7"]<-7.2
hauls$QualityWing[hauls$Gear=="BT7"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT7"]<-.6
hauls$QualityNet[hauls$Gear=="BT7"]<-"raw_netopening"

hauls$Use_DoorSpread[hauls$Gear=="BT8"]<-8
hauls$QualityDoor[hauls$Gear=="BT8"]<-"raw_doorspread"
hauls$Use_WingSpread[hauls$Gear=="BT8"]<-8
hauls$QualityWing[hauls$Gear=="BT8"]<-"raw_wingspread"
hauls$Use_Netopening[hauls$Gear=="BT8"]<-.8
hauls$QualityNet[hauls$Gear=="BT8"]<-"raw_netopening"

summary(hauls$Use_WingSpread[hauls$Gear=="BT4A"])

### RK 2021 I still have NA's in key fields.. bound to be the BAK gear

table(hauls$Gear[is.na(hauls$Use_WingSpread)])
table(hauls$Gear[is.na(hauls$Use_DoorSpread)])
table(hauls$Gear[is.na(hauls$Use_Netopening)])
### yup!
table(hauls$Gear[is.na(hauls$Use_Distance)])
table(hauls$Gear[is.na(hauls$Use_Depth)])
summary(hauls$GroundSpeed_Used)
### these are okay though

### ommitting BAK because there are odd outliers in 
# wingspread and doorspread plots when calculated

write.csv(hauls, "Data_QA_Process_V5_2022/Diagnostics/Diagnostic_data/hauls_monster_file_04_2022.csv")


# Calculation of the Area/Volume Swept by the Trawl --------------

summary(hauls$Use_WingSpread)
summary(hauls$Use_DoorSpread)
summary(hauls$Use_Netopening)
summary(hauls$newDist)

plot(hauls$DepthNew[hauls$Country=="IE"], hauls$Use_WingSpread[hauls$Country=="IE"])

hauls$SweptArea_wing_m_sqrd <- hauls$Use_WingSpread*hauls$newDist
hauls$SweptArea_wing_km_sqrd <- hauls$SweptArea_wing_m_sqrd/1000/1000
summary(hauls$SweptArea_wing_km_sqrd)
summary(hauls$Survey)
cols<-topo.colors(13)
plot(hauls$HaulDur,hauls$newDist, pch=19, col=cols[hauls$Survey])

plot(hauls$HaulDur,hauls$SweptArea_wing_km_sqrd, pch=19, col=cols[hauls$Survey])

16.05/68.12
16.05/0.23
hauls[, c("QualityWing_SweptArea") := list(paste0(qualityDistance, hauls$QualityWing)),]  
check_speed <- hauls$newDist/hauls$HaulDur

summary(check_speed)
185.20/1854*60

hauls[, c("Wing/Door(Ratio)"):= list(hauls$Use_WingSpread/hauls$Use_DoorSpread),]
# Save "raw" files - before all estimated data is added
write.csv(HH, "Data_QA_Process_V5_2022/Raw_Combined_Data-end-haul-QA.csv")
# remove from R environment
# Save monster HH Chron File
write.csv(hauls, "Data_QA_Process_V5_2022/final_full_cleaned_hauls-end-haul-QA.csv")

# take a list of haul IDs for use in the HL observation selection 
list<-unique(hauls$NewUniqueID2)
list1<-unique(HL$NewUniqueID2)
length(list)-length(list1)
setdiff(list1, list)
setdiff( list, list1) ### these are due to removing 'BAK'


HL1 <- subset(HL, NewUniqueID2%in%list)

save(list=ls(all=T), file = "./script6_output.rda")

