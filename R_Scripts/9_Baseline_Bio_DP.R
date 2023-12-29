

# This is Script 9 of 9 
# The purpose of this script is to seperate the surveys and 
# define the final data products structure for the biological data files
# Then select the Standard Survey Area using the agreed criteria 

dat8_1 <- dat_1   #### Ruth added this to connect to previous script, but not sure it's appropriate. check with Maedhbh

names(dat8_1)
summary(dat8_1$HLNoAtLngt_N)

dat8_1$ValidAphiaID.x<-NULL        
dat8_1$SweptArea_wing_km_sqrdM<-NULL  
dat8_1$ValidAphiaID.y<-NULL                        

names(h)
merger<-subset(h, select=c(NewUniqueID2,HaulID,Survey_Acronym, WingSpread_m,          
                           WingSwpArea_sqkm))

dat9<-join(merger,dat8_1, by="NewUniqueID2")
nrow(dat8_1)-nrow(dat9)
# Check length classes
names(dat9)
find<-subset(dat9, FishLength_cm_below>LmaxFB*1.4,)
# Check Abundances
# in 0.5527255% of hauls
# now use the new numbers at lenght to get densities
#dat4$HLNoAtLngt<-as.numeric(dat4$HLNoAtLngt)

dat9$SweptArea_wing_km_sqrd<-as.numeric(dat9$SweptArea_wing_km_sqrd)
dat9[!is.na(NewHLNoAtLngt), c("HLNoAtLngtkm2") :=
       list(NewHLNoAtLngt/WingSwpArea_sqkm)] 
dat9[!is.na(NewTotalNo), c("TotalNoKm2"):=
       list(NewTotalNo/WingSwpArea_sqkm)]
summary(dat9$TotalNoKm2)
summary(dat9$HLNoAtLngtkm2)
dat9[!is.na(HLNoAtLngtkm2), c("HLNoATLngt_Derived"):=
       list(HLNoAtLngtkm2*SweptArea_wing_km_sqrd)]
dat9[!is.na(TotalNoKm2), c("TotalNoKm2_Derived"):=
       list(TotalNoKm2*SweptArea_wing_km_sqrd)]
find<-subset(dat9, dat9$NewHLNoAtLngt_round> dat9$NewTotalNo_roundup,)
#Still some differences between total number and highter no at length measured
# what a total mess

dat9$DensAbund_N_Sqkm<-dat9$Density_Km2
summary(dat9$DensAbund_N_Sqkm)
find<-subset(dat9, DensAbund_N_Sqkm==0,)


# Non Standard Survery area - Master copy only
#names(dat9)
find<-subset(dat9, Density_Km2==0,)
summary(as.factor(dat9$estrank))
list<-c("Species", "Subspecies")
dat9$Filter[dat9$estrank%in%list]<-"OK"
dat9$Filter[!dat9$estrank%in%list]<-"SC"
dat9$Filter[dat9$FishLength_cm_below=="0"&dat9$estrank=="Species"]<-"LFD"
dat9$Filter[dat9$FishLength_cm_below=="0"&dat9$estrank=="Subspecies"]<-"LFD"
dat9$Filter[dat9$FishLength_cm_below=="0"&dat9$estrank=="Genus"]<-"SCLFD"
dat9$Filter[dat9$FishLength_cm_below=="0"&dat9$estrank=="Family"]<-"SCLFD"
dat9$Filter[dat9$estsciname=="Notoscopelus kroyeri"&!dat9$FishLength_cm_below=="0"]<-"OK"
dat9$Filter[dat9$estsciname=="Gobiidae"&!dat9$FishLength_cm_below=="0"]<-"OK"
dat9$Filter[dat9$estsciname=="Ammodytidae"&!dat9$FishLength_cm_below=="0"]<-"OK"
dat9$Filter[dat9$estsciname=="Gobiidae"&dat9$FishLength_cm_below=="0"]<-"LFD"
dat9$Filter[dat9$estsciname=="Ammodytidae"&dat9$FishLength_cm_below=="0"]<-"LFD"

summary(as.factor(dat9$Filter))

dat9$RecordStatus<-dat9$Filter
dat9$SciName<-dat9$estsciname
dat9$FishLength_cm<-(round(dat9$FishLength_cm_below))

dat9$Number<-dat9$Density_Km2*dat9$WingSwpArea_sqkm
dat9$Number_round<-ceiling(dat9$Number)

summary(dat9$Number_round)
dat9$DensAbund_N_Sqkm<-dat9$Number_round/dat9$SweptArea_wing_km_sqrd
summary(dat9$DensAbund_N_Sqkm)
dat9$Number<-dat9$Number_round

# CHECK PLOTS
for (cat in unique(dat9$SciName)){
  mypath <- file.path(paste("BASLINE_Length_Frequency_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(dat9, SciName == cat)
  plot(d$FishLength_cm, d$DensAbund_N_Sqkm, 
       main=unique(d$SciName), pch=19, xlab="Length (cm)", 
       ylab="Density at Length (km2)")
  dev.off()
}




###################
# Required Fields #
###################
## HaulID
## SpeciesSciName
## FishLength(cm)
## Number
## DensAbund(N_sqkm)
#######################
#Master Copey Baseline#
#######################
dat9<-subset(dat9, !(Survey_Acronym=="GNSIntOT3"&estsciname=="Acipenser sturio"),)
#Baseline<-subset(dat9, 
#                 select=c(HaulID, SciName, FishLength_cm,Number, DensAbund_N_Sqkm,RecordStatus))
#write.csv(Baseline, "BiologicalInfo_Allsurveys_Basline_V6.csv")
list<-h$HaulID

# Baseline_with_correct_start_date<-subset(dat9, HaulID%in%list,
#     select=c(HaulID, SciName, FishLength_cm,Number, DensAbund_N_Sqkm,RecordStatus))


Baseline_with_correct_start_date<-dplyr::select(dat9, HaulID, SciName, FishLength_cm,Number, DensAbund_N_Sqkm,RecordStatus)

Baseline_Biological_sum<-ddply(Baseline_with_correct_start_date,
                               c("HaulID", "SciName", "FishLength_cm", "RecordStatus"),
                          summarise,
                          Total_Number=sum(Number), 
                          Total_DensAbund_N_Sqkm=sum(DensAbund_N_Sqkm))
sum(Baseline_with_correct_start_date$DensAbund_N_Sqkm)
sum(Baseline_Biological_sum$Total_DensAbund_N_Sqkm)
sum(Baseline_with_correct_start_date$Number)
sum(Baseline_Biological_sum$Total_Number)
Baseline_Biological_sum$DensAbund_N_Sqkm<-Baseline_Biological_sum$Total_DensAbund_N_Sqkm
Baseline_Biological_sum$Number<-Baseline_Biological_sum$Total_Number
sum(Baseline_Biological_sum$DensAbund_N_Sqkm)
sum(Baseline_Biological_sum$Number)
Baseline_Biological_sum$Total_DensAbund_N_Sqkm<-NULL
Baseline_Biological_sum$Total_Number<-NULL

write.csv(Baseline_Biological_sum, "BiologicalInfo_AllSurveys_FullSMP_ Baseline_V2.csv")
#Baseline_with_correct_start_date<-read.csv("All_surveys_Baseline_with_correct_start_date_12-10-2016.csv")


unique(dat9$DayNight)

#### Initial export for Caroline 30/05/2022 ####

Baseline_Car<-dplyr::select(dat9, Survey,  Country, Ship, Gear, Year, Quarter, Month, StNo, HaulID, DepthNew, DayNight, SciName, ShootLat,  ShootLong, FishLength_cm, Number, DensAbund_N_Sqkm, RecordStatus)

### subset years 

Baseline_Car <- subset(Baseline_Car, Year > 2008)


Species_level <- Baseline_Car %>%
    filter(Survey != "NS-IBTS")  %>%
  group_by(Survey,  Country, Ship, Gear, Year, Quarter, Month, StNo, HaulID, DepthNew, SciName, ShootLat,  ShootLong) %>%
      dplyr::summarise(Total_DensAbund_N_Sqkm=sum(DensAbund_N_Sqkm, na.rm = TRUE))

write.csv(Baseline_Car, "OutputData/BiologicalInfo_withlengths_2009_on_30_05_2022.csv")
write.csv(Species_level, "OutputData/BiologicalInfo_per_species_2009_on_30_05_2022.csv")

