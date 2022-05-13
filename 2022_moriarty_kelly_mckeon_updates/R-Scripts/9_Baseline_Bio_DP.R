

# This is Script 9 of 9 
# The purpose of this script is to seperate the surveys and 
# define the final data products structure for the biological data files
# Then select the Standard Survey Area using the agreed criteria 

names(dat8_1)
summary(dat8_1$HLNoAtLngt_N)
WingSpread_m          
WingSwpArea_sqkm   
dat8_1$ValidAphiaID.x<-NULL        
dat8_1$SweptArea_wing_km_sqrdM<-NULL  
dat8_1$ValidAphiaID.y<-NULL                        

names(h)
merger<-subset(h, select=c(New_UniqueID,HaulID,Survey_Acronym, WingSpread_m,          
                           WingSwpArea_sqkm))

dat9<-join(merger,dat8_1, by="New_UniqueID")
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
list<-haul_dat1$HaulID

Baseline_with_correct_start_date<-subset(dat9, HaulID%in%list,
    select=c(HaulID, SciName, FishLength_cm,Number, DensAbund_N_Sqkm,RecordStatus))
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
################ 1 GNSGerBT3 ################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSGerBT3"])
Baseline_Full_GNSGerBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSGerBT3, "BiologicalInfo_GNSGerBT3_FullSMP_ Baseline_V2.csv")
################### 2. GNSNetBT3" ##################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSNetBT3"])
Baseline_Full_GNSNetBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSNetBT3, "BiologicalInfo_GNSNetBT3_FullSMP_ Baseline_V2.csv")
################## 3. GNSEngBT3" ##################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSEngBT3"])
Baseline_Full_GNSEngBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSEngBT3, "BiologicalInfo_GNSEngBT3_FullSMP_ Baseline_V2.csv")
################4. GNSIntOT1 ################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSIntOT1"])
Baseline_Full_GNSIntOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSIntOT1, "BiologicalInfo_GNSIntOT1_FullSMP_ Baseline_V2.csv")
################# 5. GNSIntOT3 #################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSIntOT3"])
Baseline_Full_GNSIntOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSIntOT3, "BiologicalInfo_GNSIntOT3_FullSMP_ Baseline_V2.csv")
################## 6 .GNSFraOT4  ##################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="GNSFraOT4"])
Baseline_Full_GNSFraOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_GNSFraOT4, "BiologicalInfo_GNSFraOT4_FullSMP_ Baseline_V2.csv")
################ 7. CSEngBT3 ################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSEngBT3"])
Baseline_Full_CSEngBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSEngBT3, "BiologicalInfo_CSEngBT3_FullSMP_ Baseline_V2.csv")
################ 8. CSScoOT1 ################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSScoOT1"])
Baseline_Full_CSScoOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSScoOT1, "BiologicalInfo_CSScoOT1_FullSMP_ Baseline_V2.csv")
################ 9. CSScoOT4 ################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSScoOT4"])
Baseline_Full_CSScoOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSScoOT4, "BiologicalInfo_CSScoOT4_FullSMP_ Baseline_V2.csv")
################# 10. CSIreOT4 #################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSIreOT4"])
Baseline_Full_CSIreOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSIreOT4, "BiologicalInfo_CSIreOT4_FullSMP_ Baseline_V2.csv")
################# 11. CSNIrOT1 #################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSNIrOT1"])
Baseline_Full_CSNIrOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSNIrOT1, "BiologicalInfo_CSNIrOT1_FullSMP_ Baseline_V2.csv")
################# 12. CSNIrOT4 #################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSNIrOT4"])
Baseline_Full_CNNIrOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CNNIrOT4, "BiologicalInfo_CSNIrOT4_FullSMP_ Baseline_V2.csv")
###################13. CS/BBFraOT4 ###################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="CSBBFraOT4"])
Baseline_Full_CSBBFraOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                select=c(HaulID, SciName, FishLength_cm,Number, 
                                         DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_CSBBFraOT4, "BiologicalInfo_CSBBFraOT4_FullSMP_ Baseline_V2.csv")
################### 14. BBICPorOT4 ###################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="BBICPorOT4"])
Baseline_Full_BBICPorOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                select=c(HaulID, SciName, FishLength_cm,Number, 
                                         DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_BBICPorOT4, "BiologicalInfo_BBICPorOT4_FullSMP_ Baseline_V2.csv")
################# 15. WAScoOT3 #################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="WAScoOT3"])
Baseline_Full_WAScoOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_WAScoOT3, "BiologicalInfo_WAScoOT3_FullSMP_ Baseline_V2.csv")
####################16 BBIC(n)SpaOT4 ####################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="BBIC(n)SpaOT4"])
Baseline_Full_BBICnSpaOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                 select=c(HaulID, SciName, FishLength_cm,Number, 
                                          DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_BBICnSpaOT4, "BiologicalInfo_BBICnSpaOT4_FullSMP_ Baseline_V2.csv")
####################17 BBIC(s)SpaOT1 ####################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="BBIC(s)SpaOT1"])
Baseline_Full_BBICsSpaOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                 select=c(HaulID, SciName, FishLength_cm,Number, 
                                          DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_BBICsSpaOT1, "BiologicalInfo_BBICsSpaOT1_FullSMP_ Baseline_V2.csv")
####################18 BBIC(s)SpaOT4 ####################
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="BBIC(s)SpaOT4"])
Baseline_Full_BBICsSpaOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                 select=c(HaulID, SciName, FishLength_cm,Number, 
                                          DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_BBICsSpaOT4, "BiologicalInfo_BBICsSpaOT4_FullSMP_ Baseline_V2.csv")
###############19 WASpaOT3 ###############
list<-unique(haul_dat1$HaulID[haul_dat1$Survey_Acronym=="WASpaOT3"])
Baseline_Full_WASpaOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                              select=c(HaulID, SciName, FishLength_cm,Number, 
                                       DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_Full_WASpaOT3, "BiologicalInfo_WASpaOT3_FullSMP_ Baseline_V2.csv")
###############
# SSA Baseline#
###############
SSA_haul_dat<-read.csv("SamplingInfo_AllSurveys_SSASMP_V2.csv")
list<-unique(SSA_haul_dat$HaulID)
Baseline_SSA<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA, "BiologicalInfo_AllSurveys_SSASMP_ Baseline_V2.csv")

# following on from previous file - create a biological file for each level
###############
# 1 GNSGerBT3 #
###############
list<-unique(SSA_GNSGerBT3$HaulID)

Baseline_SSA_GNSGerBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSGerBT3, "BiologicalInfo_GNSGerBT3_SSASMP_ Baseline_V2.csv")
################### 2. GNSNetBT3" ##################
#SSA_GNSNetBT3
list<-unique(SSA_GNSNetBT3$HaulID)
Baseline_SSA_GNSNetBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSNetBT3, "BiologicalInfo_GNSNetBT3_SSASMP_ Baseline_V2.csv")
################## 3. GNSEngBT3" ##################
#SSA_GNSEngBT3
list<-unique(SSA_GNSEngBT3$HaulID)
Baseline_SSA_GNSEngBT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                                   select=c(HaulID, SciName, FishLength_cm,Number, 
                                            DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSEngBT3, "BiologicalInfo_GNSEngBT3_SSASMP_ Baseline_V2.csv")
################4. GNSIntOT1"################
# SSA_GNSIntOT1
list<-unique(SSA_GNSIntOT1$HaulID)
Baseline_SSA_GNSIntOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSIntOT1, "BiologicalInfo_GNSIntOT1_SSASMP_ Baseline_V2.csv")
################# 5. GNSIntOT3 #################
#  SSA_GNSIntOT3
list<-unique(SSA_GNSIntOT3$HaulID)
Baseline_SSA_GNSIntOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSIntOT3, "BiologicalInfo_GNSIntOT3_SSASMP_Baseline_V2.csv")
################## 6 .GNSFraOT4  ##################
# SSA_GNSFraOT4
list<-unique(SSA_GNSFraOT4$HaulID)
Baseline_SSA_GNSFraOT4<-subset(dat9, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_GNSFraOT4, "BiologicalInfo_GNSFraOT4_SSASMP_Baseline_V2.csv")
################ 7. CSEngBT3 ################
# SSA_CSEngBT3
list<-unique(SSA_CSEngBT3$HaulID)
Baseline_SSA_CSEngBT3<-subset(dat9, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSEngBT3, "BiologicalInfo_CSEngBT3_SSASMP_ Baseline_V2.csv")
################ 8. CSScoOT1 ################
# SSA_CSScoOT1
list<-unique(SSA_CSScoOT1$HaulID)

Baseline_SSA_CSScoOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSScoOT1, "BiologicalInfo_CSScoOT1_SSASMP_ Baseline_V2.csv")
################ 9. CSScoOT4 ################
# SSA_CSScoOT4
list<-unique(SSA_CSScoOT4$HaulID)

Baseline_SSA_CSScoOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSScoOT4, "BiologicalInfo_CSScoOT4_SSASMP_ Baseline_V2.csv")
################# 10. CSIreOT4 #################
# SSA_CSIreOT4
list<-unique(SSA_CSIreOT4$HaulID)
Baseline_SSA_CSIreOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSIreOT4, "BiologicalInfo_CSIreOT4_SSASMP_ Baseline_V2.csv")
################# 11. CSNIrOT1 #################
# SSA_CSNIrOT1
list<-unique(SSA_CSNIrOT1$HaulID)

Baseline_SSA_CSNIrOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSNIrOT1, "BiologicalInfo_CSNIrOT1_SSASMP_ Baseline_V2.csv")
################# 12. CSNIrOT4 #################
# SSA_CNNIrOT4
list<-unique(SSA_CNNIrOT4$HaulID)

Baseline_SSA_CNNIrOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CNNIrOT4, "BiologicalInfo_CSNIrOT4_SSASMP_ Baseline_V2.csv")
###################13. CS/BBFraOT4 ###################
# SSA_CSBBFraOT4
list<-unique(SSA_CSBBFraOT4$HaulID)

Baseline_SSA_CSBBFraOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_CSBBFraOT4, "BiologicalInfo_CSBBFraOT4_SSASMP_ Baseline_V2.csv")
################### 14. BBICPorOT4 ###################
# SSA_BBICPorOT4
list<-unique(SSA_BBICPorOT4$HaulID)
Baseline_SSA_BBICPorOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_BBICPorOT4, "BiologicalInfo_BBICPorOT4_SSASMP_ Baseline_V2.csv")
################# 15. WAScoOT3 #################
# SSA_WAScoOT3
list<-unique(SSA_WAScoOT3$HaulID)
Baseline_SSA_WAScoOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_WAScoOT3, "BiologicalInfo_WAScoOT3_SSASMP_ Baseline_V2.csv")
####################16 BBIC(n)SpaOT4 ####################
# SSA_BBICnSpaOT4
list<-unique(SSA_BBICnSpaOT4$HaulID)

Baseline_SSA_BBICnSpaOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_BBICnSpaOT4, "BiologicalInfo_BBICnSpaOT4_SSASMP_ Baseline_V2.csv")
####################17 BBIC(s)SpaOT1 ####################
# SSA_BBICsSpaOT1
list<-unique(SSA_BBICsSpaOT1$HaulID)
Baseline_SSA_BBICsSpaOT1<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_BBICsSpaOT1, "BiologicalInfo_BBICsSpaOT1_SSASMP_ Baseline_V2.csv")
####################18 BBIC(s)SpaOT4 ####################
# SSA_BBICsSpaOT4
list<-unique(SSA_BBICsSpaOT4$HaulID)
Baseline_SSA_BBICsSpaOT4<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_BBICsSpaOT4, "BiologicalInfo_BBICsSpaOT4_SSASMP_ Baseline_V2.csv")
###############19 WASpaOT3 ###############
# SSA_WASpaOT3
list<-unique(SSA_WASpaOT3$HaulID)
Baseline_SSA_WASpaOT3<-subset(Baseline_with_correct_start_date, HaulID%in%list,
                               select=c(HaulID, SciName, FishLength_cm,Number, 
                                        DensAbund_N_Sqkm,RecordStatus))
write.csv(Baseline_SSA_WASpaOT3, "BiologicalInfo_WASpaOT3_SSASMP_ Baseline_V2.csv")
