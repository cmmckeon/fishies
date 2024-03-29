###################
# Version Control #
###################
# R version 3.2.2 (2015-08-14): Fire Safety 
# platform       x86_64-w64-mingw32 (64-bit)
###############
# Script Info #
###############
# Script 12
# Calculate LFI as a data diagnostic
# AUTHOR: Meadhbh Moriarty, 2016
# REVIEWED BY: 
#############################################################
# Get Densities per stat rec per year for the SSA data sets #
#############################################################
hauls_per_stat_rec<-ddply(SSA_haul_dat, c("Survey_Acronym", "YearShot", "ICESStSq"),
                          summarise, Number_of_hauls=length(unique(HaulID))) 
statsq_per_year<-ddply(SSA_haul_dat, c("Survey_Acronym", "YearShot"),
                       summarise, Number_of_rect=length(unique(ICESStSq))) 
# These two look up tables will allow me to calculate the densities
# next organise densities
merger<-subset(SSA_haul_dat, select=c("HaulID", "ICESStSq"))
SSA_kNN_Biological1<-merge(SSA_kNN_Biological, merger, by=("HaulID"))
names(SSA_kNN_Biological1)
summary(SSA_kNN_Biological1$DensAbund_N_Sqkm)

densities<-ddply(SSA_kNN_Biological1, c("Year","Survey_Acronym","SpeciesSciName", "ICESStSq"),
                 summarise, density=sum(DensAbund_N_Sqkm),
                 biomass=sum(DensBiom_kg_Sqkm))
names(densities)
names(hauls_per_stat_rec)
hauls_per_stat_rec$Year<-hauls_per_stat_rec$YearShot
hauls_per_stat_rec$YearShot<-NULL
densities_mean<-merge(densities,hauls_per_stat_rec, by=c("Year","Survey_Acronym", "ICESStSq"))
densities_mean$mean_den<-densities_mean$density/densities_mean$Number_of_hauls
densities_mean$mean_bio<-densities_mean$biomass/densities_mean$Number_of_hauls
plot(densities_mean$Year, densities_mean$mean_den, pch=19, col=cols[as.factor(densities_mean$Survey_Acronym)])

densities_no_spp<-ddply(SSA_kNN_Biological1, c("Year","Survey_Acronym","ICESStSq"),
                        summarise, density=sum(DensAbund_N_Sqkm),
                        biomass=sum(DensBiom_kg_Sqkm))
names(densities_no_spp)
names(hauls_per_stat_rec)

densities_no_spp1<-merge(densities_no_spp,hauls_per_stat_rec, by=c("Year","Survey_Acronym", "ICESStSq"))
densities_no_spp1$mean_den<-densities_no_spp1$density/densities_no_spp1$Number_of_hauls
densities_no_spp1$mean_bio<-densities_no_spp1$biomass/densities_no_spp1$Number_of_hauls
plot(densities_no_spp1$Year, densities_no_spp1$mean_den, pch=19, col=cols[as.factor(densities_no_spp1$Survey_Acronym)])

densities_no_spp_regional<-ddply(SSA_kNN_Biological1, c("Year","Survey_Acronym"),
                                 summarise, density=sum(DensAbund_N_Sqkm),
                                 biomass=sum(DensBiom_kg_Sqkm))
statsq_per_year$Year<-statsq_per_year$YearShot
statsq_per_year$YearShot<-NULL
densities_no_spp_regional1<-merge(densities_no_spp_regional,statsq_per_year, by=c("Year","Survey_Acronym"))
densities_no_spp_regional1$mean_den<-densities_no_spp_regional1$density/densities_no_spp_regional1$Number_of_rect
densities_no_spp_regional1$mean_bio<-densities_no_spp_regional1$biomass/densities_no_spp_regional1$Number_of_rect
plot(densities_no_spp_regional1$Year, densities_no_spp_regional1$mean_den, pch=19, col=cols[as.factor(densities_no_spp1$Survey_Acronym)])
# Average_Annual_Rect_Density_per_Spp
write.csv(densities_mean, "AvgAnnualSppRecDensity_AllSurveys_SSASMP_kNN_V2.csv")
#Average_Annual_Rect_Density
write.csv(densities_no_spp1, "AvgAnnualRecDensity_AllSurveys_SSASMP_kNN_V2.csv")
#Average_Annual_Regional_Density
write.csv(densities_no_spp_regional1, "AvgAnnualRegionalDensity_AllSurveys_SSASMP_kNN_V2.csv")

# Average Annual Density of spp at length 
names(SSA_kNN_Biological1)
densitiesatlng<-ddply(SSA_kNN_Biological1, c("Year","Survey_Acronym","ICESStSq","SpeciesSciName","FishLength_cm"),
                 summarise, density=sum(DensAbund_N_Sqkm),
                 biomass=sum(DensBiom_kg_Sqkm))
names(densities)
names(hauls_per_stat_rec)
densities_mean<-merge(densitiesatlng,hauls_per_stat_rec, by=c("Year","Survey_Acronym", "ICESStSq"))
densities_mean$mean_den<-densities_mean$density/densities_mean$Number_of_hauls
densities_mean$mean_bio<-densities_mean$biomass/densities_mean$Number_of_hauls
plot(densities_mean$Year, densities_mean$mean_den, pch=19, col=cols[as.factor(densities_mean$Survey_Acronym)])
write.csv(densities_mean, "AvgAnnualSppAtLngRecDensity_AllSurveys_SSASMP_kNN_V2.csv")
#for every year/region calculate the sum of densities across all lengths
Btot<-ddply(densities_mean, c("Year","Survey_Acronym"),
                            summarise, Total_biomass=sum(mean_bio))
                        
#for every year/region calculate the sum of densities across all lengths>40
Bigfish<-subset(densities_mean, FishLength_cm>40,)
Blarge<- ddply(Bigfish, c("Year","Survey_Acronym"),
               summarise, biomass_large=sum(mean_bio))
LFI<-merge(Btot, Blarge, by=c("Year","Survey_Acronym"))
#########################
# Quick LFI Calculation##
#########################
LFI$Ratio<-LFI$biomass_large/LFI$Total_biomass
summary(LFI$Survey_Acronym)
plot(LFI$Year[LFI$Survey_Acronym=="GNSIntOT1"], 
     LFI$Ratio[LFI$Survey_Acronym=="GNSIntOT1"], pch=19, lty=1)
write.csv(LFI, "LFI_ratios_AllSurveys_SSASMP_kNN_V2.csv")
############################
# Annual Regional Average ##
############################
names(SSA_kNN_Biological1)
densitiesatlng<-ddply(SSA_kNN_Biological1, c("Year","Survey_Acronym","ICESStSq","SpeciesSciName","FishLength_cm"),
                      summarise, density=sum(DensAbund_N_Sqkm),
                      biomass=sum(DensBiom_kg_Sqkm))
names(densities)
names(hauls_per_stat_rec)
densities_mean<-merge(densitiesatlng,hauls_per_stat_rec, by=c("Year","Survey_Acronym", "ICESStSq"))
densities_mean$mean_den<-densities_mean$density/densities_mean$Number_of_hauls
densities_mean$mean_bio<-densities_mean$biomass/densities_mean$Number_of_hauls
plot(densities_mean$Year, densities_mean$mean_den, pch=19, col=cols[as.factor(densities_mean$Survey_Acronym)])
write.csv(densities_mean, "AvgAnnualSppAtLngRecDensity_AllSurveys_SSASMP_kNN_V2.csv")
############################################
# LFI Diagnostic plots (40cm) in all areas #
############################################
names(LFI)
for (cat in unique(LFI$Survey_Acronym)){
  mypath <- file.path(paste("LFI_40cm_Regional_Diagnostic", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  par(mfrow=c(3,1),oma = c(0, 0, 2, 0))
  d <- subset(LFI, Survey_Acronym == cat)
  plot(d$Year, d$Ratio, 
       main=unique(d$cat), pch=19, xlab="Year", 
       ylab="LFI Index",cex.lab=1.5, type="o")
  mtext(cat, outer = TRUE, cex = 1.5)
  mtext("LFI (40cm)", outer = F, cex = 1.5)
  plot(d$Year, d$biomass_large, pch=19, xlab="Year", 
       ylab="Biomass (Kg/Km2)", cex.lab=1.5, type="o")
  mtext("Large Fish Biomass", outer = F, cex = 1.5)
  x<-c(13:66)
  plot(d$Year, (d$Total_biomass-d$biomass_large), pch=19, xlab="Year", 
       ylab="Biomass (Kg/Km2)", cex.lab=1.5, type="o")
  mtext("Small Fish Biomass", outer = F, cex = 1.5)
  dev.off()
}
