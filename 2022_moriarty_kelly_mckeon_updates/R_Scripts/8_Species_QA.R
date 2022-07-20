# title: "8_Species_QA.R"
# last updated: "08/04/2022"

# This is Script 8 of 13 
# We now correct the biological data for each of the surveys

# PURPOSE:
# 1. Select relevant hauls within standard survey protocol
# 2. Select only fish in 
#      Phylum Chordata  
#       Classes:  Actinopteri
#                  Elasmobranchii 
#                  Holocephali 
#                  Myxini 
#                  Petromyzonti
# 3. Correct known errors in the biological files
# 4. Make global changes to the data, eg standardise length codes used.
# 5. Prepare BASELINE DATA (All steps except k-NN)

# load("./script7_output.rda")

#boost memory for species stuff

## tidy up ------------
# rm("blues", "canSpeed", "canSpeedShip", "cat", "check",
#   "check__dist_haversine_match", "check_dist_speed_match", "check_lat_speed_match",
#   "check_no_match", "check_speed", "check_within_bounds", "checkhaul",
#   "coeff", "coeffs", "cols", "contour_1000m",
#   "contour_200m", "corrhaul_rot", "CV", "d", "data", "dist",
#   "europe", "evhoe", "find", "greys", "gs", "gs_model1", "gs_model2",
#   "haul_dat", "hauls", "hauls1", "HH", "HL",  "i", "ices",
#   "ire", "lat", "legend.text", "list", "list1", "m", "model1_ds",
#   "model2", "mypath",  "nb", "needSpeed",
#   "needSpeedGear", "needSpeedShip", "new.x", "ni", "nm1", "NOAA_Depth",
#   "NWW_boundary", "ospar", "outside_bounds", "p1", "papoue", "pred",
#   "pred_gear", "pred_ship", "predict1", "predict2", "radius", "rand_vect",
#   "rock", "save", "sco1", "sco3",
#   "setSeed", "ships", "speed_none", "sweepcatsummary", "sweepsummary",
#   "the_gov", "train", "withSpeedShip", "ws_dat", "ws_model1", "wx",
#   "x", "y")

library("dplyr")

hauls <- readRDS("clean_HH.rds")
MyFishList <- read.csv("biodiversity/HL_tax_fish_species.csv")
MyFishBaseLengthWeight <- read.csv("biodiversity/MyFishBaseLengthWeight.csv")

hauls <- hauls[hauls$Survey != "DYFS",]
MyFishList <- MyFishList[MyFishList$NewUniqueID2 %in% hauls$NewUniqueID2,]


#tidy up

### Preparation of fish data -------------

# Initial Screening ----------------

# Step 1: Resolving Species unique ID is in a seperate code - requires different version of R
# file of preprepared fish life history info and taxonomic resolution data
traits<-read.csv("./biodiversity/Species_List_Final_2017.csv") ## File of fish traits compiled by Moriarty for MSFD data product 2017. 
# This file contains information about the Classes of fish that we are interested in

# Combine relevant data 

# merge haul and biodiversity data by "NewUniqueID2"
dat <- merge(MyFishList, hauls, by="NewUniqueID2")

## remove
dat <- dat[dat$BySpecRecCode %in% c("1", "2", "6"),]

# Check that all hauls are retained?
checkhaul<-unique(hauls$NewUniqueID2)
checkdat2<-unique(dat$NewUniqueID2)

# this gives 45919 hauls - same as HH hauls file
setdiff(checkdat2, checkhaul)
list<-setdiff(checkhaul, checkdat2)

# 85 hauls contained no fish species - do not remove for now. 
# could be real hauls with no fish, but with other non-chordata species present? 
#hauls1 <-subset(hauls, !NewUniqueID2%in%list,)
hauls1 <- hauls
rm(hauls)

# write a table to save the all the information
#saveRDS(hauls1, "clean_hh_fish_only.rds")
#write.csv(dat, "biodiversity/all_hh_hl_fish_data.csv") 

# remove any SpecVal==0, these P/A values are rubbish - connect to the CA records
find <- subset(dat, SpecVal==0, )  ## 1366 obs

dat <- droplevels(dat[dat$SpecVal != 0,])

# select columns of interest and set up col type
keepers<-c("AphiaID","NewUniqueID2", "Survey.x","Quarter.x","Country.x",
           "Ship.x","Gear.x","StNo.x", "HaulNo.x", "Year.x","SpecCodeType",
           "SpecCode","SpecVal","Sex","TotalNo","CatIdentifier","NoMeas",  
           "SubWgt","CatCatchWgt","LngtCode","LngtClass", "HLNoAtLngt", "HaulDur",
           "DayNight","ShootLat","ShootLong","StatRec","HaulVal","StdSpecRecCode",
           "BySpecRecCode", "DataType", "Month", "DepthNew","SweptArea_wing_m_sqrd",
           "SweptArea_wing_km_sqrd", "valid_name","class",                     
           "order","family","genus","rank","SubFactor", "UniqueIDP.x") 
setdiff(keepers, names(dat))

## going to join these seperately to the smaller dataframe
#c("FRS_Common.Name","LmaxFB","LWRa")

## I THINK that "LmaxFB" == "LengthMax" and "LWRa" == "a" from MyFishBaseLengthWeight......
#dat <- dat

dat <- dat[,which(names(dat) %in% keepers)]

#remove the .x part of the names
dat$Survey <- dat$Survey.x
dat$Quarter <- dat$Quarter.x
dat$Country <- dat$Country.x
dat$Ship <- dat$Ship.x
dat$Gear <- dat$Gear.x
dat$StNo <- dat$StNo.x
dat$HaulNo <- dat$HaulNo.x
dat$Year <- dat$Year.x
dat$UniqueIDP <- dat$UniqueIDP.x

dat <- dat[, which(names(dat) %nin% c("Survey.x", "Quarter.x", "Country.x", "Ship.x", "Gear.x", "StNo.x", 
                                      "HaulNo.x", "Year.x", "UniqueIDP.x"))]
#dat <- dat

### Merge Moriarty's trait data with new fishbase data if need. 

# check if all species needed are in Moriarty version
head(traits)

inHLonly <- setdiff(dat$AphiaID, traits$ValidAphiaID)
inTraits_only <- setdiff(traits$ValidAphiaID, dat$AphiaID)

x <- dat[dat$AphiaID %in% inHLonly,]
intersect(x$valid_name, MyFishBaseLengthWeight$Species)




# x <- MyFishBaseLengthWeight[, which(names(MyFishBaseLengthWeight) %in% c("Species", "LengthMax", "a", "b"))]
# dat <- merge(dat, x, by.x = "valid_name",  by.y = "Species")

# Taxonomic ID protocol -----------------

# How many codes are species, genus, family
summary(as.factor(dat$rank))
# Class     Family      Genus      Order    Species  Subfamily   Suborder  Subphylum Subspecies 
# 2103      15351      67982         36    5847952         62        133        155       7052 


# M.M:What records have genus names when there is only one possible solution to species names

source("R-scripts/8a_source_taxomony_range_adjustments.R")
gc()

# Length Codes Corrections -----------------

#  Standardise DATRAS Codes
# DATRAS allows multiple formats for length (why would they do this.....?)
# There are codes that allow us to distingush the type of data recorded
# All data will be brought to a single code for each type of data
#dat <- dat
dat<-as.data.table(dat)
gc()
# Further to this some species are 
# lets get all the length codes to the same currency - 1cm class
head(dat$LngtClass)

summary(as.numeric(dat$LngtClass))
summary(as.factor(dat$LngtCode))
dat$LngtClass<-as.numeric(dat$LngtClass)
dat[LngtCode=="."|LngtCode=="0",
    c("newLngtCode", "newLngtClass") :=
      list("cm", LngtClass/10)]
dat[LngtCode=="1",
    c("newLngtCode", "newLngtClass") :=
      list("cm", LngtClass)]
dat[LngtCode=="5",
    c("newLngtCode", "newLngtClass") :=
      list("cm", (LngtClass+(LngtClass+4))/2)]
summary(dat$newLngtClass)
dat$newLngtClass[is.na(dat$LngtClass)]<-0
gc()
# Abundance Codes Corrections ----------------

# Deal with C data - this needs to be brought back to R
# when DataType = C  HLNoAtLngt/60 * HaulDur
# when DataType = C  TotalNo/60 * HaulDur 
# Adds 3 new Cols
# NewDataType= R ,  NewTotalNo ,  NewHLNoAtLngt
summary(as.factor(dat$DataType))
dat$NewDataType[dat$DataType=="R"]<-"R"
dat$NewDataType[dat$DataType=="P"]<-"R"  ###  Datatype P pseudo subsampling treat as R

dat$NewDataType[dat$DataType=="C"]<-"R"
#dat$NewDataType[dat$DataType=="S"]<-"RS"
summary(as.factor(dat$NewDataType))
dat$TotalNo_N<-as.numeric(dat$TotalNo)
dat$HaulDur<-as.numeric(dat$HaulDur)


summary(as.numeric(dat$TotalNo))
summary(dat$SubFactor)
summary(as.factor(find$SubFactor))
# check these are all whole numbers 
#so 1 is not equal to Inf i'll just remove that also the HL Noat Lenght is 1 
# the subfactor is inf - that should be 1
dat$TotalNo[dat$NewUniqueID2=="NIGFS_2004_4_74LG_62_ROT_116_NI"
            &dat$valid_name=="Gobiidae"]<-1
dat$SubFactor[dat$NewUniqueID2=="NIGFS_2004_4_74LG_62_ROT_116_NI"
              &dat$valid_name=="Gobiidae"]<-1
dat$HLNoAtLngt_N<-as.numeric(dat$HLNoAtLngt)
gc()
summary(dat$HLNoAtLngt_N)
# check
find<- subset(dat,dat$HLNoAtLngt_N>dat$TotalNo )
summary(find$HLNoAtLngt_N)
summary(find$TotalNo)
# strange things going on with some of the HLNoatLng- really big numbers should be -9/NA
dat$NoMeas[dat$NoMeas=="-9"]<-NA
dat$NoMeas[dat$NoMeas=="0"]<-NA
dat$HLNoAtLngt[dat$TotalNo=="-9"]<-"-9"
dat$HLNoAtLngt[dat$TotalNo=="0"]<-"0"
dat$HLNoAtLngt[dat$HLNoAtLngt=="-9"]<-"0"
dat$TotalNo[dat$TotalNo=="-9"]<-"0"
dat$TotalNo[is.na(dat$TotalNo)]<-"0"
dat$HLNoAtLngt[is.na(dat$HLNoAtLngt)]<-"0"
summary(as.numeric(dat$HLNoAtLngt))
summary(as.numeric(dat$TotalNo))
#dat$HLNoAtLngt<-as.numeric(dat$HLNoAtLngt)
summary(as.numeric(dat$TotalNo))
summary(as.numeric(dat$HLNoAtLngt))

dat$TotalNo_N<-as.numeric(dat$TotalNo)
summary(dat$HLNoAtLngt_N)
dat$HLNoAtLngt_N[(dat$HLNoAtLngt_N<0)]<-0
dat$NewTotalNo[dat$DataType=="R"]<-dat$TotalNo_N[dat$DataType=="R"]
#dat$NewTotalNo[dat$DataType=="S"]<-dat$TotalNo_N[dat$DataType=="S"]
dat$NewTotalNo[dat$DataType=="C"]<-(dat$TotalNo_N[dat$DataType=="C"])/60*dat$HaulDur[dat$DataType=="C"]
dat$NewHLNoAtLngt[dat$DataType=="R"]<-dat$HLNoAtLngt_N[dat$DataType=="R"]
dat$NewTotalNo[dat$DataType=="P"]<-dat$TotalNo_N[dat$DataType=="P"]
dat$NewHLNoAtLngt[dat$DataType=="P"]<-dat$HLNoAtLngt_N[dat$DataType=="P"]
dat$SubFactor<-as.numeric(dat$SubFactor)
summary(dat$SubFactor)

find <- dat[which(is.na(dat$SubFactor)),]

dat$SubFactor[is.na(dat$SubFactor) & dat$DataType == "R"] <- 1

find <- dat[dat$SubFactor <1,]

dat$SubFactor[dat$SubFactor <1 & dat$DataType == "R"] <- 1


#dat$NewHLNoAtLngt[dat$DataType=="S"]<-dat$HLNoAtLngt_N[dat$DataType=="S"]*dat$SubFactor[dat$DataType=="S"]
dat$NewHLNoAtLngt[dat$DataType=="R"]<-dat$HLNoAtLngt_N[dat$DataType=="R"]*dat$SubFactor[dat$DataType=="R"]
dat$NewHLNoAtLngt[dat$DataType=="P"]<-dat$HLNoAtLngt_N[dat$DataType=="P"]*dat$SubFactor[dat$DataType=="P"]
dat$NewHLNoAtLngt[dat$DataType=="C"]<-(dat$HLNoAtLngt_N[dat$DataType=="C"])/60*dat$HaulDur[dat$DataType=="C"]
gc()
summary(as.numeric(dat$NewTotalNo))
dat$NewHLNoAtLngt[is.na(dat$HLNoAtLngt_N)]<-0
summary(as.numeric(dat$NewHLNoAtLngt))
summary(dat$NewTotalNo)
#dat$NoMeas<-as.numeric(dat$NoMeas)
summary(dat$NoMeas)
# Lets take a closer look at S cat.
names(dat)
summary(as.numeric(dat$SubFactor[dat$DataType=="P"]))
summary(as.numeric(dat$NewHLNoAtLngt))
# if subfactor is 1` then S should really be R!
dat$NewDataType[dat$DataType=="P"&dat$SubFactor==1]<-"R"
summary(as.factor(dat$NewDataType))
# R         NAs 
# 5885759   55066 

summary((dat$NewTotalNo))
dat$NewTotalNo[is.na(dat$NewTotalNo)]<-0
# 6524 say they have a subfactor but in reality there are more Subfactors 
# in R
dat$HaulDur<-as.numeric(dat$HaulDur)
dat$SubFactor<-as.numeric(dat$SubFactor)
#subfactor_size <- subset(dat, NewDataType=="RS", ) ## no obs
#summary(subfactor_size$SubFactor)
#subfactor_size$Actual_Haul_Duration <- (subfactor_size$HaulDur/subfactor_size$SubFactor)  
#summary(subfactor_size$Actual_Haul_Duration)
#length((subfactor_size$Actual_Haul_Duration[subfactor_size$Actual_Haul_Duration>12]))
#summary(subfactor_size$HaulDur)

# So the Total number is already Raised no need to multiply the Subfactor 
# but the HL_no_at_lenght isnt raised.
# based on the fact that some of the haul times of the subsampled 
# haul is only a fraction of the actual haul time, we must consider that some of 
# these hauls may be missing out on the min haul criteria and should be removed
h <- hauls1 ## hauls data for which there are fish species
rm(hauls1)
gc()
# smallhauls<- subset(h, HaulDur<12 , ) ## i think we've already removed these hauls
# summary(dat$HaulDur)

summary(dat$SubFactor)
dat$Est_HaulDur <- dat$HaulDur  
# dat$Est_HaulDur[dat$NewDataType=="RS"]<-(dat$HaulDur[dat$NewDataType=="RS"]/dat$SubFactor[dat$NewDataType=="RS"])

summary(dat$Est_HaulDur)
# Add a flag to the Subsampled hauls that may be too short
dat$Est_HaulDur_flag[dat$Est_HaulDur>12]<-"no_issue"
dat$Est_HaulDur_flag[dat$Est_HaulDur<13]<-"subsample_too_small_for_biodiversity_sample"
summary(as.factor(dat$Est_HaulDur_flag))
summary(as.factor(dat$SpecVal))
dat$New_Spec_Val[dat$SpecVal=="1"]<-1
dat$New_Spec_Val[dat$SpecVal=="V"]<-1
Sub_SampledHauls<-subset(dat, dat$Est_HaulDur_flag=="subsample_too_small_for_biodiversity_sample",)
list<-Sub_SampledHauls$NewUniqueID2
summary(as.factor(Sub_SampledHauls$Survey))
unique(Sub_SampledHauls$NewUniqueID2)
list1<-unique(list)
summary(as.factor(dat$Est_HaulDur_flag))

# no samples affected

# now use the new numbers at lenght to get densities
#dat$HLNoAtLngt<-as.numeric(dat$HLNoAtLngt)
dat$NewHLNoAtLngt_round<-round(dat$NewHLNoAtLngt)
dat$NewTotalNo_roundup<-ceiling(dat$NewTotalNo)
#dat$SweptArea_wing_km_sqrd<-as.numeric(dat$SweptArea_wing_km_sqrd)
dat[!is.na(NewHLNoAtLngt), c("HLNoAtLngtkm2") :=
      list(NewHLNoAtLngt/SweptArea_wing_km_sqrd)] 
dat[!is.na(NewTotalNo), c("TotalNoKm2"):=
      list(NewTotalNo/SweptArea_wing_km_sqrd)]
summary(dat$TotalNoKm2)
summary(dat$HLNoAtLngtkm2)
#dat[!is.na(HLNoAtLngtkm2), c("HLNoATLngt_Derived"):=
#      list(HLNoAtLngtkm2*SweptArea_wing_km_sqrd)]
#dat[!is.na(TotalNoKm2), c("TotalNoKm2_Derived"):=
#      list(TotalNoKm2*SweptArea_wing_km_sqrd)]
#find<-subset(dat, dat$NewHLNoAtLngt_round> dat$NewTotalNo_roundup,)
dat$NewHLNoAtLngt_round<-NULL
dat$NewTotalNo_roundup<-NULL
#Still some differences between total number and highter no at length measured
# what a total mess
#dat <- dat
dat <- as.data.frame(dat)
gc()
dat$Density_Km2<-dat$HLNoAtLngtkm2
#dat$Density_Km2[which(!is.na(dat$HLNoAtLngtkm2==0))] <- dat$TotalNoKm2[which(!is.na(dat$HLNoAtLngtkm2==0))]
#x <- dat[which(is.na(dat$NewHLNoAtLngt)),]
summary(dat$Density_Km2)
find<-subset(dat, Density_Km2==0, )
# I will address theses 62 fish later on in code


# Insure Length is TL ------------------------------------

source("R-Scripts/8b_source_insure_length_is_total_length.R")


# List species records which exceed their maximum reported length
names(dat)

x <- setdiff(dat$estsciname, traits$valid_name)
y <- setdiff(dat$estsciname, MyFishBaseLengthWeight$Species)
d <- dat[dat$valid_name %in% x,]
x <- setdiff(d$valid_name, MyFishBaseLengthWeight$Species) 
no_length_validation <- dat[dat$valid_name %in% x,] ## no length validation data

speciesx <- select(dat, valid_name, estsciname, AphiaID)

speciesx <- unique(speciesx)
#write.csv(speciesx, "Species_list_post_cleaning_forfishbase_24_05_2021.csv")

## subsetting to exclude chordata that are not really fish (tunicates and gulls)

dat <- dat[dat$estsciname %nin% c("Laridae", "Aplidium", "Ascidia", "Ascidia conchilega", "Ascidia mentula", 
                                "Ascidia prunum", "Ascidia virginea", "Ascidiacea", "Ascidiella", 
                                "Ascidiella aspersa", "Ascidiella scabra", "Ascidiidae",  "Botrylloides leachii",
                                "Botrylloides leachii", "Botryllus", "Botryllus schlosseri",
                                "Ciona intestinalis","Corella parallelogramma", "Dendrodoa grossularia",
                                "Diazona violacea", "Distomus variolosus", "Molgula", "Molgula manhattensis", 
                                "Molgula occulta", "Molgulidae", "Pelonaia corrugata", "Phlebobranchia",
                                "Polycarpa pomaria", "Polyclinidae", "Polyclinum aurantium", 
                                "Pyrosoma", "Pyuridae","Salpidae", "Salpinae", "Soestia zonaria",
                                "Styela canopus", "Styela clava",  "Styelidae",  "Synoicum", "Thetys vagina", 
                                "Tunicata"),]

### Exclude - tunicates and a gull genus

## start
# Check LMax - step 1 -------------------

## add in Meadhbh's trait data and a few rows from rfishbase for length checking
x <- setdiff(dat$estsciname, traits$valid_name)
length(unique(MyFishBaseLengthWeight$Species[MyFishBaseLengthWeight$Species %in% x]))
traitsLen <- traits[,which(names(traits) %in% c("valid_name", "LmaxFB", "LWRa", "LWRb"))]

x <- unique(MyFishBaseLengthWeight[, which(names(MyFishBaseLengthWeight) %in% 
                                             c("Species", "LengthMax",  "a",  "b", "Method"))])

missed <- setdiff(dat$estsciname, traits$valid_name)
fishbase_fillers <- x[x$Species %in% missed,]

fishbase_fillers <- subset(fishbase_fillers, Method == "type I linear regression")

fishbase_fillers <- fishbase_fillers %>%
  group_by(Species) %>%
  summarise(LmaxFB = max(LengthMax, na.rm = T),
            LWRa = mean(a, na.rm = T),
            LWRb = mean(b, na.rm = T))

fishbase_fillers$LmaxFB[fishbase_fillers$LmaxFB == "-Inf"] <- NA
names(fishbase_fillers) <- c("valid_name", "LmaxFB",  "LWRa" ,   "LWRb") 
traitsLen <- rbind(traitsLen, fishbase_fillers)

dat <- merge(dat, traitsLen, by.x = "estsciname", by.y = "valid_name", all.x = T)
rm(fishbase_fillers,new,missed,x)


# Read in fish Max data
plot(dat$LmaxFB*1.1, dat$newLngtClass, pch=19, xlim=c(1,500), 
     ylim=c(1,500), xlab="Species LMax + 10%",
    ylab="Species Recorded Length",
    #col=cols[dat$valid_name], 
    main="Species Length Classes")
abline(0, 1, col="red")
#plot(dat$MaxLngt, dat$newLngtClass, pch=19, xlim=c(1,75)
#     , ylim=c(1,75), xlab="Species LMax + 10%",
#     ylab="Species Recorded Length",
#     col=cols[dat$scientificname], main="GOV Species Length Classes")
#abline(0, 1, col="red")
# write a piece of code to capture all records that exceed lmax
summary(dat$FishLength_cm)
summary(dat$LmaxFB)
test1 <- dat[which(dat$FishLength_cm > dat$LmaxFB*1.4),]
unique(test1$valid_name[test1$family=="Macrouridae"])
unique(test1$Year[test1$family=="Macrouridae"])
unique(test1$Country[test1$family=="Macrouridae"])
# some of the data providers have provided answers on the L max questions
# some are genuine mistakes with corrections available 
# fix using uniqueID, spp Valid Aphia and Lenght class - this could 
# take a while to code
#length change 1. Buglossidium luteum to Solea solea
names(dat)
#length change 3.Buglossidium luteum to Microchirusvariegatus
dat$estAphia_Code[dat$UniqueIDP =="SWC-IBTS_1995_4_SCO2_20_GOV"&
                         dat$AphiaID==127153&
                         dat$newLngtClass==22]<-274304  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1995_4_SCO2_20_GOV"&
                  dat$AphiaID==127153&
                  dat$newLngtClass==22]<-"Microchirus variegatus"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1995_4_SCO2_20_GOV"&
                 dat$AphiaID==127153&dat$newLngtClass==22]<-"Species_changed(DP)"
#length change 4.Buglossidium luteum to Microchirusvariegatus
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                         dat$AphiaID==127153&
                         dat$newLngtClass==22]<-274304  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                  dat$AphiaID==127153&
                  dat$newLngtClass==22]<-"Microchirus variegatus"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                 dat$AphiaID==127153&dat$newLngtClass==22]<-"Species_changed(DP)"
#length change 5.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 6.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==30]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==30]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==30]<-"Species_changed(DP)"
#length change 7.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_65_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_65_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_65_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 8.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 9.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==27]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==27]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_66_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==27]<-"Species_changed(DP)"
#length change 10.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_10_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_10_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_10_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 11.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_38_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_38_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_38_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 12.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==27]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==27]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==27]<-"Species_changed(DP)"
#dat <- dat
#length change 13.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_53_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_53_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_SCO2_53_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 14.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 15.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 16.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==30]<-126792  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==30]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==30]<-"Species_changed(DP)"
#length change 17.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1998_1_SCO3_55_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==29]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1998_1_SCO3_55_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==29]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1998_1_SCO3_55_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==29]<-"Species_changed(DP)"
#length change 18.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_49_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_49_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_49_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 19.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 20.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2011_3_SCO3_34_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2011_3_SCO3_34_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2011_3_SCO3_34_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 21.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                         dat$AphiaID==150630&
                         dat$newLngtClass==23]<-127082  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                  dat$AphiaID==150630&
                  dat$newLngtClass==23]<-"Trachinus draco"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                 dat$AphiaID==150630&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 22.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                         dat$AphiaID==150630&
                         dat$newLngtClass==24]<-127082  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                  dat$AphiaID==150630&
                  dat$newLngtClass==24]<-"Trachinus draco"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                 dat$AphiaID==150630&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 23.Gymnammodytes semisquamatus
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2006_4_SCO3_14_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==145]<-14.5 
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2006_4_SCO3_14_GOV"&
                 dat$AphiaID==126754&dat$LngtClass==145]<-"Length_changed(DP)"
#length change 24.Gymnammodytes semisquamatus
list<-c('SWC-IBTS_2006_4_SCO3_14_GOV','NS-IBTS_2007_3_SCO3_23_GOV',
        'NS-IBTS_2007_3_SCO3_84_GOV','SWC-IBTS_2008_1_SCO3_38_GOV',
        'NS-IBTS_2008_3_SCO3_62_GOV','SWC-IBTS_2008_4_SCO3_3_GOV',
        'SWC-IBTS_2009_1_SCO3_50_GOV','NS-IBTS_2009_3_SCO3_38_GOV',
        'NS-IBTS_2009_3_SCO3_41_GOV','NS-IBTS_2009_3_SCO3_65_GOV',
        'SWC-IBTS_2009_4_SCO3_3_GOV','SWC-IBTS_2009_4_SCO3_4_GOV',
        'SWC-IBTS_2009_4_SCO3_56_GOV','NS-IBTS_2010_1_SCO3_36_GOV',
        'NS-IBTS_2010_1_SCO3_54_GOV','SWC-IBTS_2010_1_SCO3_32_GOV',
        'SWC-IBTS_2010_1_SCO3_33_GOV')
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==126754]<-"Length_changed(DP)"

dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2006_4_SCO3_14_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==145]<-14.5 

dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-17 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-18
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-18.5 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-19
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-19.5 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-20 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-20.5 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-21
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-22
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==150]<-15
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==151]<-15.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==152]<-16
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==153]<-16.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==154]<-17
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==155]<-17.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==156]<-18
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==157]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==158]<-19
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==159]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==160]<-20
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==161]<-20.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==162]<-21
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==164]<-22
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-22.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_1_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-18
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==160]<-16
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==161]<-16.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==162]<-17
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==163]<-17.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==164]<-18
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==166]<-19
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-17
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==171]<-17.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-18
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-19
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-20
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-20.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-21
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_1_SCO3_50_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==150]<-15
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==140]<-14
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==151]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==152]<-20
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==153]<-20.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==154]<-21
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==155]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-18
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==181]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==182]<-19
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==183]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==184]<-20
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==185]<-20.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==186]<-21
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==187]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==188]<-22
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==189]<-22.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==190]<-23
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==191]<-23.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_65_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==185]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==90]<-9
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==92]<-10
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==93]<-10.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==94]<-11
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==95]<-11.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==96]<-12
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==97]<-12.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==99]<-13.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==101]<-14.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==103]<-15.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==104]<-16
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==105]<-16.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==106]<-17
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==107]<-17.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==108]<-18
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==109]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==110]<-19
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==111]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==112]<-20
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_4_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==120]<-12
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_4_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==134]<-19
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_56_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==255]<-25.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_36_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==215]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_36_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==217]<-22.5
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_2010_1_SCO3_54_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==220]<-22
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==80]<-8
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==83]<-9.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==84]<-10
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==85]<-10.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==86]<-11
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==87]<-11.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==88]<-12
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==89]<-12.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==90]<-13
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==92]<-14
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-16.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==166]<-17
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==167]<-17.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==168]<-18
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==169]<-18.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-19
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==171]<-19.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-20
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-20.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-21
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-21.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-22
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-22.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-23
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-23.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-24
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==182]<-25
#change length of Hyperoplus lanceolatus problem in cruisefile
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==540]<-27
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==690]<-28.5
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==490]<-26.5
#check length of Leucoraja naevus has been reuploaded
check<-dat[dat$Year==1989
            & dat$AphiaID==127139, ]
# reuploaded sucessfully 
#check 54cm Limanda limanda has been removed
check<-dat[dat$UniqueIDP=="NS-IBTS_2015_3_SCO3_223_GOV"
            & dat$AphiaID==127139, ]
# gone  
dat <- dat
#check oversized Pearlside has been removed
check<-dat[dat$UniqueIDP=="SWC-IBTS_1997_1_SCO2_13_GOV"
            & dat$AphiaID==127312, ]
# still there remove or change to Pearlfish???
#length change 21.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_53_GOV"&
                         dat$AphiaID==127147&
                         dat$newLngtClass==22]<-127151 
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_53_GOV"&
                  dat$AphiaID==127147&
                  dat$newLngtClass==22]<-"Zeugopterus punctatus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1991_1_SCO2_53_GOV"&
                 dat$AphiaID==127147&dat$newLngtClass==22]<-"Species_changed(DP)"
#Both Species and length change 21.Raja clavata to D. batis 
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==150]<-105869 
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==150]<-"Dipturus batis"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==198]<-"Species&Lenght_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_1992_1_SCO2_15_GOV"
                  & dat$AphiaID==105883
                  & dat$LngtClass==150]<-198
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==150]<-105869 
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==150]<-"Dipturus batis"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==205]<-"Species&Lenght_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="SWC-IBTS_1994_1_SCO2_27_GOV"
                  & dat$AphiaID==105883
                  & dat$LngtClass==150]<-205
#Species change 21.Raja clavata to D. batis 
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==207]<-105869 
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==207]<-"Dipturus batis"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==207]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==141]<-105869 
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==141]<-"Dipturus batis"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==141]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==128]<-105869 
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==128]<-"Dipturus batis"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==128]<-"Species_changed(DP)"
#Species change 21.Raja montagui to Raja brachyura
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==95]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==95]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==95]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==94]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==94]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==94]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==94]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==94]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==94]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==97]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==97]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==97]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==101]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==101]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==101]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==96]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==96]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==96]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==103]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==103]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==103]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==97]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==97]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==97]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==101]<-367297  
dat$estsciname[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==101]<-"Raja brachyura"
dat$QC_Length[dat$UniqueIDP=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==101]<-"Species_changed(DP)"
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                         dat$AphiaID==127204&
                         dat$newLngtClass==25]<-127203  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                  dat$AphiaID==127204&
                  dat$newLngtClass==25]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==25]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                         dat$AphiaID==127204&
                         dat$newLngtClass==27]<-127203  
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                  dat$AphiaID==127204&
                  dat$newLngtClass==27]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1987_1_SCO2_34_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==27]<-"Species_changed(DP)"
# Norway corrections
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==24]<-154675   
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==24]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==24]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==26]<-154675   
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==26]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==26]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==27]<-154675   
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==27]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==27]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==28]<-154675   
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==28]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==28]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==29]<-154675   
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==29]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==29]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1996_1_MIC_5_GOV"&
                         dat$AphiaID==271564  &
                         dat$newLngtClass==70]<-105870    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1996_1_MIC_5_GOV"&
                  dat$AphiaID==271564  &
                  dat$newLngtClass==70]<-"Dipturus linteus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1996_1_MIC_5_GOV"&
                 dat$AphiaID==271564 &dat$newLngtClass==70]<-"Species_changed(DP)"
# remove record of Lycodes gracilis
dat<-dat[ !(dat$UniqueIDP=="NS-IBTS_2006_3_JHJ_287_GOV"&
                dat$AphiaID==274100   &
                dat$newLngtClass==56),]
# remove record of Sebastes viviparus
dat<-dat[ !(dat$UniqueIDP=="ROCKALL_2014_3_SCO3_311_GOV"&
                dat$AphiaID==127255   &
                dat$newLngtClass==50),]
# EVHOE Length Corrections
# change Species
dat$estAphia_Code[dat$UniqueIDP=="EVHOE_2014_4_THA2_120_GOV"&
                         dat$AphiaID==272278  &
                         dat$newLngtClass==12]<-272728     
dat$estsciname[dat$UniqueIDP=="EVHOE_2014_4_THA2_120_GOV"&
                  dat$AphiaID==272278  &
                  dat$newLngtClass==12]<-"Notoscopelus kroyeri"
dat$QC_Length[dat$UniqueIDP=="EVHOE_2014_4_THA2_120_GOV"&
                 dat$AphiaID==272278 & 
                 dat$newLngtClass==12]<-"Species_changed(DP)"
# Change length 
check<-dat[dat$UniqueIDP=="EVHOE_2002_4_THA2_41_GOV"&
              dat$AphiaID==126976,]
# only 2 that need changing
dat$QC_Length[dat$UniqueIDP=="EVHOE_2002_4_THA2_41_GOV"&
                 dat$AphiaID==126976]<-"Length_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="EVHOE_2002_4_THA2_41_GOV"
                  & dat$AphiaID==126976
                  & dat$LngtClass==93]<-33 
dat$FishLength_cm[dat$UniqueIDP=="EVHOE_2002_4_THA2_41_GOV"
                  & dat$AphiaID==126976
                  & dat$LngtClass==96]<-36 
# Change length 
check<-dat[dat$UniqueIDP=="EVHOE_2013_4_THA2_23_GOV"&
              dat$AphiaID==126413,]
# only 1 that need changing
dat$QC_Length[dat$UniqueIDP=="EVHOE_2013_4_THA2_23_GOV"&
                 dat$AphiaID==126413]<-"Length_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="EVHOE_2013_4_THA2_23_GOV"
                  & dat$AphiaID==126413
                  & dat$LngtClass==260]<-26 

# Change length 
check<-dat[dat$UniqueIDP=="EVHOE_2013_4_THA2_21_GOV"&
              dat$AphiaID==126415,]
# only 1 that need changing
dat$QC_Length[dat$UniqueIDP=="EVHOE_2013_4_THA2_21_GOV"&
                 dat$AphiaID==126415]<-"Length_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="EVHOE_2013_4_THA2_21_GOV"
                  & dat$AphiaID==126415
                  & dat$LngtClass==420]<-42
# England corrections  check
# Probably wrongly swiped on measuring board. 
# Could not delete length from FSS, as this would affect weight.
check<-dat[dat$UniqueIDP=="BTS-VIIa_2007_3_COR_45_BT4A"&
              dat$AphiaID==127126,]
# still in database
#delete 
dat<-dat[ !(dat$UniqueIDP=="BTS-VIIa_2007_3_COR_45_BT4A"&
                dat$AphiaID==127126   &
                dat$newLngtClass==44),]
# Probable typo. Probably SDR (spotted ray) and not SPR. 
# The catch was also sexed. Changed to SDR on FSS
# Check
check<-dat[dat$UniqueIDP=="BTS_2009_4_CAR_55_BT4A" & 
              dat$AphiaID==126425,]
# Ship CAR already removed
check<-dat[dat$UniqueIDP=="BTS_2003_3_COR_99_BT4A" & 
              dat$AphiaID==126445 &
              dat$newLngtClass==88,]
# Probably wrongly swiped on measuring board. 
# Correction captured previously. Deleted from FSS
# needs to be removed from dataset
dat<-dat[!(dat$UniqueIDP=="BTS_2003_3_COR_99_BT4A" & 
               dat$AphiaID==126445 &
               dat$newLngtClass==88),]
check<-dat[dat$UniqueIDP=="BTS_2013_3_END_90_BT4A" & 
              dat$AphiaID==127126 &
              dat$newLngtClass==43,]
dat<-dat[!(dat$UniqueIDP=="BTS_2013_3_END_90_BT4A" & 
               dat$AphiaID==127126 &
               dat$newLngtClass==43),]

check<-dat[dat$UniqueIDP=="BTS_2008_3_END_80_BT4A" & 
              dat$AphiaID==127153 &
              dat$newLngtClass==50,]
dat<-dat[!(dat$UniqueIDP=="BTS_2008_3_END_80_BT4A" & 
               dat$AphiaID==127153 &
               dat$newLngtClass==50),]
check<-dat[dat$UniqueIDP=="BTS_2010_3_END_4_BT4A" & 
              dat$AphiaID==127153,]
dat<-dat[!(dat$UniqueIDP=="BTS_2010_3_END_4_BT4A" & 
               dat$AphiaID==127153 &
               dat$newLngtClass==38),]
# gone
check<-dat[dat$UniqueIDP=="BTS_2002_3_COR_12_BT4A" & 
              dat$AphiaID==274304,]
dat<-dat[!(dat$UniqueIDP=="BTS_2002_3_COR_12_BT4A" & 
               dat$AphiaID==274304 &
               dat$newLngtClass==56),]
# CGFS - errors in lmax
# yves has sent me lots of corrections 
# get these included.
# Change Alosa agone to Alosa fallax
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_53_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==46]<-126415     
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_53_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==46]<-"Alosa fallax"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_53_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==46]<-"Species_changed(DP)"
# Change Alosa agone to Alosa fallax
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_16_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==43]<-126415     
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_16_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==43]<-"Alosa fallax"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_16_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==43]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2009_1_THA2_79_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==43]<-126415     
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2009_1_THA2_79_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==43]<-"Alosa fallax"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2009_1_THA2_79_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==43]<-"Species_changed(DP)"
# change Sprattus sprattus to Clupea Harengus
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1983_1_THA_8_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==18]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1983_1_THA_8_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==18]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1983_1_THA_8_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==18]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==19.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==19.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==19.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==20]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==20]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==20]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==20.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==20.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==20.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==21]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==21]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==21]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==21.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==21.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==21.5]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==22]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==22]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==22]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==22.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==22.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==22.5]<-"Species_changed(DP)"
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==23]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==23]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==23]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==23.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==23.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==23.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==24.5]<-126417    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==24.5]<-"Clupea harengus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==24.5]<-"Species_changed(DP)"
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==21]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==21]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==21]<-"Species_changed(DP)"


dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==23]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==23]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_THA_31_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==23]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1992_1_THA_39_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==30]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1992_1_THA_39_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==30]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1992_1_THA_39_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==30]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1995_1_THA_24_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1995_1_THA_24_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1995_1_THA_24_GOV"&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

check<-test[dat$UniqueIDP=="NS-IBTS_1995_1_THA_24_GOV"&
              dat$AphiaID==127204,]

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1995_1_THA_26_GOV"&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

list<-c('NS-IBTS_1995_1_THA_27_GOV', 'NS-IBTS_1995_1_THA_28_GOV',
        'NS-IBTS_1995_1_THA_35_GOV', 'NS-IBTS_1995_1_THA_36_GOV',
        'NS-IBTS_1995_1_THA_38_GOV', 'NS-IBTS_1995_1_THA_8_GOV',
        'NS-IBTS_1996_1_THA_17_GOV', 'NS-IBTS_1996_1_THA_2_GOV',
        'NS-IBTS_1996_1_THA_24_GOV', 'NS-IBTS_1996_1_THA_25_GOV',
        'NS-IBTS_1996_1_THA_3_GOV', 'NS-IBTS_1996_1_THA_37_GOV',
        'NS-IBTS_1996_1_THA_7_GOV', 'NS-IBTS_1997_1_THA2_13_GOV',
        'NS-IBTS_1997_1_THA2_23_GOV', 'NS-IBTS_1997_1_THA2_24_GOV',
        'NS-IBTS_1997_1_THA2_26_GOV', 'NS-IBTS_1997_1_THA2_31_GOV',
        'NS-IBTS_1997_1_THA2_41_GOV', 'NS-IBTS_1997_1_THA2_53_GOV',
        'NS-IBTS_1997_1_THA2_54_GOV', 'NS-IBTS_1997_1_THA2_59_GOV',
        'NS-IBTS_1998_1_THA2_1_GOV', 'NS-IBTS_1998_1_THA2_30_GOV',
        'NS-IBTS_1998_1_THA2_39_GOV', 'NS-IBTS_1998_1_THA2_40_GOV',
        'NS-IBTS_1998_1_THA2_41_GOV', 'NS-IBTS_1998_1_THA2_42_GOV',
        'NS-IBTS_1998_1_THA2_58_GOV', 'NS-IBTS_1998_1_THA2_59_GOV',
        'NS-IBTS_1998_1_THA2_61_GOV', 'NS-IBTS_1998_1_THA2_71_GOV',
        'NS-IBTS_1998_1_THA2_73_GOV', 'NS-IBTS_1998_1_THA2_82_GOV',
        'NS-IBTS_1998_1_THA2_83_GOV', 'NS-IBTS_1999_1_THA2_2_GOV',
        'NS-IBTS_1999_1_THA2_27_GOV', 'NS-IBTS_1999_1_THA2_30_GOV',
        'NS-IBTS_1999_1_THA2_32_GOV', 'NS-IBTS_1999_1_THA2_34_GOV',
        'NS-IBTS_1999_1_THA2_35_GOV', 'NS-IBTS_1999_1_THA2_5_GOV',
        'NS-IBTS_1999_1_THA2_7_GOV', 'NS-IBTS_1999_1_THA2_8_GOV',
        'NS-IBTS_2000_1_THA2_1_GOV', 'NS-IBTS_2000_1_THA2_2_GOV',
        'NS-IBTS_2000_1_THA2_30_GOV', 'NS-IBTS_2000_1_THA2_31_GOV',
        'NS-IBTS_2000_1_THA2_32_GOV', 'NS-IBTS_2000_1_THA2_41_GOV',
        'NS-IBTS_2000_1_THA2_42_GOV', 'NS-IBTS_2000_1_THA2_43_GOV')
#dat <- dat
dat$estAphia_Code[dat$UniqueIDP%in%list&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$UniqueIDP%in%list&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2004_1_THA2_29_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass>24.9]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2004_1_THA2_29_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass>24.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2004_1_THA2_29_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass>24.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_41_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass>26.9]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_41_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass>26.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2006_1_THA2_41_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass>26.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2014_1_THA2_87_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass==25]<-127203    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2014_1_THA2_87_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass==25]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2014_1_THA2_87_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass==25]<-"Species_changed(DP)"
# Nerophis ophidion - Possible confusion with Entelurus aequoreus
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_42_GOV"&
                         dat$AphiaID==127385 &
                         dat$newLngtClass==38]<-127379    
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_42_GOV"&
                  dat$AphiaID==127385 &
                  dat$newLngtClass==38]<-"Entelurus aequoreus"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2005_1_THA2_42_GOV"&
                 dat$AphiaID==127385&dat$newLngtClass==38]<-"Species_changed(DP)"

list<-c('NS-IBTS_2005_1_THA2_42_GOV', 'NS-IBTS_2005_1_THA2_44_GOV',
        'NS-IBTS_2005_1_THA2_47_GOV', 'NS-IBTS_2005_1_THA2_55_GOV',
        'NS-IBTS_2005_1_THA2_58_GOV', 'NS-IBTS_2005_1_THA2_59_GOV',
        'NS-IBTS_2005_1_THA2_67_GOV', 'NS-IBTS_2005_1_THA2_70_GOV')

dat$estAphia_Code[dat$UniqueIDP%in%list&
                         dat$AphiaID==127385 &
                         dat$newLngtClass>35.9]<-127379    
dat$estsciname[dat$UniqueIDP%in%list&
                  dat$AphiaID==127385 &
                  dat$newLngtClass==35.9]<-"Entelurus aequoreus"
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==127385&dat$newLngtClass==35.9]<-"Species_changed(DP)"
# change Ammodytes tobianus to  Hyperoplus immaculatus
list<-c("NS-IBTS_1998_1_THA2_68_GOV","NS-IBTS_1998_1_THA2_69_GOV",
        'NS-IBTS_1998_1_THA2_71_GOV','NS-IBTS_1998_1_THA2_72_GOV',
        'NS-IBTS_1998_1_THA2_73_GOV','NS-IBTS_1998_1_THA2_75_GOV',
        'NS-IBTS_2001_1_THA2_5_GOV','NS-IBTS_2003_1_THA2_7_GOV',
        'NS-IBTS_2004_1_THA2_40_GOV','NS-IBTS_2006_1_THA2_36_GOV',
        'NS-IBTS_2008_1_THA2_77_GOV','NS-IBTS_2009_1_THA2_13_GOV',
        'NS-IBTS_2009_1_THA2_14_GOV','NS-IBTS_2010_1_THA2_91_GOV',
        'NS-IBTS_2011_1_THA2_83_GOV')

dat$estAphia_Code[dat$UniqueIDP%in%list&
                         dat$AphiaID==126752 &
                         dat$newLngtClass>22.9]<-126755     
dat$estsciname[dat$UniqueIDP%in%list&
                  dat$AphiaID==126752 &
                  dat$newLngtClass>22.9]<-"Hyperoplus immaculatus"
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==126752&dat$newLngtClass>22.9]<-"Species_changed(DP)"
# more Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$UniqueIDP=="FR-CGFS_1997_4_GWD_1_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203
dat$estsciname[dat$UniqueIDP=="FR-CGFS_1997_4_GWD_1_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="FR-CGFS_1997_4_GWD_1_GOV"&
                 dat$AphiaID==127203 & dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$UniqueIDP=="FR-CGFS_2005_4_GWD_11_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203
dat$estsciname[dat$UniqueIDP=="FR-CGFS_2005_4_GWD_11_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$UniqueIDP=="FR-CGFS_2005_4_GWD_11_GOV"&
                 dat$AphiaID==127203 & dat$newLngtClass>19.9]<-"Species_changed(DP)"
# Liparis montagui is probably Liparis liparis 
list<-c('NS-IBTS_1999_1_THA2_3_GOV',  'NS-IBTS_2000_1_THA2_43_GOV',
        'NS-IBTS_2002_1_THA2_68_GOV', 'NS-IBTS_2002_1_THA2_69_GOV')

dat$estAphia_Code[dat$UniqueIDP%in%list&dat$AphiaID==127220 
                       & dat$newLngtClass>13.9]<-293624 
dat$estsciname[dat$UniqueIDP%in%list & dat$AphiaID==127220 
                & dat$newLngtClass>13.9]<-"Liparis liparis liparis"
dat$QC_Length[dat$UniqueIDP%in%list & dat$AphiaID==127220
               & dat$newLngtClass>13.9]<-"Species_changed(DP)"
# Buglossidium luteum is probably Solea solea
dat$estAphia_Code[dat$UniqueIDP=="NS-IBTS_2000_1_THA2_68_GOV"
                       &dat$AphiaID==127153
                       & dat$newLngtClass>19.9]<-127160 
dat$estsciname[dat$UniqueIDP=="NS-IBTS_2000_1_THA2_68_GOV"
                & dat$AphiaID==127153
                & dat$newLngtClass>19.9]<-"Solea solea"
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2000_1_THA2_68_GOV"
               & dat$AphiaID==127153
               & dat$newLngtClass>19.9]<-"Species_changed(DP)"
# Symphodus roissali probably another Symphodus spp 
dat$estAphia_Code[dat$UniqueIDP=="FR-CGFS_2014_4_GWD_6_GOV"
                       &dat$AphiaID==273573
                       & dat$newLngtClass>28.9]<-126023 
dat$estsciname[dat$UniqueIDP=="FR-CGFS_2014_4_GWD_6_GOV"
                & dat$AphiaID==273573
                & dat$newLngtClass>28.9]<-"Symphodus"
dat$QC_Length[dat$UniqueIDP=="FR-CGFS_2014_4_GWD_6_GOV"
               & dat$AphiaID==273573
               & dat$newLngtClass>28.9]<-"Species_changed_to_genus(DP)"
# two lenghts need changing 
dat$FishLength_cm[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_61_GOV"
                  & dat$AphiaID==126415
                  & dat$LngtClass==110]<-11 
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_1999_1_THA2_61_GOV"&
                 dat$AphiaID==126415&dat$LngtClass==110]<-"Length_changed(DP)"
dat$FishLength_cm[dat$UniqueIDP=="FR-CGFS_2006_4_GWD_47_GOV"
                  & dat$AphiaID==127126
                  & dat$LngtClass==150]<-15 
dat$QC_Length[dat$UniqueIDP=="FR-CGFS_2006_4_GWD_47_GOV"&
                 dat$AphiaID==127126&dat$LngtClass==150]<-"Length_changed(DP)"

# Check LMax - step 2 ---------------------

# Fill in rest of data into new Valid name and id and QC length
summary(as.factor(dat$QC_Length))
dat$QC_Length[is.na(dat$QC_Length)]<-"no_change"
# redo max data on new spp names 
# names(traits)
# names(dat)
# traits$estsciname<-traits$valid_name
# dat$LmaxFB<-NULL
# dat$valid_name<-NULL
# dat$valid_authority<-NULL
# dat$kingdom<-NULL
# dat$phylum<-NULL
# dat$order<-NULL
# dat$family<-NULL
# dat$genus<-NULL
# dat$rank<-NULL
# dat$FRS_Common.Name<-NULL
# dat$CLASS<-NULL
# # new data frame
# names(dat)
# dat<-join(dat, traits, by="estsciname")
# nrow(dat)-nrow(dat)
# names(dat)

test1<-dat[which(dat$newLngtClass > dat$LmaxFB*1.1),]
# 1962 observations 

test2<-dat[which(dat$newLngtClass > dat$LmaxFB*1.4),]
# 354 observations
summary(as.factor(test2$Country))
summary(as.factor(test2$estsciname))
summary(as.factor(test2$estrank))

 
Problem_Lenghts<-ddply(test1[!is.na(HLNoAtLngtkm2)],
                       c("Survey", "Year", "estsciname", "rank"),
                       summarise, 
                       totalnopofsamples=length(as.numeric(HLNoAtLngtkm2)))
write.csv(Problem_Lenghts, "Summary_probelm_lengths_10-10-2016.csv")
summarysppnosatLngtperyerandcon<-ddply(dat[!is.na(HLNoAtLngtkm2)],
                                       c("valid_name", "rank", 
                                         "Country", "Year",
                                         "newLngtClass"),
                                       summarise, totalnopofsamples=length(as.numeric(HLNoAtLngtkm2)))
write.csv(summarysppnosatLngtperyerandcon, "Summary_Fish_at_Length.csv")
# take a copy of the lengths
dat$newLngtClass_10<-dat$newLngtClass_10
# next fit all the Coelorinchus caelorhincus lenghts
list<-c('Length_changed(DP)', 'no_change','Species&Lenght_changed(DP)',
        'Species_changed(DP)', 'Species_changed_to_genus(DP)' )
dat$Use_Lenght_cm[dat$QC_Length%in%list] <- dat$FishLength_cm[dat$QC_Length%in%list]
summary(dat$Use_Lenght_cm)
# have we got them all now?
test1 <- dat[ which(dat$Use_Lenght_cm > dat$LmaxFB*1.1),]
# still 2052 
test2<-dat[ which(dat$Use_Lenght_cm > dat$LmaxFB*1.4),]
# 305 are outwith criteria for "big fish" 
summary(as.factor(test1$Survey))
# again spain is causing the major concens here
summary(dat$LmaxFB)
write.csv(test2, "more_lenghts_to_check-10-10-2016.csv")
#lets get these ones sorted out
# more Engraulis encrasicolus needing division
list<-c("NS-IBTS_2000_3_DAN2_31_GOV", "NS-IBTS_2000_3_DAN2_31_GOV", 
        "NS-IBTS_2000_3_DAN2_31_GOV", "NS-IBTS_2000_3_DAN2_31_GOV", 
        "NS-IBTS_2005_3_WAH3_151_GOV")
find<-dat[dat$UniqueIDP%in%list &
             dat$AphiaID==126426]
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==126426]<-"Length/10"
find<-dat[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_73_NCT" &
             dat$AphiaID==126426 & dat$newLngtClass==155]
dat$QC_Length[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_73_NCT" &
                 dat$AphiaID==126426 & dat$newLngtClass==155]<-"Length/10"
find<-subset(dat, dat$UniqueIDP=="NS-IBTS_1995_1_WAH3_1_GOV",)
# Callionymus maculatus oversized records
cm<-subset(dat, AphiaID==126793,)
plot(cm$Use_Lenght_cm, cm$HLNoAtLngtkm2)
# issue with the really big ones about 150 obs. 
list<-c("IE-IGFS_2008_4_CEXP_84_GOV","NS-IBTS_2000_1_ARG_27_GOV",
        "EVHOE_2003_4_THA2_134_GOV","NIGFS_1998_3_LF_33_ROT",	
        "NS-IBTS_1995_1_WAH3_1_GOV","IE-IGFS_2004_4_CEXP_135_GOV",
        "EVHOE_2008_4_THA2_134_GOV","EVHOE_1998_4_THA2_126_GOV",	
        "NS-IBTS_2007_1_WAH3_24_GOV",	"NS-IBTS_1991_1_TRI2_20_GOV",
        "NS-IBTS_2004_1_THA2_24_GOV","IE-IGFS_2004_4_CEXP_19_GOV",
        "NS-IBTS_1991_1_TRI2_7_GOV","IE-IGFS_2005_4_CEXP_3_GOV",
        "EVHOE_2001_4_THA2_102_GOV","NS-IBTS_2004_3_HAV_314_GOV",
        "IE-IGFS_2005_4_CEXP_4_GOV","NS-IBTS_2001_3_CIR_69_GOV",
        "NS-IBTS_1995_1_WAH3_4_GOV","IE-IGFS_2005_3_CEXP_4_GOV",
        "IE-IGFS_2005_3_CEXP_3_GOV")
find <- dat[dat$UniqueIDP%in%list &
             dat$AphiaID==126793]
# more likely to be C. Lyra at that size not C.maculatus
dat$estAphia_Code[dat$UniqueIDP%in%list&
                         dat$AphiaID==126793 &
                         dat$newLngtClass>16]<-126792      
dat$estsciname[dat$UniqueIDP%in%list&
                  dat$AphiaID==126793  &
                  dat$newLngtClass>16]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==126793 &
                 dat$newLngtClass>16]<-"Species_changed(DP)"

cr <- subset(dat, AphiaID==126795,)
plot(cr$Use_Lenght_cm, cr$HLNoAtLngtkm2)
list<-c('NS-IBTS_1991_1_TRI2_20_GOV','NS-IBTS_2004_1_THA2_24_GOV',
        'IE-IGFS_2004_4_CEXP_19_GOV','NS-IBTS_1991_1_TRI2_7_GOV',
        'IE-IGFS_2005_4_CEXP_3_GOV','NS-IBTS_2004_3_HAV_314_GOV',
        'IE-IGFS_2005_4_CEXP_4_GOV','NS-IBTS_2001_3_CIR_69_GOV',
        'IE-IGFS_2005_3_CEXP_4_GOV','IE-IGFS_2005_3_CEXP_3_GOV')
find <- dat[dat$UniqueIDP%in%list &
             dat$AphiaID==126795&dat$Use_Lenght_cm>11,]

dat$estAphia_Code[dat$UniqueIDP%in%list&
                         dat$AphiaID==126795 &
                         dat$newLngtClass>14]<-126792      
dat$estsciname[dat$UniqueIDP%in%list&
                  dat$AphiaID==126795  &
                  dat$newLngtClass>14]<-"Callionymus lyra"
dat$QC_Length[dat$UniqueIDP%in%list&
                 dat$AphiaID==126795 &
                 dat$newLngtClass>14]<-"Species_changed(DP)"
plot(dat$Use_Lenght_cm[dat$estAphia_Code==126795], 
     dat$HLNoAtLngtkm2[dat$estAphia_Code==126795])
# that looks grand and sensible, nach bhfuil muid go hiontach 
summary(as.factor(cm$Survey))
at<-subset(dat, AphiaID==126752,)
plot(at$Use_Lenght_cm, at$HLNoAtLngtkm2)
am<-subset(dat,AphiaID==126751, )
plot(am$Use_Lenght_cm, am$HLNoAtLngtkm2)

# as the Ammodytes tobianus and marinus will be combine to family levels
# I'll leave lenghts as is long A.tobianus  missidentified A.marinus or Hyperoplus
# no brainer for Notoscopelus kroyeri
# second pass
nb<-subset(dat, AphiaID==126642,)
summary(as.factor(nb$Country))
summary(as.factor((nb$Survey)))
cols<-rainbow(4)

plot(dat$Use_Lenght_cm[dat$AphiaID==126642], 
     dat$HLNoAtLngtkm2[dat$AphiaID==126642],
     pch=19, col="green")
summary(dat$newLngtClass)

find<-dat[dat$UniqueIDP=="NS-IBTS_2016_1_SCO3_46_GOV" &
             dat$AphiaID==126792 & dat$newLngtClass==95]
#dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2016_1_SCO3_46_GOV" &
#                 dat$AphiaID==126792 & dat$newLngtClass==95]<-"Length_10"
find<-dat[dat$UniqueIDP=="NS-IBTS_2000_1_WAH3_59_GOV" &
             dat$AphiaID==126928 & dat$newLngtClass==55]
dat$QC_Length[dat$UniqueIDP=="NS-IBTS_2000_1_WAH3_59_GOV" &
                 dat$AphiaID==126928 & dat$newLngtClass==55]<-"Length_10"

dat$LMax1.1<-dat$LmaxFB*1.1
# if lenght is more than 1.4 lmax and change to 1.1 Lmax 
summary(as.factor(dat$QC_Length))
dat$Use_Lenght_cm[dat$QC_Length=="Length_10"]<- dat$newLngtClass_10[dat$QC_Length=="Length_10"]
#dat$Use_Lenght_cm[dat$QC_Length=='Lmax+40%']<- dat$LMax1.4[dat$QC_Length=='Lmax+40%']

summary(dat$Use_Lenght_cm)
#list<-c('Length_changed(DP)', 'no_change','Species&Lenght_changed(DP)',
#        'Species_changed(DP)', 'Species_changed_to_genus(DP)' )
#dat$Use_Lenght_cm[dat$QC_Length%in%list]<- dat$newLngtClass[dat$QC_Length%in%list]
#summary(dat$Use_Lenght_cm)
# have we got them all now?
test1 <- dat[ which(dat$Use_Lenght_cm > dat$LmaxFB*1.1),]
# still 2049 oversized fish getting closer
test2<-dat[ which(dat$Use_Lenght_cm > dat$LmaxFB*1.4),]
# 1747 are within criteria for "big fish" and can be accepted as such
# 302 the fish are still "too big" based on lmax
# combination of missidentifications, poor estimate of Lmax and possibly 
# lenght problems???
summary(as.factor(test2$AphiaID))
dat$LmaxFB[dat$AphiaID==126662]<-NA
dat$LmaxFB[dat$AphiaID==126642]<-NA
dat$LmaxFB[dat$AphiaID==126752]<-NA
dat$LmaxFB[dat$AphiaID==126751]<-NA
dat$LmaxFB[dat$AphiaID==126756]<-NA
# Echiodon dentatus:Lmax is potentially strange, so Ill leave records as is
# Notacanthus bonaparte:Lmax is potentially strange,  so Ill leave records as is
# all Ammodytes are going to family level so not messing with the recorded lenghts
test1 <- dat[which(dat$Use_Lenght_cm > dat$LmaxFB*1.1),]
# still 2028 oversized fish getting closer
test2<-dat[which(dat$Use_Lenght_cm > dat$LmaxFB*1.4),]
test2<-subset(test2, QC_Length=="no_change",)
test2<-subset(test2, !family=="Gobiidae",)
# now that leaves 268 to look at
plot(dat$Use_Lenght_cm[dat$AphiaID==105851], 
     dat$HLNoAtLngt[dat$AphiaID==105851],
     pch=19, col="green")

# change oversized Taurulus bubalis to Myoxocephalus scorpius in line 
# with DP decisions
names(dat)
dat$estAphia_Code[dat$AphiaID==127204&
                         dat$newLngtClass>24.5]<-127203  
dat$estsciname[dat$AphiaID==127204&
                  dat$newLngtClass>24.5]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$AphiaID==127204&
                 dat$newLngtClass>24.5]<-"Species_changed(DP)"

find<-dat[dat$UniqueIDP=="NIGFS_2007_4_CO_19_ROT" &
             dat$AphiaID==126417 & dat$newLngtClass==935]
dat$QC_Length[dat$UniqueIDP=="NIGFS_2007_4_CO_19_ROT" &
                 dat$AphiaID==126417 & dat$newLngtClass==935]<-"Length/10"

dat$Use_Lenght_cm[dat$QC_Length=="Length/10"]<- dat$newLngtClass_10[dat$QC_Length=="Length/10"]
#dat$Use_Lenght_cm[dat$QC_Length=='Lmax+40%']<- dat$LMax1.4[dat$QC_Length=='Lmax+40%']

summary(dat$Use_Lenght_cm)
#list<-c('Length_changed(DP)', 'no_change','Species&Lenght_changed(DP)',
#        'Species_changed(DP)', 'Species_changed_to_genus(DP)' )
#dat$Use_Lenght_cm[dat$QC_Length%in%list]<- dat$newLngtClass[dat$QC_Length%in%list]
#summary(dat$Use_Lenght_cm)
test2<-dat[ which(dat$Use_Lenght_cm > dat$LmaxFB*1.4),]
test2<-subset(test2, QC_Length=="no_change",)
test2<-subset(test2, !family=="Gobiidae",)

dat$QC_Length[(dat$Use_Lenght_cm > dat$LmaxFB*1.4) & 
                 dat$QC_Length=="no_change" & !dat$family=="Gobiidae"]<-"length_unrelibable"

dat$Use_Lenght_cm1<-as.numeric(dat$Use_Lenght_cm)
dat$LMax1.1<-as.numeric(dat$LMax1.1)
summary(dat$Use_Lenght_cm1)
dat$Use_Lenght_cm1[dat$QC_Length=="length_unrelibable"]<-dat$LMax1.1[dat$QC_Length=="length_unrelibable"]
# all spp above the max lenght now gone
# replaced using 1.1Lmax
summary(dat$Use_Lenght_cm1)
dat$Use_Lenght_cm1[is.na(dat$Use_Lenght_cm1)]<-0
summary(dat$Use_Lenght_cm1)
test2<-dat[which(dat$Use_Lenght_cm1 > dat$LmaxFB*1.4),]
summary(as.factor(test2$estsciname))
#write.csv(dat, "Species_data_lngt_checked_10_10_2016.csv")
write.csv(dat, "dat_2766.csv")


# Check Lmin #


names(traits)
names(dat)
traits$estsciname<-traits$valid_name
dat$LmaxFB<-NULL
dat$valid_name<-NULL
dat$valid_authority<-NULL
dat$kingdom<-NULL
dat$phylum<-NULL
dat$order<-NULL
dat$family<-NULL
dat$genus<-NULL
dat$rank<-NULL
dat$FRS_Common.Name<-NULL
dat$CLASS<-NULL
dat$Lmin<-NULL
dat$LMax1.4<-NULL
dat$LMax1.1<-NULL
dat$LWRa<-NULL
dat$LWRb<-NULL
# new data frame
dat<-join(dat, traits, by="estsciname")
names(dat)
# have we got them all now?
test1 <- dat[ which(dat$Use_Lenght_cm1 > dat$LmaxFB*1.1),]
test2<-dat[ which(dat$Use_Lenght_cm1 > dat$LmaxFB*1.4),]
dat$Use_Lenght_cm1[dat$FishLength_cm==171&dat$valid_name=="Ammodytidae"]<-17.5
dat$QC_Length[which(dat$Use_Lenght_cm1 > dat$LmaxFB*1.4)]<-"length_unrelibable"
summary(as.factor(dat$QC_Length))
dat$Lmax1.1<-dat$LmaxFB*1.1
dat$Use_Lenght_cm1[dat$QC_Length=="length_unrelibable"]<-dat$Lmax1.1[dat$QC_Length=="length_unrelibable"]
test2<-dat[ which(dat$Use_Lenght_cm1 > dat$LmaxFB*1.4),]
dat$QC_Length[dat$QC_Length=="length_unrelibable"]<-"Lmax+10%"
# now Lmin 
# Is length less that 0.9 time species known Lmin
test1<-dat[which(dat$Use_Lenght_cm1 < dat$Lmin*0.9),]
# I don't believe some of these Lmins - larger than L max!
dat$Lmin[dat$estsciname=="Petromyzon marinus"]<-1.35
dat$Lmin[dat$estsciname=="Lampetra fluviatilis"]<-1.3
test1<-dat[which(dat$Use_Lenght_cm1 < dat$Lmin*0.9),]
summary(as.factor(test1$estsciname))
summary(dat$Lmin)
summary(test1$Use_Lenght_cm1)
test1<-subset(test1, !Use_Lenght_cm1==0,)
summary(test1$family)
test1<-dat[which(dat$Use_Lenght_cm1 < dat$Lmin*0.9),]
test1<-subset(test1, !Use_Lenght_cm1==0,)
dat$QC_Length[which(dat$Use_Lenght_cm1 < dat$Lmin*0.9)]
test2<-dat[which(dat$Use_Lenght_cm1 < dat$Lmin*0.5),]
test2<-subset(test2, !Use_Lenght_cm1==0,)
dat$QC_Length[which(dat$Use_Lenght_cm1 < dat$Lmin*0.9)]<-"Questionalble_Lmin"
summary(test2$Use_Lenght_cm1)

dat$QC_Length[dat$Use_Lenght_cm1==0]<-"No_Len_Data"
summary(as.factor(dat$QC_Length))

# Lengths - Catch Weights 
#start

dat$CatCatchWgt_N<-as.numeric(dat$CatCatchWgt)
summary(dat$CatCatchWgt_N)
dat$CatCatchWgt_N[is.na(dat$CatCatchWgt_N)]<-0
dat$CatCatchWgt_N[dat$CatCatchWgt_N==-9000]<-9000
dat$NewCatCatchWgt[dat$DataType=="R"]<-dat$CatCatchWgt_N[dat$DataType=="R"]
dat$NewCatCatchWgt[dat$DataType=="S"]<-dat$CatCatchWgt_N[dat$DataType=="S"]
dat$NewCatCatchWgt[dat$DataType=="C"]<-(dat$CatCatchWgt_N[dat$DataType=="C"])/60*dat$HaulDur[dat$DataType=="C"]
dat$CatCatchWgt_per_km2<-dat$NewCatCatchWgt/dat$SweptArea_wing_km_sqrd
summary(dat$NewCatCatchWgt)
summary(dat$CatCatchWgt_per_km2)
exp((log(9000)-log(.03))/3)

dat$fish_len_est_catch_weight<-exp((log(dat$NewCatCatchWgt)-log(dat$LWRa))/dat$LWRb)
dat$fish_len_est_catch_weight_round<-floor(dat$fish_len_est_catch_weight)
summary(dat$fish_len_est_catch_weight_round)
dat$FishLength_cm_below<-floor(dat$Use_Lenght_cm1)

summary(as.factor(dat$QC_Length))
list<-c("Length/10","Lmax+10%","No_Len_Data", "Questionalble_Lmin")
LFDab<-subset(dat, QC_Length%in%list&NewTotalNo<2, )
# If Length is missing or questionable then replace where possible with a 
# lenght dervied using the LWR a and b paramaters
dat$FishLength_cm_below[dat$QC_Length%in%list&dat$NewTotalNo<2&!dat$fish_len_est_catch_weight==0]<-dat$fish_len_est_catch_weight_round[dat$QC_Length%in%list&dat$NewTotalNo<2&!dat$fish_len_est_catch_weight==0]
dat$QC_Length[dat$QC_Length%in%list&dat$NewTotalNo<2&!dat$fish_len_est_catch_weight==0]<-"Lenght_Weight_Relationship"
# check wee fish
find<-subset(dat, dat$FishLength_cm_below<3,)
# remove the 0 lengths
find1<-subset(find, !FishLength_cm_below==0,)
# 1377 observations of small fish
summary(as.factor(find1$QC_Length))
# all BTS could get 1 - 2cm fish - accept these records
find2<-subset(find1, !Survey=="BTS",)
find2<-subset(find2, !Survey=="BTS-VIIa",)
# leaves 1077 records
summary(as.factor(find2$estsciname))
# Gobiidae and Caprosaper are okay
find2<-subset(find2, !estsciname=="Gobiidae",)
find2<-subset(find2, !estsciname=="Capros aper",)

# Coelorinchuscaelorhincus
# find2<-subset(find2, !SciName=="Coelorinchus caelorhincus",)
# Maurolicusmuelleri are really little fish - accept these lenghts
find2<-subset(find2, !estsciname=="Maurolicus muelleri",)
# accept Argyropelecushemigymnus 2 cm not unheard of
find2<-subset(find2, !estsciname=="Argyropelecus hemigymnus",)
# accept Trachurustrachurus
find2<-subset(find2, !estsciname=="Trachurus trachurus",)
# accept all wee
# Smoothheads and Searsids (Alepocephalidae and Searsidae) - SL
# Grenadiers (Macrouridae)- PAFL – Pre Anal Fin Length
# Chimaeridae (Rabbitfish) – PSCFL – Pre Supra Caudal Fin Length
find2<-subset(find2, !family=="Chimaeridae",)
find2<-subset(find2, !family=="Macrouridae",)
find2<-subset(find2, !family=="Alepocephalidae",)
find2<-subset(find2, !family=="Searsidae",)
write.csv(find2, "Very_small_fish_11-10-2016.csv")
unique(as.factor(find2$family))
summary(as.factor(find2$SciName))
summary(find1$NewHLNoAtLngt)
check<-subset(dat, dat$UniqueIDP=="EVHOE_1997_4_THA2_76_GOV",)

check1<-subset(dat, dat$UniqueIDP=="EVHOE_2014_4_THA2_19_GOV")
find<-subset(dat, dat$estsciname=="Bathysolea profundicola" &
               FishLength_cm_below<5,)
# length code error - fix lengths
dat$FishLength_cm_below[dat$estsciname=="Bathysolea profundicola" &
                               dat$FishLength_cm_below<5]<-dat$LngtClass[dat$estsciname=="Bathysolea profundicola" &
                                                                             dat$FishLength_cm_below<5]

check1<-subset(dat, dat$UniqueIDP=="NS-IBTS_2006_1_THA2_59_GOV")
# corrections from DPS
# netherlands
dat$FishLength_cm_below[dat$estsciname=="Clupea harengus" &
                               dat$UniqueIDP=="NS-IBTS_2000_1_TRI2_42_GOV" &
                               dat$LngtClass==20]<-20
dat$FishLength_cm_below[dat$estsciname=="Clupea harengus" &
                               dat$UniqueIDP=="NS-IBTS_2000_1_TRI2_8_GOV" &
                               dat$LngtClass==25]<-25
dat$FishLength_cm_below[dat$estsciname=="Callionymus reticulatus" &
                               dat$UniqueIDP=="NS-IBTS_2007_1_TRI2_5_GOV" &
                               dat$LngtClass==2]<-12

# 2 corrections from scotland 
#NS-IBTS_2009_3_SCO3_1_GOV Agonuscataphractus 1 should be 15cm
find<-subset(dat, dat$estsciname=="Agonus cataphractus" &
               dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_1_GOV"&
               dat$FishLength_cm_below==1,)

dat$FishLength_cm_below[dat$estsciname=="Agonus cataphractus" &
                               dat$UniqueIDP=="NS-IBTS_2009_3_SCO3_1_GOV"&
                               dat$FishLength_cm_below==1]<-15

#NS-IBTS_2001_1_SCO3_7_GOV Sardinapilchardus 1 should be 10cm
find<-subset(dat, dat$estsciname=="Sardina pilchardus" &
               dat$UniqueIDP=="NS-IBTS_2001_1_SCO3_7_GOV"&
               dat$FishLength_cm_below==1,)

dat$FishLength_cm_below[dat$estsciname=="Sardina pilchardus" &
                               dat$UniqueIDP=="NS-IBTS_2001_1_SCO3_7_GOV"&
                               dat$FishLength_cm_below==1]<-10
# 3 Sweden corrections

dat$FishLength_cm_below[dat$estsciname=="Trisopterus esmarkii" &
                               dat$UniqueIDP=="NS-IBTS_2016_1_DANS_46_GOV"&
                               dat$FishLength_cm_below==2]<-11


# NS-IBTS_1995_1_ARG_18_GOV	Platichthysflesus	should be 20

dat$FishLength_cm_below[dat$estsciname=="Platichthys flesus" &
                               dat$UniqueIDP=="NS-IBTS_1995_1_ARG_18_GOV"&
                               dat$FishLength_cm_below==2]<-20

# NS-IBTS_2007_3_ARG_33_GOV	Scophthalmusmaximus	should be 20
dat$FishLength_cm_below[dat$estsciname=="Scophthalmus maximus" &
                               dat$UniqueIDP=="NS-IBTS_2007_3_ARG_33_GOV"&
                               dat$FishLength_cm_below==2]<-20


# a very odd thing has happened with the use of length codes in this haul
find<-subset(dat, dat$estsciname=="Amblyraja radiata" &
               dat$LngtCode=="."&dat$Country=="FRA"&
               dat$Survey=="NS-IBTS"&dat$Year==2006,)
dat$FishLength_cm_below[dat$estsciname=="Amblyraja radiata" &
                               dat$LngtCode=="."&
                               dat$Country=="FRA"&
                               dat$Survey=="NS-IBTS"&
                               dat$Year==2006]<-dat$LngtClass[dat$estsciname=="Amblyraja radiata" &
                                                                          dat$LngtCode=="." &
                                                                          dat$Country=="FRA" &
                                                                          dat$Survey=="NS-IBTS" &
                                                                          dat$Year==2006]

# a very odd thing has happened with the use of length codes in this haul
find<-subset(dat, dat$estsciname=="Ciliata mustela" &
               dat$LngtCode=="."&dat$Country=="FRA"&
               dat$Survey=="NS-IBTS"&dat$Year==2006,)
dat$FishLength_cm_below[dat$estsciname=="Ciliata mustela" &
                               dat$LngtCode=="."&
                               dat$Country=="FRA"&
                               dat$Survey=="NS-IBTS"&
                               dat$Year==2006]<-dat$LngtClass[dat$estsciname=="Ciliata mustela" &
                                                                          dat$LngtCode=="." &
                                                                          dat$Country=="FRA" &
                                                                          dat$Survey=="NS-IBTS" &
                                                                          dat$Year==2006]
find<-subset(dat, dat$estsciname=="Alosa agone" &
               dat$LngtCode=="0"&dat$Country=="FRA"&
               dat$Survey=="NS-IBTS"&dat$FishLength_cm_below<5,)

dat$FishLength_cm_below[dat$estsciname=="Alosa agone" &
                               dat$LngtCode=="0"&
                               dat$Country=="FRA"&
                               dat$Survey=="NS-IBTS"&
                               dat$FishLength_cm_below<5]<-dat$LngtClass[dat$estsciname=="Alosa agone" &
                                                                                     dat$LngtCode=="0" &
                                                                                     dat$Country=="FRA" &
                                                                                     dat$Survey=="NS-IBTS"&
                                                                                     dat$FishLength_cm_below<5]

find<-subset(dat, dat$estsciname=="Alosa alosa" &
               dat$LngtCode=="0"&dat$Country=="FRA"&
               dat$Survey=="NS-IBTS"&dat$FishLength_cm_below<5,)

dat$FishLength_cm_below[dat$estsciname=="Alosa alosa" &
                               dat$LngtCode=="0"&
                               dat$Country=="FRA"&
                               dat$Survey=="NS-IBTS"&
                               dat$FishLength_cm_below<5]<-dat$LngtClass[dat$estsciname=="Alosa alosa" &
                                                                                     dat$LngtCode=="0" &
                                                                                     dat$Country=="FRA" &
                                                                                     dat$Survey=="NS-IBTS"& dat$FishLength_cm_below<5]
# nothing to suggest the Clupea harengus are wrong except mismatch with catch weights
find<-subset(dat, dat$estsciname=="Clupea harengus" & 
               dat$Year==1985 & dat$Country=="NOR",)
find<-subset(dat, dat$estsciname=="Clupea harengus" ,)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2)
find<-subset(dat, dat$estsciname=="Zeus faber" & 
               dat$UniqueIDP=="SWC-IBTS_2003_1_SCO3_99_GOV" ,)

#Chelidonichthysobscurus
find<-subset(dat, dat$estsciname=="Trisopterus esmarkii" & 
               dat$Year==2013 & dat$Country=="NOR",)
plot(find$LngtClass, find$HLNoAtLngtkm2)
# really looks like some of the samples lost the 1 at the front of them

find<-subset(dat, dat$estsciname=="Squalus acanthias" & 
               dat$UniqueIDP=="SWC-IBTS_2007_1_SCO3_64_GOV",)

plot(find$LngtClass, find$HLNoAtLngtkm2)
# let length = 1 equal length 0 and estimate sensible length
dat$FishLength_cm_below[dat$estsciname=="Squalus acanthias" &
                               dat$LngtClass=="1"]<-0

# Rajaclavata
find<-subset(dat, dat$estsciname=="Raja clavata" & 
               dat$UniqueIDP=="FR-CGFS_2003_4_GWD_62_GOV",)
# estimate the lenght
# let length = 1 equal length 0 and estimate sensible length
dat$FishLength_cm_below[dat$estsciname=="Raja clavata" &
                               dat$UniqueIDP=="FR-CGFS_2003_4_GWD_62_GOV"&
                               dat$Use_Lenght_cm=="10"]<-0


# poor cod
find<-subset(dat, dat$estsciname=="Trisopterus minutus" & 
               dat$Survey=="ROCKALL",)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2, pch=19)

find<-subset(dat, dat$estsciname=="Sprattus sprattus" & 
               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_15_GOV",)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2, pch=19)
# should be 12	NS-IBTS_1995_1_MIC_15_GOV
dat$FishLength_cm_below[dat$estsciname=="Sprattus sprattus" &
                               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_15_GOV"&
                               dat$Use_Lenght_cm=="1"]<-12


#range 10-39 cm, length code error	NS-IBTS_1995_1_MIC_23_GOV
dat$FishLength_cm_below[dat$estsciname=="Micromesistius poutassou" &
                               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_23_GOV"&
                               dat$Use_Lenght_cm=="1.5"]<-15
find<-subset(dat, dat$estsciname=="Micromesistius poutassou" & 
               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_23_GOV",)


# range 13-35 cm, length code error	NS-IBTS_1995_1_MIC_31_GOV
find<-subset(dat, dat$estsciname=="Micromesistius poutassou" & 
               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_31_GOV",)
dat$FishLength_cm_below[dat$estsciname=="Micromesistius poutassou" &
                               dat$UniqueIDP=="NS-IBTS_1995_1_MIC_31_GOV"&
                               dat$Use_Lenght_cm=="1.5"]<-15


# Copy the working file
write.csv(dat, "dat_workingHL_11-10-2016.csv")
#
dat<-read.csv("dat_workingHL_11-10-2016.csv")
find<-subset(dat, dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_44_GOV")

dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_44_GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-19
dat$Filter[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_44_GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==19]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_43_GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-17
dat$Filter[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_43_GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==17]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_42_GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-14
dat$Filter[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_42_GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==14]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_34_GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-15
dat$Filter[dat$UniqueIDP=="NS-IBTS_2005_1_DAN2_34_GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==15]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_43_GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-18
dat$Filter[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_43_GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==18]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_40_GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-20
dat$Filter[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_40_GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==20]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_38_GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-20
dat$Filter[dat$UniqueIDP=="NS-IBTS_2004_3_DAN2_38_GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==20]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2007_4_CO_29_ROT"&
                         dat$estsciname=="Phrynorhombus norvegicus"&
                         dat$Filter=="LFD"]<-7
dat$Filter[dat$UniqueIDP=="NIGFS_2007_4_CO_29_ROT"&
            dat$estsciname=="Phrynorhombus norvegicus"&
            dat$FishLength_cm_below==7]<-"OK"
dat$FishLength_cm_below[dat$UniqueIDP=="EVHOE_2015_4_THA2_46_GOV"&
                         dat$estsciname=="Merlangius merlangus"&
                         dat$Filter=="LFD"]<-37
dat$Filter[dat$UniqueIDP=="EVHOE_2015_4_THA2_46_GOV"&
            dat$estsciname=="Merlangius merlangus"&
            dat$FishLength_cm_below==37]<-"OK"
# Syngnathus rostellatus which is more likely to be S. acus based on size
list<-c("BTS_2004_3_COR_100_BT4","BTS_2006_3_COR_37_BT4","BTS_2006_3_COR_39_BT4")

dat$estsciname[dat$HaulID%in%list&dat$estsciname=="Syngnathus rostellatus"
                &dat$FishLength_cm_below>24]<-"Syngnathus acus"
dat$estAphia_Code[dat$HaulID%in%list&dat$estsciname=="Syngnathus rostellatus"
                   &dat$FishLength_cm_below>24]<-"127387"
dat$Filter[dat$HaulID%in%list&dat$estsciname=="Syngnathus rostellatus"
            &dat$FishLength_cm_below>24]<-"OK"

# Echiichthys vipera should be Trachinus draco
dat$estsciname[dat$estsciname=="Echiichthys vipera"&
                  dat$estAphia_Code==150630&
                  dat$FishLength_cm_below>22.5]<-"Trachinus draco"
dat$QC_Length[dat$FishLength_cm_below==150630&
                 dat$newLngtClass>22.5]<-"Species_changed"
dat$estAphia_Code[dat$estAphia_Code==150630&
                     dat$FishLength_cm_below>22.5]<-127082  
# Lepadogaster at 20cm - should be 2cm
dat$FishLength_cm_below[dat$FishLength_cm==20&
                           dat$estsciname=="Lepadogaster lepadogaster"]<-2

# ADD  FLITER FOR BASELINE -----------------------------------------

summary(dat$Density_Km2)

summary(as.factor(dat$SpeciesQualityCode))
list<-c('Genus_to_spp',  'Recorded_Data', 'Family_to_spp', 'Species_changed')
dat$Filter[dat$SpeciesQualityCode%in%list]<-"OK"
dat$Filter[!dat$SpeciesQualityCode%in%list]<-"SC"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Species"]<-"LFD"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Genus"]<-"SCLFD"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Family"]<-"SCLFD"
dat$Filter[dat$estsciname=="Gobiidae"&!dat$FishLength_cm_below=="0"]<-"OK"
dat$Filter[dat$estsciname=="Ammodytidae"&!dat$FishLength_cm_below=="0"]<-"OK"
dat$Filter[dat$estsciname=="Gobiidae"&dat$FishLength_cm_below=="0"]<-"LFD"
dat$Filter[dat$estsciname=="Ammodytidae"&dat$FishLength_cm_below=="0"]<-"LFD"
names(dat)
dat$Filter[dat$FishLength_cm_below=="0"&dat$Density_Km2=="0"&dat$TotalNoKm2==0]<-"CatchWeight"
summary(as.factor(dat$Filter))

# Abundance of Fish --------------------

# Plot Summed Biomass at Lenght for a species in a hauls
# against Reported Catch Weight
names(dat)
# Some columns could be removed to make the file more managable
dat$FRS_Common.Name<-NULL
dat$kingdom<-NULL
dat$phylum<-NULL
dat$CLASS<-NULL
dat$valid_authority<-NULL
dat$SweptArea_wing_m_sqrd<-NULL
dat$HaulVal<-NULL
summary((dat$FishLength_cm_below))
summary(dat$LWRa)

dat<-subset(dat, !is.na(LWRa)&!is.na(LWRb)&!LWRa==0&!LWRb==0&!CatCatchWgt_per_km2==0&!HLNoAtLngtkm2==0&!FishLength_cm_below==0,)           
names(dat)
summary(as.factor(dat$Filter))
summary(dat$FishLength_cm_below)
dat$X.1<-NULL
dat$X<-NULL
dat$AphiaID.1<-NULL                 
dat$LWRa<-NULL
dat$AphiaID.2<-NULL
dat$valid_name<-NULL
dat$order<-NULL
dat$family<-NULL
dat$genus<-NULL
dat$rank<-NULL
dat$LmaxFB<-NULL
dat$LWRa.1<-NULL
dat$LWRb<-NULL
dat$Lmin<-NULL
dat$Lmax1.1<-NULL                        
dat.1<-merge(dat, traits, by="AphiaID")
names(dat.1)
summary(dat.1$FishLength_cm_below)
summary(dat.1$Density_Km2)
Weight_Check<-ddply(dat.1, c("NewUniqueID2", "Survey", "Country",
                            "Year", "estsciname", "HLNoAtLngtkm2", 
                            "FishLength_cm_below", "CatCatchWgt_per_km2", 
                            "CatIdentifier"),
                       summarise, EstimatedWeight=sum(LWRa*FishLength_cm_below^LWRb))
write.csv(Weight_Check, "Summary_weights_lengths_15-110-2016.csv")
summaryweight<-ddply(Weight_Check, c("NewUniqueID2", "Survey", "Country",
                                     "Year", "estsciname","CatCatchWgt_per_km2",
                                     "CatIdentifier"),
                                      summarise, 
                                      EstimatedCatchWeight=sum(as.numeric(EstimatedWeight*HLNoAtLngtkm2)))
summaryweight1<-ddply(summaryweight, c("NewUniqueID2", "Survey", "Country",
                                     "Year", "estsciname","EstimatedCatchWeight"),      
                     summarise, AggCatCatchWgt_per_km2=sum(CatCatchWgt_per_km2))

write.csv(summaryweight, "Summary_Fish_CatchWeight.csv")
write.csv(summaryweight1, "Summary1_Fish_CatchWeight.csv")
summary(summaryweight1$AggCatCatchWgt_per_km2)
summaryweight1<-subset(summaryweight1, !is.na(AggCatCatchWgt_per_km2),)
summaryweight1<-subset(summaryweight1, !(AggCatCatchWgt_per_km2==0),)
summaryweight1<-subset(summaryweight1, !is.na(EstimatedCatchWeight),)
summaryweight1<-subset(summaryweight1, !(EstimatedCatchWeight==0),)
plot(summaryweight1$CatCatchWgt_per_km2, summaryweight1$EstimatedCatchWeight, pch=19)

for (cat in unique(summaryweight1$estsciname)){
  mypath <- file.path(paste("Weight_Check_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
    d <- subset(summaryweight1, estsciname == cat)
    plot(d$AggCatCatchWgt_per_km2, d$EstimatedCatchWeight, 
       main=unique(d$estsciname), pch=19, xlab="Recorded Catch Weight (g/Km2)", 
       ylab="Estimated Catch Weight (g/Km2)")
    abline(0,1, col="red")
    abline(0,0.75, col='red', lty=2)
    abline(0,1.25, col='red', lty=2)
  dev.off()
}

# Plot Summed Densities at lenght against reported Catch weight
Weight_Check1<-ddply(dat, c("NewUniqueID2", "Survey", "Country",
                             "Year", "estsciname", "CatCatchWgt_per_km2", 
                             "TotalNoKm2", "CatIdentifier" ),
                      summarise, SummedHLNoLng=sum(HLNoAtLngtkm2))

Weight_Check2<-ddply(Weight_Check1, c("NewUniqueID2", "Survey", "Country",
                                      "Year", "estsciname",  
                                      "TotalNoKm2", "SummedHLNoLng" ),
                     summarise, SummedCatCatchWgt=sum(CatCatchWgt_per_km2))

Weight_Check1<-subset(Weight_Check1, !is.na(CatCatchWgt_per_km2),)
Weight_Check1<-subset(Weight_Check1, !(CatCatchWgt_per_km2==0),)
Weight_Check1<-subset(Weight_Check1, !is.na(SummedHLNoLng),)
Weight_Check1<-subset(Weight_Check1, !(SummedHLNoLng==0),)

write.csv(Weight_Check1, "Weight_Density_Checks.csv")

plot(Weight_Check1$CatCatchWgt_per_km2, Weight_Check1$SummedHLNoLng)
summary(as.factor(Weight_Check1$Survey))

for (cat in unique(Weight_Check2$estsciname)){
  mypath <- file.path(paste("Density_at_Weight_Check_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(Weight_Check2, estsciname == cat)
  plot(d$SummedCatCatchWgt, d$SummedHLNoLng, 
       main=unique(d$estsciname), pch=19, xlab="Recorded Catch Weight (g/Km2)", 
       ylab="Density (no/km2)")
  dev.off()
}

# plot Lenght frequency distributions per species per counrty per year.
for (cat in unique(dat$estsciname)){
  mypath <- file.path(paste("Length_Frequency_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(dat, estsciname == cat)
  plot(d$FishLength_cm_below, d$HLNoAtLngtkm2, 
       main=unique(d$estsciname), pch=19, xlab="Length (cm)", 
       ylab="Density at Lenght (km2)")
  dev.off()
}

# Change CatchWeights to LFD --------------

find<-subset(dat, dat$Filter=="CatchWeight",)
dat<-subset(dat, !(dat$Filter=="CatchWeight"&is.na(dat$CatCatchWgt)),)
nrow(dat)-nrow(dat)
find<-subset(dat, dat$Filter=="CatchWeight",)
list<-unique(as.factor(dat$estsciname[dat$Filter=="CatchWeight"]))
find<-subset(dat, dat$Density_Km2==0,)

meancatchweight<-ddply(summaryweight1,
                       c("Survey", "Country","Year", "estsciname"),      
                        summarise, meanAggCatCatchWgt_per_km2=mean(AggCatCatchWgt_per_km2))
meancatchweight1<-subset(meancatchweight, meancatchweight$estsciname%in%list,)
list<-unique(as.factor(dat$Year[dat$Filter=="CatchWeight"]))
meancatchweight1<-subset(meancatchweight1, meancatchweight1$Year%in%list,)
list<-unique(as.factor(dat$Survey[dat$Filter=="CatchWeight"]))
meancatchweight1<-subset(meancatchweight1, meancatchweight1$Survey%in%list,)

write.csv(find, "Catch_Weight_only_11-10-2016.csv")
write.csv(meancatchweight, "mean_catchweight-11-10-2016.csv")
find<-subset(dat, Filter=="CatchWeight",)
# Fix Catch weights
summary(as.factor(dat$Filter))
ceiling(268456.3758/283215.5222)
1/0.017284
summary(dat$Density_Km2)
dat$Filter[dat$Density_Km2==0]<-"CatchWeight"
summary(as.factor(dat$Filter))
# Scyliorhinus canicula 2002
dat$Density_Km2[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_24_BT4A"&
               dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-57.85698
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_24_BT4A"&
                 dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-57.85698
dat$FishLength_cm_below[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_24_BT4A"&
                     dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-105
dat$Filter[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_24_BT4A"&
                           dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-"0K"
# Scyliorhinus canicula
50447.2512769591/22478.960064874
2/0.0475348
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2014_4_NOR_61_NCT"&
                 dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-42.07444
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2014_4_NOR_61_NCT"&
                     dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-42.07444
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2014_4_NOR_61_NCT"&
                           dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-89
dat$Filter[dat$UniqueIDP=="PT-IBTS_2014_4_NOR_61_NCT"&
              dat$estsciname=="Scyliorhinus canicula"&dat$Filter=="CatchWeight"]<-"0K"
#Syngnathidae
7619.90139/778.508066614135
10/0.008924
dat$Density_Km2[dat$UniqueIDP=="BTS_2015_3_END_55_BT4A"&
                 dat$estsciname=="Syngnathidae"&dat$Filter=="CatchWeight"]<-1120.574
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS_2015_3_END_55_BT4A"&
                     dat$estsciname=="Syngnathidae"&dat$Filter=="CatchWeight"]<-1120.574
dat$FishLength_cm_below[dat$UniqueIDP=="BTS_2015_3_END_55_BT4A"&
                           dat$estsciname=="SSyngnathidae"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="BTS_2015_3_END_55_BT4A"&
              dat$estsciname=="Syngnathidae"&dat$Filter=="CatchWeight"]<-"SCLFD"
#Conger conger
25792.7239485665/12283.83003
2/0.0447413
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_17_NCT"&
                 dat$estsciname=="Conger conger"&dat$Filter=="CatchWeight"]<-44.70143
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_17_NCT"&
                     dat$estsciname=="Conger conger"&dat$Filter=="CatchWeight"]<-44.70143
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_17_NCT"&
                           dat$estsciname=="Conger conger"&dat$Filter=="CatchWeight"]<-86
dat$Filter[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_17_NCT"&
              dat$estsciname=="Conger conger"&dat$Filter=="CatchWeight"]<-"0K"
# Trisopterus esmarkii  NS-IBTS_2011_3_JHJ_301_GOV
236.7954889/117938.5798
#1 fish ?
1/0.067568855
dat$Density_Km2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_301_GOV"&
                 dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-14.79972
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_301_GOV"&
                     dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-14.79972
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_301_GOV"&
                           dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-12
dat$Filter[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_301_GOV"&
              dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-"0K"
# Trisopterus esmarkii NS-IBTS_2011_3_JHJ_260_GOV
1380.359397/117938.5798
#1 fish?
1/0.062676914
dat$Density_Km2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_260_GOV"&
                 dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-15.95484
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_260_GOV"&
                     dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-15.95484
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_260_GOV"&
                           dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-21
dat$Filter[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_260_GOV"&
              dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-"0K"
# Trisopterus luscus BTS-VIIa_2002_3_COR_8_BT4A
12462.61216/35414.47696
# 1 Fish
1/0.016048
dat$Density_Km2[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_8_BT4A"&
                 dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-62.31306
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_8_BT4A"&
                     dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-62.31306
dat$FishLength_cm_below[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_8_BT4A"&
                           dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-23
dat$Filter[dat$UniqueIDP=="BTS-VIIa_2002_3_COR_8_BT4A"&
              dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-"0K"
# Trisopterus luscus PT-IBTS_2013_4_NOR_47_NCT
32710.2699295729/68232.24859
1/0.0447413
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_47_NCT"&
                 dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-22.35071
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_47_NCT"&
                     dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-22.35071
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_47_NCT"&
                           dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-43
dat$Filter[dat$UniqueIDP=="PT-IBTS_2013_4_NOR_47_NCT"&
              dat$estsciname=="Trisopterus luscus"&dat$Filter=="CatchWeight"]<-"0K"
# Scomber scombrus PT-IBTS_2008_3_NOR_81_NCT
33.6845226583415/244739.3541
1/0.0223631
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2008_3_NOR_81_NCT"&
                 dat$estsciname=="Scomber scombrus"&dat$Filter=="CatchWeight"]<-44.71652
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2008_3_NOR_81_NCT"&
                     dat$estsciname=="Scomber scombrus"&dat$Filter=="CatchWeight"]<-44.71652
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2008_3_NOR_81_NCT"&
                           dat$estsciname=="Scomber scombrus"&dat$Filter=="CatchWeight"]<-33
dat$Filter[dat$UniqueIDP=="PT-IBTS_2008_3_NOR_81_NCT"&
              dat$estsciname=="Scomber scombrus"&dat$Filter=="CatchWeight"]<-"0K"

# Boops boops PT-IBTS_2006_3_NOR_7_NCT
117111.0489/76370.49092
2/0.0503283
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_7_NCT"&
                 dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-39.73907
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_7_NCT"&
                     dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-39.73907
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_7_NCT"&
                           dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_7_NCT"&
              dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-"0K"

# Boops boops  PT-IBTS_2010_4_NOR_12_NCT
29044.75283/116611.0045
1/0.0503283
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                 dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-19.86954
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                     dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-19.86954
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                           dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-49
dat$Filter[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
              dat$estsciname=="Boops boops"&dat$Filter=="CatchWeight"]<-"0K"

# Spondyliosoma cantharus  PT-IBTS_2009_4_NOR_72_NCT
37951.14316/64202.09959
1/0.0475348
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2009_4_NOR_72_NCT"&
                 dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-21.03722
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2009_4_NOR_72_NCT"&
                     dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-21.03722
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2009_4_NOR_72_NCT"&
                           dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-54
dat$Filter[dat$UniqueIDP=="PT-IBTS_2009_4_NOR_72_NCT"&
              dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-"0K"

# Spondyliosoma cantharus  PT-IBTS_2010_4_NOR_12_NCT
82027.12036/64202.09959
1/0.0447413
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                 dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-22.35071
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                     dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-22.35071
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
                           dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-68
dat$Filter[dat$UniqueIDP=="PT-IBTS_2010_4_NOR_12_NCT"&
              dat$estsciname=="Spondyliosoma cantharus"&dat$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS-VIIa_2014_3_END_134_BT4A
11784.7411444142/21951.93821
1/0.01468
dat$Density_Km2[dat$UniqueIDP=="BTS-VIIa_2014_3_END_134_BT4A"&
                 dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-68.11989
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS-VIIa_2014_3_END_134_BT4A"&
                     dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-68.11989
dat$FishLength_cm_below[dat$UniqueIDP=="BTS-VIIa_2014_3_END_134_BT4A"&
                           dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-23
dat$Filter[dat$UniqueIDP=="BTS-VIIa_2014_3_END_134_BT4A"&
              dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS_2013_3_END_108_BT4A
1344.430218/32855.76347
1/0.01562
dat$Density_Km2[dat$UniqueIDP=="BTS_2013_3_END_108_BT4A"&
                 dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-64.02049
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS_2013_3_END_108_BT4A"&
                     dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-64.02049
dat$FishLength_cm_below[dat$UniqueIDP=="BTS_2013_3_END_108_BT4A"&
                           dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-11
dat$Filter[dat$UniqueIDP=="BTS_2013_3_END_108_BT4A"&
              dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS_2013_3_END_25_BT4A
5264.543301/32855.76347
1/0.015196

dat$Density_Km2[dat$UniqueIDP=="BTS_2013_3_END_25_BT4A"&
                 dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-65.80679
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS_2013_3_END_25_BT4A"&
                     dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-65.80679
dat$FishLength_cm_below[dat$UniqueIDP=="BTS_2013_3_END_25_BT4A"&
                           dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-18
dat$Filter[dat$UniqueIDP=="BTS_2013_3_END_25_BT4A"&
              dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-"0K"

# Trachinus draco  BTS_2013_3_END_2_BT4A
3584.743332/32855.76347
1/0.013948
dat$Density_Km2[dat$UniqueIDP=="BTS_2013_3_END_2_BT4A"&
                 dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-71.69487
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS_2013_3_END_2_BT4A"&
                     dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-71.69487
dat$FishLength_cm_below[dat$UniqueIDP=="BTS_2013_3_END_2_BT4A"&
                           dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-15
dat$Filter[dat$UniqueIDP=="BTS_2013_3_END_2_BT4A"&
              dat$estsciname=="Trachinus draco"&dat$Filter=="CatchWeight"]<-"0K"

# Lepidopus caudatus PT-IBTS_2006_3_NOR_15_NCT
317912.586/111309.5422
3/0.0503283
dat$Density_Km2[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_15_NCT"&
                 dat$estsciname=="Lepidopus caudatus"&dat$Filter=="CatchWeight"]<-19.86954
dat$HLNoAtLngtkm2[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_15_NCT"&
                     dat$estsciname=="Lepidopus caudatus"&dat$Filter=="CatchWeight"]<-19.86954
dat$FishLength_cm_below[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_15_NCT"&
                           dat$estsciname=="Lepidopus caudatus"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="PT-IBTS_2006_3_NOR_15_NCT"&
              dat$estsciname=="Lepidopus caudatus"&dat$Filter=="CatchWeight"]<-"LFD"

# Hippoglossoides platessoides NS-IBTS_2011_3_JHJ_249_GOV
3479.014221/3564.508492
1/0.056164826

dat$Density_Km2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_249_GOV"&
                 dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-17.80474
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_249_GOV"&
                     dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-17.80474
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_249_GOV"&
                           dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-28
dat$Filter[dat$UniqueIDP=="NS-IBTS_2011_3_JHJ_249_GOV"&
              dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-"0K"

# Limanda limanda BTS_2003_3_SOL_23_BT7
1385289.75/2585.386748
536/0.0263952
dat$Density_Km2[dat$UniqueIDP=="BTS_2003_3_SOL_23_BT7"&
                 dat$estsciname=="Limanda limanda"&dat$Filter=="CatchWeight"]<-20306.72
dat$HLNoAtLngtkm2[dat$UniqueIDP=="BTS_2003_3_SOL_23_BT7"&
                     dat$estsciname=="Limanda limanda"&dat$Filter=="CatchWeight"]<-20306.72
dat$FishLength_cm_below[dat$UniqueIDP=="BTS_2003_3_SOL_23_BT7"&
                           dat$estsciname=="Limanda limanda"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="BTS_2003_3_SOL_23_BT7"&
              dat$estsciname=="Limanda limanda"&dat$Filter=="CatchWeight"]<-"LFD"

1_0.04944979
dat$Density_Km2[dat$UniqueIDP=="NIGFS_2001_4_LF_33_ROT"&
                   dat$estsciname=="Gobiidae"&dat$Filter=="CatchWeight"]<-20.22253
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2001_4_LF_33_ROT"&
                     dat$estsciname=="Gobiidae"&dat$Filter=="CatchWeight"]<-20.22253
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2001_4_LF_33_ROT"&
                           dat$estsciname=="Gobiidae"&dat$Filter=="CatchWeight"]<-10
dat$Filter[dat$UniqueIDP=="NIGFS_2001_4_LF_33_ROT"&
              dat$estsciname=="Gobiidae"&dat$Filter=="CatchWeight"]<-"0K"
1_0.04909500
dat$Density_Km2[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                   dat$estsciname=="Osmerus eperlanus"&dat$Filter=="CatchWeight"]<-20.36867
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                     dat$estsciname=="Osmerus eperlanus"&dat$Filter=="CatchWeight"]<-20.36867
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                           dat$estsciname=="Osmerus eperlanus"&dat$Filter=="CatchWeight"]<-8
dat$Filter[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
              dat$estsciname=="Osmerus eperlanus"&dat$Filter=="CatchWeight"]<-"0K"

dat$Density_Km2[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                   dat$estsciname=="Alosa agone"&dat$Filter=="CatchWeight"]<-20.36867
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                     dat$estsciname=="Alosa agone"&dat$Filter=="CatchWeight"]<-20.36867
dat$FishLength_cm_below[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
                           dat$estsciname=="Alosa agone"&dat$Filter=="CatchWeight"]<-8
dat$Filter[dat$UniqueIDP=="NS-IBTS_2012_1_THA2_58_GOV"&
              dat$estsciname=="Alosa agone"&dat$Filter=="CatchWeight"]<-"0K"
1_0.08594431
dat$Density_Km2[dat$UniqueIDP=="NIGFS_2004_1_LF_20_ROT"&
                   dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-11.63544
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2004_1_LF_20_ROT"&
                     dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-11.63544
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2004_1_LF_20_ROT"&
                           dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-39
dat$Filter[dat$UniqueIDP=="NIGFS_2004_1_LF_20_ROT"&
              dat$estsciname=="Hippoglossoides platessoides"&dat$Filter=="CatchWeight"]<-"0K"

summary(dat$Density_Km2)
# NIGFS_2007_1_CO_10_ROT Callionymus maculatus

dat$Density_Km2[dat$UniqueIDP=="NIGFS_2007_1_CO_10_ROT"&
                   dat$estsciname=="Callionymus maculatus"&dat$Filter=="CatchWeight"]<-105.3899
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2007_1_CO_10_ROT"&
                     dat$estsciname=="Callionymus maculatus"&dat$Filter=="CatchWeight"]<-105.3899
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2007_1_CO_10_ROT"&
                           dat$estsciname=="Callionymus maculatus"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="NIGFS_2007_1_CO_10_ROT"&
              dat$estsciname=="Callionymus maculatus"&dat$Filter=="CatchWeight"]<-"LFD"


#NIGFS_2006_4_CO_1_ROT Sprattus sprattus

#		Sprattus sprattus
dat$Density_Km2[dat$UniqueIDP=="NIGFS_2006_4_CO_1_ROT"&
                   dat$estsciname=="Sprattus sprattus"&dat$Filter=="CatchWeight"]<-43.90143
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2006_4_CO_1_ROT"&
                     dat$estsciname=="Sprattus sprattus"&dat$Filter=="CatchWeight"]<-43.90143
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2006_4_CO_1_ROT"&
                           dat$estsciname=="Sprattus sprattus"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="NIGFS_2006_4_CO_1_ROT"&
              dat$estsciname=="Sprattus sprattus"&dat$Filter=="CatchWeight"]<-"LFD"

#NIGFS_2003_1_LF_21_ROT Trisopterus esmarkii

#2003		Trisopterus esmarkii
dat$Density_Km2[dat$UniqueIDP=="NIGFS_2003_1_LF_21_ROT"&
                   dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-228.0875
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2003_1_LF_21_ROT"&
                     dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-228.0875
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2003_1_LF_21_ROT"&
                           dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="NIGFS_2003_1_LF_21_ROT"&
              dat$estsciname=="Trisopterus esmarkii"&dat$Filter=="CatchWeight"]<-"LFD"
#NIGFS_2003_1_LF_11_ROT Platichthys flesus

#2003		Platichthys flesus
dat$Density_Km2[dat$UniqueIDP=="NIGFS_2003_1_LF_11_ROT"&
                   dat$estsciname=="Platichthys flesus"&dat$Filter=="CatchWeight"]<-73.70758
dat$HLNoAtLngtkm2[dat$UniqueIDP=="NIGFS_2003_1_LF_11_ROT"&
                     dat$estsciname=="Platichthys flesus"&dat$Filter=="CatchWeight"]<-73.70758
dat$FishLength_cm_below[dat$UniqueIDP=="NIGFS_2003_1_LF_11_ROT"&
                           dat$estsciname=="Platichthys flesus"&dat$Filter=="CatchWeight"]<-0
dat$Filter[dat$UniqueIDP=="NIGFS_2003_1_LF_11_ROT"&
              dat$estsciname=="Platichthys flesus"&dat$Filter=="CatchWeight"]<-"0K"


summary(dat$Density_Km2)
summary(dat$HLNoAtLngtkm2)
summary(dat$HLNoAtLngt_N)

# Check inconistent abundances and catch weights ---------------
summary(dat$fish_len_est_catch_weight)
names(dat)
dat$Derived_Indiv_Fish_weight_At_Lng<-dat$LWRa*(dat$FishLength_cm_below^dat$LWRb)
summary(dat$Derived_Indiv_Fish_weight_At_Lng)
dat$Derived_Indiv_Fish_weight_At_Lng[dat$Derived_Indiv_Fish_weight_At_Lng==0]<-NA

summary(dat$Density_Km2)
summary(dat$TotalNoKm2)
summary(dat$CatCatchWgt_per_km2)
dat$TotalNo_km2_derived_from_weight<-dat$CatCatchWgt_per_km2/dat$Derived_Indiv_Fish_weight_At_Lng
dat$Difference_in_total_weight<-dat$TotalNoKm2-dat$TotalNo_km2_derived_from_weight
dat$Difference_in_Density_weight<-sqrt((dat$Density_Km2-dat$TotalNo_km2_derived_from_weight)^2)

summary(dat$TotalNo_km2_derived_from_weight)
summary(dat$TotalNoKm2)
summary(dat$Difference_in_Density_weight)
summary(dat$Difference_in_total_weight)

boxplot(dat$Difference_in_total_weight~dat$Survey)

ggplot(dat, aes(x=CatCatchWgt_per_km2, y=TotalNo_km2_derived_from_weight, colour=Survey))+
  geom_jitter()

ggplot(dat, aes(x=TotalNoKm2, y=TotalNo_km2_derived_from_weight, colour=Survey))+
  geom_jitter()


find<-subset(dat, dat$Difference_in_total_weight>1000000,)
summary(as.factor(find$Survey))


find<-subset(dat, Density_Km2==0,)
find<-subset(dat, dat$HLNoAtLngtkm2>dat$TotalNoKm2)
summary(dat$Density_Km2)
#summary(dat$Density_Km21)
#Weight_Check<-ddply(dat, c("NewUniqueID2", "Survey", "Country",
#                            "Year", "estsciname", "Density_Km21", 
##                            "FishLength_cm_below", "CatCatchWgt_per_km2", 
#                            "CatIdentifier"),
#                    summarise, EstimatedWeight=sum(LWRa*FishLength_cm_below^LWRb))
#write.csv(Weight_Check, "Summary_weights_lengths_10-10-2016.csv")
#summaryweight<-ddply(Weight_Check, c("NewUniqueID2", "Survey", "Country",
#                                     "Year", "estsciname","CatCatchWgt_per_km2",
#                                     "CatIdentifier"),
#                     summarise, 
#                     EstimatedCatchWeight=sum(as.numeric(EstimatedWeight*Density_Km21)))
#summaryweight1<-ddply(summaryweight, c("NewUniqueID2", "Survey", "Country",
#                                       "Year", "estsciname","EstimatedCatchWeight"),      
#                      summarise, AggCatCatchWgt_per_km2=sum(CatCatchWgt_per_km2))

#write.csv(summaryweight, "Summary_Fish_CatchWeight_corrections1.csv")
#write.csv(summaryweight1, "Summary1_Fish_CatchWeight_corrections1.csv")
#summary(summaryweight1$AggCatCatchWgt_per_km2)
#summaryweight1<-subset(summaryweight1, !is.na(AggCatCatchWgt_per_km2),)
#summaryweight1<-subset(summaryweight1, !(AggCatCatchWgt_per_km2==0),)
#summaryweight1<-subset(summaryweight1, !is.na(EstimatedCatchWeight),)
#summaryweight1<-subset(summaryweight1, !(EstimatedCatchWeight==0),)
#plot(summaryweight1$CatCatchWgt_per_km2, summaryweight1$EstimatedCatchWeight, pch=19)

for (cat in unique(summaryweight1$estsciname)){
  mypath <- file.path(paste("C1_Weight_Check_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(summaryweight1, estsciname == cat)
  plot(d$AggCatCatchWgt_per_km2, d$EstimatedCatchWeight, 
       main=unique(d$estsciname), pch=19, xlab="Recorded Catch Weight (g/Km2)", 
       ylab="Estimated Catch Weight (g/Km2)")
  abline(0,1, col="red")
  abline(0,0.75, col='red', lty=2)
  abline(0,1.25, col='red', lty=2)
  dev.off()
}

# Plot Summed Densities at lenght against reported Catch weight
#Weight_Check1<-ddply(dat, c("NewUniqueID2", "Survey", "Country",
#                             "Year", "estsciname", "CatCatchWgt_per_km2", 
#                             "TotalNoKm2", "CatIdentifier" ),
#                     summarise, SummedHLNoLng=sum(Density_Km21))

#Weight_Check2<-ddply(Weight_Check1, c("NewUniqueID2", "Survey", "Country",
 #                                     "Year", "estsciname",  
 #                                     "TotalNoKm2", "SummedHLNoLng" ),
 #                    summarise, SummedCatCatchWgt=sum(CatCatchWgt_per_km2))

#Weight_Check1<-subset(Weight_Check1, !is.na(CatCatchWgt_per_km2),)
#Weight_Check1<-subset(Weight_Check1, !(CatCatchWgt_per_km2==0),)
#Weight_Check1<-subset(Weight_Check1, !is.na(SummedHLNoLng),)
#Weight_Check1<-subset(Weight_Check1, !(SummedHLNoLng==0),)

#write.csv(Weight_Check1, "Weight_Density_Checks_c1.csv")

#plot(Weight_Check1$CatCatchWgt_per_km2, Weight_Check1$SummedHLNoLng)
#summary(as.factor(Weight_Check1$Survey))

for (cat in unique(Weight_Check2$estsciname)){
  mypath <- file.path(paste("C1_Density_at_Weight_Check_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(Weight_Check2, estsciname == cat)
  plot(d$SummedCatCatchWgt, d$SummedHLNoLng, 
       main=unique(d$estsciname), pch=19, xlab="Recorded Catch Weight (g/Km2)", 
       ylab="Density (no/km2)")
  dev.off()
}

# plot Lenght frequency distributions per species per counrty per year.
for (cat in unique(dat$estsciname)){
  mypath <- file.path(paste("C2_Length_Frequency_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(dat, estsciname == cat)
  plot(d$FishLength_cm_below, d$Density_Km22, 
       main=unique(d$estsciname), pch=19, xlab="Length (cm)", 
       ylab="Density at Lenght (km2)")
  dev.off()
}

#summary(dat$Density_Km21)
#dat$Density_Km22<-dat$Density_Km21
#dat$Density_Km22[dat$Density_Km21==0]<-dat$HLNoAtLngtkm2[dat$Density_Km21==0]
#summary(dat$Density_Km22)

# Check Filter ---------------
summary(dat$Density_Km2)

summary(as.factor(dat$estrank))
list<-c("Species", "Subspecies")
dat$Filter[dat$estrank%in%list]<-"OK"
dat$Filter[!dat$estrank%in%list]<-"SC"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Species"]<-"LFD"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Subspecies"]<-"LFD"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Genus"]<-"SCLFD"
dat$Filter[dat$FishLength_cm_below=="0"&dat$estrank=="Family"]<-"SCLFD"
dat$Filter[dat$estsciname=="Notoscopelus kroyeri"&!dat$FishLength_cm_below=="0"]<-"OK"
dat$Filter[dat$estsciname=="Gobiidae"&!dat$FishLength_cm_below=="0"]<-"OK"
dat$Filter[dat$estsciname=="Ammodytidae"&!dat$FishLength_cm_below=="0"]<-"OK"
dat$Filter[dat$estsciname=="Gobiidae"&dat$FishLength_cm_below=="0"]<-"LFD"
dat$Filter[dat$estsciname=="Ammodytidae"&dat$FishLength_cm_below=="0"]<-"LFD"

summary(as.factor(dat$Filter))

summary(as.factor(dat$estsciname[dat$Filter=="SC"]))
summary(as.factor(dat$estsciname[dat$Filter=="SCLFD"]))
summary(as.factor(dat$estsciname[dat$Filter=="LFD"]))

# All checks now complete for fish --------------------
# next step is to remove all fish  #
# less than 2 cm                   #

smallfish<-subset(dat, FishLength_cm_below<3&!FishLength_cm_below==0,)
summary(smallfish$FishLength_cm_below)
summary(dat$FishLength_cm_below)
summary(dat$HLNoAtLngt_N)
names(dat)
summary(dat$TotalNo_N)
summary(dat$HLNoAtLngtkm2)
summary(dat$TotalNoKm2)
write.csv(smallfish, "Small_fish_removed_from_DP_12-10-2016.csv")
dat<-subset(dat, FishLength_cm_below>2|FishLength_cm_below==0,)
dat$LmaxFB

test1<-dat[which(dat$FishLength_cm_below > dat$LmaxFB*1.1),]
# 1962 observations 

test2<-dat[which(dat$FishLength_cm_below > dat$LmaxFB*1.4),]
summary(as.factor(dat$FishLength_cm_below[dat$estsciname=="Lepadogaster lepadogaster"]))

write.csv(dat, "dat_HL_16-11-2016.csv")

dat<-read.csv("dat_HL_08-11-2016.csv")
memory.size(10000000000000)
names(dat)
summary(dat$FishLength_cm_below)
summary(dat$HLNoAtLngt_N)
names(dat)

dat$X.1<-NULL
dat$X<-NULL                            
dat$AphiaID.1<-NULL
dat$LWRa<-NULL                            
dat$AphiaID.2<-NULL                  
dat$valid_name<-NULL
dat$order<-NULL                         
dat$family<-NULL
dat$genus<-NULL                           
dat$rank<-NULL
dat$LmaxFB<-NULL                          
dat$LWRa.1<-NULL
dat$LWRb<-NULL                          
dat$Lmin<-NULL
dat$Lmax1.1<-NULL                        
summary(as.factor(dat$estsciname))
names(traits)
traits$estsciname<-traits$valid_name
dat_1<-merge(dat,traits, by="estsciname")
nrow(dat)-nrow(dat_1)
dat_1$LmaxFB[dat_1$valid_name=="Torpedo nobiliana"]<-180
dat_1$LmaxFB[dat_1$valid_name=="Urophycis chuss"]<-66
dat_1$LmaxFB[dat_1$valid_name=="Howella sherborni"]<-8

test1<-dat_1[which(dat_1$FishLength_cm_below > dat_1$LmaxFB*1.1),]
# 1962 observations 

dat_1$estsciname[dat_1$estsciname=="Callionymus risso"&dat_1$FishLength_cm_below>15]<-"Callionymus"
dat_1$estrank[dat_1$estsciname=="Callionymus"]<-"Genus"
dat_1$LmaxFB[dat_1$estsciname=="Callionymus"]<-30

test2<-dat_1[which(dat_1$FishLength_cm_below > dat_1$LmaxFB*1.4),]
dat_1$FishLength_cm_below[dat_1$FishLength_cm_below > dat_1$LmaxFB*1.4]<-0
diff<-test2$newLngtClass-test2$FishLength_cm_below
summary(dat_1$FishLength_cm_below[dat_1$estsciname=="Limanda limanda"])
