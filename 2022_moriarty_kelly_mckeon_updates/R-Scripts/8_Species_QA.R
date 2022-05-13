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

HL1 <- readRDS("HL1.rds")
h <- readRDS("clean_HH.rds")

hauls <- rem_na_df(droplevels(h))

fish <- read.csv("biodiversity/HL_tax_fish_species.csv")


### Prepration of fish data -------------

# Check the Hauls and HL1 file match
# now select the hauls and HL files that match up given the new unique IDS
checkhaul <- unique(hauls$NewUniqueID2) ## 60076
checkhl1 <- unique(droplevels(HL1$NewUniqueID2)) ## 60413

## keep only HL rows that come from standardized HH data
HL1 <- droplevels(rem_na_df(HL1[HL1$NewUniqueID2 %in% hauls$NewUniqueID2,]))

# this gives 45919 hauls - same as HH hauls file
length(setdiff(checkhl1, checkhaul))
setdiff(checkhaul, checkhl1)


# All NewUniqueID2 match - good news as I'll need to merge both together 

# Initial Screening ----------------

# Step 1: Resolving Species unique ID is in a seperate code - requires different 
# version of R
# I need to bring in an external file of preprepared fish life history info and 
# taxonomic resolution data
#traits<-read.csv("./Raw_Data/Fish_traits_products/Species_List_Final.csv")
# This file contains information about the Classes of fish that we are interested in


# Combine relevant data 

# merge haul and biodiversity data by "NewUniqueID2"
dat <- merge(fish, hauls, by="NewUniqueID2")

# Check that all hauls are retained?
checkhaul<-unique(hauls$NewUniqueID2)
checkdat2<-unique(dat$NewUniqueID2)

# this gives 45919 hauls - same as HH hauls file
setdiff(checkdat2, checkhaul)
list<-setdiff(checkhaul, checkdat2)

# two hauls contained no fish species - remove from hauls data
hauls1 <-subset(hauls, !NewUniqueID2%in%list,)

# write a table to save the all the information
write.csv(hauls1, "Haul_Information.csv")
write.csv(dat, "all_hh_hl_fish_data.csv") 

# remove any SpecVal==0, these P/A values are rubbish - connect to the CA records
find <- subset(dat, SpecVal==0, )  ## 1515 obs

dat <- rem_na_df(droplevels(dat[dat$SpecVal != 0,]))

# select columns of interest and set up col type
keepers<-c("AphiaID","NewUniqueID2", "Survey.x","Quarter.x","Country.x",
           "Ship.x","Gear.x","StNo.x", "HaulNo.x", "Year.x","SpecCodeType",
           "SpecCode","SpecVal","Sex","TotalNo","CatIdentifier","NoMeas",  
           "SubWgt","CatCatchWgt","LngtCode","LngtClass", "HLNoAtLngt", "HaulDur",
           "DayNight","ShootLat","ShootLong","StatRec","HaulVal","StdSpecRecCode",
           "BycSpecRecCode", "DataType", "month", "DepthNew","SweptArea_wing_m_sqrd",
           "SweptArea_wing_km_sqrd", "valid_name","class",                     
           "order","family","genus","rank","FRS_Common.Name","LmaxFB","LWRa","SubFactor") 


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

dat <- dat[, which(names(dat) %nin% c("Survey.x", "Quarter.x", "Country.x", "Ship.x", "Gear.x", "StNo.x", 
                                      "HaulNo.x", "Year.x"))]


# Taxonomic ID protocol -----------------

# How many codes are species, genus, family
summary(as.factor(dat$rank))
# Class     Family      Genus      Order    Species  Subfamily   Suborder  Subphylum Subspecies 
# 2103      15351      67982         36    5847952         62        133        155       7052 

# Retain spp/subspp records that we trust
dat$estrank[dat$rank=="Species"]<-"Species"
dat$estsciname <- as.character(dat$valid_name)
dat$estAphia_Code[dat$rank=="Species"] <- dat$AphiaID[dat$rank=="Species"]
dat$SpeciesQualityCode[dat$rank=="Species"] <- "Recorded_Data"
dat$estrank[dat$rank=="Subspecies"] <- "Subspecies"
dat$estAphia_Code[dat$rank=="Subspecies"] <- dat$AphiaID[dat$rank=="Subspecies"]
dat$SpeciesQualityCode[dat$rank=="Subspecies"] <- "Recorded_Data"

# Are these the most approperiate taxonomic resolutions for these fish?
# Suborder 
suborder <- subset(dat, rank=="Suborder",)
summary(as.factor(suborder$valid_name))

# D.P: Gobioidei could be more approperiatly called Gobiidae
dat$estsciname[dat$valid_name=="Gobioidei"] <- "Gobiidae"
dat$estrank[dat$valid_name=="Gobioidei"] <- "Family"
dat$estAphia_Code[dat$estsciname=="Gobiidae"] <- 125537
dat$SpeciesQualityCode[dat$valid_name=="Gobioidei"] <- "suborder_to_family"

# M.M:What records have genus names when there is only one possible solution to species names

# Resolve famliy/genus to species ---------------------
# One species in the genus/family recoreded in survey 

## In meadhbh we trust
# all Anguillidae and Anguilla to Anguilla anguilla
dat$estsciname[dat$valid_name=="Anguillidae"]<-"Anguilla anguilla"
dat$estrank[dat$valid_name=="Anguillidae"]<-"Species"
dat$estsciname[dat$valid_name=="Anguilla"]<-"Anguilla anguilla"
dat$estrank[dat$valid_name=="Anguilla"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Anguilla anguilla"]<-126281
dat$SpeciesQualityCode[dat$valid_name=="Anguilla"]<-"Genus_to_spp"
dat$SpeciesQualityCode[dat$valid_name=="Anguillidae"]<-"Family_to_spp"
# all Conger to conger conger
dat$estsciname[dat$valid_name=="Conger"]<-"Conger conger"
dat$estrank[dat$valid_name=="Conger"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Conger conger"]<-126285
dat$SpeciesQualityCode[dat$valid_name=="Conger"]<-"Genus_to_spp"
# all Engraulis to Engraulis encrasicolus
dat$estsciname[dat$valid_name=="Engraulis"]<-"Engraulis encrasicolus"
dat$estrank[dat$valid_name=="Engraulis"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Engraulis encrasicolus"]<-126426
dat$SpeciesQualityCode[dat$valid_name=="Engraulis"]<-"Genus_to_spp"
# all Merlucciidae to Merluccius merluccius
dat$estsciname[dat$valid_name=="Merlucciidae"]<-"Merluccius merluccius"
dat$estrank[dat$valid_name=="Merlucciidae"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Merluccius merluccius"]<-126484
dat$SpeciesQualityCode[dat$valid_name=="Merlucciidae"]<-"Family_to_spp"
# all Caproidaeto Capros aper
dat$estsciname[dat$valid_name=="Caproidae"]<-"Capros aper"
dat$estrank[dat$valid_name=="Caproidae"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Capros aper"]<-127419
dat$SpeciesQualityCode[dat$valid_name=="Caproidae"]<-"Family_to_spp"
# all Eutrigla to Eutrigla gurnardus
dat$estsciname[dat$valid_name=="Eutrigla"]<-"Eutrigla gurnardus"
dat$estrank[dat$valid_name=="Eutrigla"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Eutrigla gurnardus"]<-150637
dat$SpeciesQualityCode[dat$valid_name=="Eutrigla"]<-"Genus_to_spp"
# all Trigla to Trigla lyra
dat$estsciname[dat$valid_name=="Trigla"]<-"Trigla lyra"
dat$estrank[dat$valid_name=="Trigla"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Trigla lyra"]<-127266
dat$SpeciesQualityCode[dat$valid_name=="Trigla"]<-"Genus_to_spp"
# all Trigloporus to Trigloporus lastoviza
dat$estsciname[dat$valid_name=="Trigloporus"]<-"Trigloporus lastoviza"
dat$estrank[dat$valid_name=="Trigloporus"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Trigloporus lastoviza"]<-154462
dat$SpeciesQualityCode[dat$valid_name=="Trigloporus"]<-"Genus_to_spp"
# all Stomias to Stomias boa boa
#find<-subset(dat, dat$genus=="Stomias",)
# Stomias boa ferox doesnt occur in North Sea so its going to be S.boa  
# for the BTS record at genus level (only 1 at genus level)
dat$estsciname[dat$valid_name=="Stomias"]<-"Stomias boa boa"
dat$estrank[dat$valid_name=="Stomias"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Stomias boa boa"]<-469202
dat$SpeciesQualityCode[dat$valid_name=="Stomias"]<-"Genus_to_spp"
# all Lampetra to Lampetra fluviatilis
 # summary(as.factor(find$valid_name))
# All the Lampetra records are gone/changed at source
dat$estsciname[dat$valid_name=="Lampetra"]<-"Lampetra fluviatilis"
dat$estrank[dat$valid_name=="Lampetra"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lampetra fluviatilis"]<-101172
dat$SpeciesQualityCode[dat$valid_name=="Lampetra"]<-"Genus_to_spp"
# all Petromyzon to Petromyzon marinus
##find<-dat[dat$genus=="Petromyzon",]
dat$estsciname[dat$valid_name=="Petromyzon"]<-"Petromyzon marinus"
dat$estrank[dat$valid_name=="Petromyzon"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Petromyzon marinus"]<-101174
dat$SpeciesQualityCode[dat$valid_name=="Petromyzon"]<-"Genus_to_spp"
# all Echiichthys to Echiichthys vipera
##find<-dat[dat$genus=="Echiichthys",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Echiichthys"]<-"Echiichthys vipera"
dat$estrank[dat$valid_name=="Echiichthys"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Echiichthys vipera"]<-150630
dat$SpeciesQualityCode[dat$valid_name=="Echiichthys"]<-"Genus_to_spp"
# all Pegusa to Pegusa lascaris
##find<-dat[dat$genus=="Pegusa",]
dat$estsciname[dat$valid_name=="Pegusa"]<-"Pegusa lascaris"
dat$estrank[dat$valid_name=="Pegusa"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Pegusa lascaris"]<-127156
dat$SpeciesQualityCode[dat$valid_name=="Pegusa"]<-"Genus_to_spp"
# Cyclopteridae to Cyclopteruslumpus
##find<-dat[dat$family=="Cyclopteridae",]
dat$estsciname[dat$valid_name=="Cyclopteridae"]<-"Cyclopterus lumpus"
dat$estrank[dat$valid_name=="Cyclopteridae"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Cyclopterus lumpus"]<-127214
dat$SpeciesQualityCode[dat$valid_name=="Cyclopteridae"]<-"Family_to_spp"
# Gonostoma  to Gonostomaelongatum
#find<-dat[dat$genus=="Gonostoma",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Gonostoma"]<-"Gonostoma elongatum"
dat$estrank[dat$valid_name=="Gonostoma"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Gonostoma elongatum"]<-127296
dat$SpeciesQualityCode[dat$valid_name=="Gonostoma"]<-"Genus_to_spp"
# Hygophum to Hygophum benoiti
#find<-dat[dat$genus=="Hygophum",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Hygophum"]<-"Hygophum benoiti"
dat$estrank[dat$valid_name=="Hygophum"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Hygophum benoiti"]<-126602
dat$SpeciesQualityCode[dat$valid_name=="Hygophum"]<-"Genus_to_spp"
# Lepadogaster to Lepadogaster lepadogaster
#find<-dat[dat$genus=="Lepadogaster",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Lepadogaster"]<-"Lepadogaster lepadogaster"
dat$estrank[dat$valid_name=="Lepadogaster"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lepadogaster lepadogaster"]<-126518
dat$SpeciesQualityCode[dat$valid_name=="Lepadogaster"]<-"Genus_to_spp"
# Lithognathus to Lithognathus mormyrus
#find<-dat[dat$genus=="Lithognathus",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Lithognathus"]<-"Lithognathus mormyrus"
dat$estrank[dat$valid_name=="Lithognathus"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lithognathus mormyrus"]<-127055
dat$SpeciesQualityCode[dat$valid_name=="Lithognathus"]<-"Genus_to_spp"
#  Lumpenus to Lumpenus lampretaeformis
# #find<-dat[dat$genus=="Lumpenus"]
# summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Lumpenus"]<-"Lumpenus lampretaeformis"
dat$estrank[dat$valid_name=="Lumpenus"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lumpenus lampretaeformis"]<-154675
dat$SpeciesQualityCode[dat$valid_name=="Lumpenus"]<-"Genus_to_spp"
#  Macroramphosus to Macroramphosus scolopax
# #find<-dat[dat$genus=="Macroramphosus"]
# summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Macroramphosus"]<-"Macroramphosus scolopax"
dat$estrank[dat$valid_name=="Macroramphosus"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Macroramphosus scolopax"]<-127378
dat$SpeciesQualityCode[dat$valid_name=="Macroramphosus"]<-"Genus_to_spp"
# Myctophum 
# #find<-dat[dat$genus=="Myctophum"]
# summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Myctophum"]<-"Myctophum punctatum"
dat$estrank[dat$valid_name=="Myctophum"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Myctophum punctatum"]<-126627
dat$SpeciesQualityCode[dat$valid_name=="Myctophum"]<-"Genus_to_spp"
#Nansenia
##find<-dat[dat$genus=="Nansenia"]
#summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Nansenia"]<-"Nansenia tenera"
dat$estrank[dat$valid_name=="Nansenia"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Nansenia tenera"]<-126729
dat$SpeciesQualityCode[dat$valid_name=="Nansenia"]<-"Genus_to_spp"
#Pholidae
##find<-dat[dat$family=="Pholidae"]
#summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Pholidae"]<-"Pholis gunnellus"
dat$estrank[dat$valid_name=="Pholidae"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Pholis gunnellus"]<-126996
dat$SpeciesQualityCode[dat$valid_name=="Pholidae"]<-"Family_to_spp"
#Triglops
##find<-dat[dat$genus=="Triglops"]
#summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Triglops"]<-"Triglops murrayi"
dat$estrank[dat$valid_name=="Triglops"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Triglops murrayi"]<-127205
dat$SpeciesQualityCode[dat$valid_name=="Triglops"]<-"Genus_to_spp"
# Pegusa
dat$estsciname[dat$valid_name=="Pegusa"]<-"Pegusa lascaris"
dat$estrank[dat$valid_name=="Pegusa"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Pegusa lascaris"]<-127156
dat$SpeciesQualityCode[dat$valid_name=="Pegusa"]<-"Genus_to_spp"
# Balistes to Balistes capriscus
#find<-dat[dat$genus=="Balistes",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Balistes"]<-"Balistes capriscus"
dat$estrank[dat$valid_name=="Balistes"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Balistes capriscus"]<-154721
dat$SpeciesQualityCode[dat$valid_name=="Balistes"]<-"Genus_to_spp"
# Benthosema to Benthosema glaciale
#find<-dat[dat$genus=="Benthosema",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Benthosema"]<-"Benthosema glaciale"
dat$estrank[dat$valid_name=="Benthosema"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Benthosema glaciale"]<-126580
dat$SpeciesQualityCode[dat$valid_name=="Benthosema"]<-"Genus_to_spp"
# Notacanthidae will be Notacanthus bonaparte
#find<-dat[dat$family=="Notacanthidae",]
summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Notacanthidae"]<-"Notacanthus bonaparte"
dat$estrank[dat$valid_name=="Notacanthidae"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Notacanthus bonaparte"]<-126642
dat$SpeciesQualityCode[dat$estsciname=="Notacanthus bonaparte"]<-"Genus_to_spp"
# Buglossidium will be Buglossidium lutem
#find<-dat[dat$genus=="Buglossidium",]
unique(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Buglossidium"]<-"Buglossidium luteum"
dat$estrank[dat$valid_name=="Buglossidium"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Buglossidium luteum"]<-127153 
dat$SpeciesQualityCode[dat$estsciname=="Buglossidium luteum"]<-"Genus_to_spp"


# Keep Genus Name -------------------

# Origionally all Lampadena to were to go to Lampadena urophaos atlantica (221500)
# But changes in the Spanish data set now no longer support that
# Retain all Lampadena at Genus level code 
# find <- rem_na_df(dat[dat$genus=="Lampadena",])
# summary(as.factor(find$valid_name))
dat$estsciname[dat$valid_name=="Lampadena"]<-"Lampadena"
dat$estrank[dat$valid_name=="Lampadena"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lampadena"]<-dat$AphiaID[dat$estsciname=="Lampadena"]
dat$SpeciesQualityCode[dat$valid_name=="Lampadena"]<-"Recorded_Data"
# Breviraja - never been identified to species level - retain as genus
# Now removed from dataset
##find<-dat[dat$genus=="Breviraja",]
#summary(as.factor(find$valid_name))
#dat$estsciname[dat$valid_name=="Breviraja"]<-"Breviraja"
#dat$estrank[dat$valid_name=="Breviraja"]<-"Genus"
#dat$estAphia_Code[dat$estsciname=="Breviraja"]<-158545
#dat$SpeciesQualityCode[dat$estsciname=="Breviraja"]<-"Genus"
# Chiasmodon - only one record in entire data set 
#find<-dat[dat$genus=="Chiasmodon",]
dat$estsciname[dat$valid_name=="Chiasmodon"]<-"Chiasmodon"
dat$estrank[dat$valid_name=="Chiasmodon"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Chiasmodon"]<-125956
dat$SpeciesQualityCode[dat$estsciname=="Chiasmodon"]<-"Recorded_Data"
# Cyclothone - only 2 records in entire data set 
dat$estsciname[dat$valid_name=="Cyclothone"]<-"Cyclothone"
dat$estrank[dat$valid_name=="Cyclothone"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Cyclothone"]<-126187
dat$SpeciesQualityCode[dat$estsciname=="Cyclothone"]<-"Recorded_Data"
# Melamphaes    no longer in database
##find<-dat[dat$genus=="Melamphaes",]
#dat$estsciname[dat$valid_name=="Melamphaes"]<-"Melamphaes"
#dat$estrank[dat$valid_name=="Melamphaes"]<-"Genus"
#dat$estAphia_Code[dat$estsciname=="Melamphaes"]<-126181
#dat$SpeciesQualityCode[dat$estsciname=="Melamphaes"]<-"Recorded_Data"
#################################
#Aggragate to Genus/Family Level#
#################################
# all sandeels to Ammodytidae
# Ammodytes, A.marinus, A.tobianus 
# Hyperoplus, H. immaculatus, H. lanceolatus
# Gymnammodytes semisquamatus   
# Gymnammodytes cicerelus
##find<-dat[dat$family=="Ammodytidae",]
summary(as.factor(find$rank))
summary(as.factor(find$valid_name))
# 19.5% needs resolved to species level BUT huge length inconsistancys meaning
# species are likeley to be poorly identified
dat$estsciname[dat$family=="Ammodytidae"]<-"Ammodytidae"
dat$estrank[dat$family=="Ammodytidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Ammodytidae"]<-125516
dat$SpeciesQualityCode[dat$estsciname=="Ammodytidae"]<-"spp/genus_to_family"
dat$SpeciesQualityCode[dat$valid_name=="Ammodytidae"]<-"Recorded_Data"
# All Gobies to Family Level
##find<-dat[dat$family=="Gobiidae"]
summary(as.factor(find$valid_name))
summary(as.factor(find$rank))

# 41.5% needs resolved to species level BUT huge length inconsistancys meaning
# species are likeley to be poorly identified
dat$estsciname[dat$family=="Gobiidae"]<-"Gobiidae"
dat$estrank[dat$family=="Gobiidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Gobiidae"]<-125537
dat$SpeciesQualityCode[dat$family=="Gobiidae"]<-"spp/genus_to_family"
dat$SpeciesQualityCode[dat$valid_name=="Gobiidae"]<-"Recorded_Data"

# Resolve family to genus  ----------------
# Minimise codes for      #
# further resolution      #

# 64 taxonomic groups which need to be assigned a species level code across all surveys
# some of these groups are 2 codes that essentially mean the same thing, 
# eg. Callionymidae and Callionymus
# no need to look at these Callionymidae seperately to the Callionymus 
# in Knn resolution later on
# Mullidae to Mullus
dat$estsciname[dat$valid_name=="Mullidae"]<-"Mullus"
dat$estrank[dat$valid_name=="Mullidae"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Mullidae"]<-126034
dat$SpeciesQualityCode[dat$estsciname=="Mullidae"]<-"Family_to_genus"
#find<-dat[is.na(dat$estrank),]
summary(find$valid_name)
# safe to assume Argentinidae=Argentina
#find<-dat[dat$family=="Argentinidae",]
summary(find$valid_name[find$Country=="SPA"])
# Glossanodon leioglossus only found in Gulf of Cadiz - all ID to spp level
# safe to assume Argentinidae=Argentina
# change all Argentinidae to Argentina
dat$estsciname[dat$valid_name=="Argentinidae"]<-"Argentina"
dat$estrank[dat$valid_name=="Argentinidae"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Argentina"]<-125885 
dat$SpeciesQualityCode[dat$valid_name=="Argentinidae"]<-"Family_to_genus"
# all raja need to be called Rajidae because of changes in the taxonomic id things
dat$estsciname[dat$valid_name=="Rajidae"]<-"Rajidae"
dat$estrank[dat$valid_name=="Rajidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Rajidae"]<-160845
dat$SpeciesQualityCode[dat$estsciname=="Rajidae"]<-"Needs_assessment"
dat$estsciname[dat$valid_name=="Raja"]<-"Rajidae"
dat$estrank[dat$valid_name=="Raja"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Rajidae"]<-160845
dat$SpeciesQualityCode[dat$valid_name=="Raja"]<-"Family_to_genus"
# Gobiesocidae 
dat$estsciname[dat$valid_name=="Gobiesocidae"]<-"Gobiesocidae"
dat$estrank[dat$valid_name=="Gobiesocidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Gobiesocidae"]<-125477
dat$SpeciesQualityCode[dat$estsciname=="Gobiesocidae"]<-"Needs_assessment"
# Liparis
dat$estsciname[dat$valid_name=="Liparis"]<-"Liparis"
dat$estrank[dat$valid_name=="Liparis"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Liparis"]<-126160
dat$SpeciesQualityCode[dat$estsciname=="Liparis"]<-"Needs_assessment"
# Labridae
dat$estsciname[dat$valid_name=="Labridae"]<-"Labridae"
dat$estrank[dat$valid_name=="Labridae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Labridae"]<-125541 
dat$SpeciesQualityCode[dat$estsciname=="Labridae"]<-"Needs_assessment"
# Squalidae is the same as Squalus
#find<-subset(dat, family=="Squalidae")
unique(find$valid_name)
dat$estsciname[dat$valid_name=="Squalidae"]<-"Squalidae"
dat$estrank[dat$valid_name=="Squalidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Squalidae"]<-105716 
dat$SpeciesQualityCode[dat$estsciname=="Squalidae"]<-"Needs_assessment"
# Bothidae will become Arnoglossus
#find<-subset(dat, family=="Bothidae")
dat$estsciname[dat$valid_name=="Bothidae"]<-"Arnoglossus"
dat$estrank[dat$valid_name=="Bothidae"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Arnoglossus"]<-126109
dat$SpeciesQualityCode[dat$valid_name=="Bothidae"]<-"Family_to_genus"
dat$estsciname[dat$valid_name=="Arnoglossus"]<-"Arnoglossus"
dat$estrank[dat$valid_name=="Arnoglossus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Arnoglossus"]<-126109
dat$SpeciesQualityCode[dat$valid_name=="Arnoglossus"]<-"Needs_assessment"
# Callionymidae will become Callionymus
dat$estsciname[dat$valid_name=="Callionymidae"]<-"Callionymus"
dat$estrank[dat$valid_name=="Callionymidae"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Callionymus"]<-125930 
dat$SpeciesQualityCode[dat$valid_name=="Callionymidae"]<-"Family_to_genus"
dat$estsciname[dat$valid_name=="Callionymus"]<-"Callionymus"
dat$estrank[dat$valid_name=="Callionymus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Callionymus"]<-125930 
dat$SpeciesQualityCode[dat$valid_name=="Callionymus"]<-"Needs_assessment"
# Alosa           
dat$estsciname[dat$valid_name=="Alosa"]<-"Alosa"
dat$estrank[dat$valid_name=="Alosa"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Alosa"]<-125715 
dat$SpeciesQualityCode[dat$estsciname=="Alosa"]<-"Needs_assessment"
# Salmo           
dat$estsciname[dat$valid_name=="Salmo"]<-"Salmo"
dat$estrank[dat$valid_name=="Salmo"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Salmo"]<-126141 
dat$SpeciesQualityCode[dat$estsciname=="Salmo"]<-"Needs_assessment"
# Petromyzontidae 
dat$estsciname[dat$valid_name=="Petromyzontidae"]<-"Petromyzontidae"
dat$estrank[dat$valid_name=="Petromyzontidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Petromyzontidae"]<-101163 
dat$SpeciesQualityCode[dat$estsciname=="Petromyzontidae"]<-"Needs_assessment"
# Argentina       
dat$estsciname[dat$valid_name=="Argentina"]<-"Argentina"
dat$estrank[dat$valid_name=="Argentina"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Argentina"]<-125885
dat$SpeciesQualityCode[dat$estsciname=="Argentina"]<-"Needs_assessment"
# Gadiculus       
dat$estsciname[dat$valid_name=="Gadiculus"]<-"Gadiculus"
dat$estrank[dat$valid_name=="Gadiculus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Gadiculus"]<-125731
dat$SpeciesQualityCode[dat$estsciname=="Gadiculus"]<-"Needs_assessment"
# Chelidonichthys 
dat$estsciname[dat$valid_name=="Chelidonichthys"]<-"Chelidonichthys"
dat$estrank[dat$valid_name=="Chelidonichthys"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Chelidonichthys"]<-126178 
dat$SpeciesQualityCode[dat$estsciname=="Chelidonichthys"]<-"Needs_assessment"
# Zeugopterus    
dat$estsciname[dat$valid_name=="Zeugopterus"]<-"Zeugopterus"
dat$estrank[dat$valid_name=="Zeugopterus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Zeugopterus"]<-126125 
dat$SpeciesQualityCode[dat$valid_name=="Zeugopterus"]<-"Needs_assessment"
#Symphodus       
dat$estsciname[dat$valid_name=="Symphodus"]<-"Symphodus"
dat$estrank[dat$valid_name=="Symphodus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Symphodus"]<-126023 
dat$SpeciesQualityCode[dat$estsciname=="Symphodus"]<-"Needs_assessment"
# Syngnathidae   
dat$estsciname[dat$valid_name=="Syngnathidae"]<-"Syngnathidae"
dat$estrank[dat$valid_name=="Syngnathidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Syngnathidae"]<-125606
dat$SpeciesQualityCode[dat$estsciname=="Syngnathidae"]<-"Needs_assessment"
# Mugilidae      
dat$estsciname[dat$valid_name=="Mugilidae"]<-"Mugilidae"
dat$estrank[dat$valid_name=="Mugilidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Mugilidae"]<-125546 
dat$SpeciesQualityCode[dat$estsciname=="Mugilidae"]<-"Needs_assessment"
# Dicentrarchus  
dat$estsciname[dat$valid_name=="Dicentrarchus"]<-"Dicentrarchus"
dat$estrank[dat$valid_name=="Dicentrarchus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Dicentrarchus"]<-126029 
dat$SpeciesQualityCode[dat$estsciname=="Dicentrarchus"]<-"Needs_assessment"
# Syngnathus      
dat$estsciname[dat$valid_name=="Syngnathus"]<-"Syngnathus"
dat$estrank[dat$valid_name=="Syngnathus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Syngnathus"]<-126227 
dat$SpeciesQualityCode[dat$estsciname=="Syngnathus"]<-"Needs_assessment"
# Mustelus      
dat$estsciname[dat$valid_name=="Mustelus"]<-"Mustelus"
dat$estrank[dat$valid_name=="Mustelus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Mustelus"]<-105732 
dat$SpeciesQualityCode[dat$estsciname=="Mustelus"]<-"Needs_assessment"
# Gaidropsarus    
dat$estsciname[dat$valid_name=="Gaidropsarus"]<-"Gaidropsarus"
dat$estrank[dat$valid_name=="Gaidropsarus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Gaidropsarus"]<-125743 
dat$SpeciesQualityCode[dat$estsciname=="Gaidropsarus"]<-"Needs_assessment"
# Mullus         
dat$estsciname[dat$valid_name=="Mullus"]<-"Mullus"
dat$estrank[dat$valid_name=="Mullus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Mullus"]<-126034 
dat$SpeciesQualityCode[dat$estsciname=="Mullus"]<-"Needs_assessment"
# Pagellus       
dat$estsciname[dat$valid_name=="Pagellus"]<-"Pagellus"
dat$estrank[dat$valid_name=="Pagellus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Pagellus"]<-126079 
dat$SpeciesQualityCode[dat$estsciname=="Pagellus"]<-"Needs_assessment"
# Myctophidae     
dat$estsciname[dat$valid_name=="Myctophidae"]<-"Myctophidae"
dat$estrank[dat$valid_name=="Myctophidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Myctophidae"]<-125498 
dat$SpeciesQualityCode[dat$estsciname=="Myctophidae"]<-"Needs_assessment"
# Diaphus         
dat$estsciname[dat$valid_name=="Diaphus"]<-"Diaphus"
dat$estrank[dat$valid_name=="Diaphus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Diaphus"]<-125819 
dat$SpeciesQualityCode[dat$estsciname=="Diaphus"]<-"Needs_assessment"
# Argyropelecus   
dat$estsciname[dat$valid_name=="Argyropelecus"]<-"Argyropelecus"
dat$estrank[dat$valid_name=="Argyropelecus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Argyropelecus"]<-126196 
dat$SpeciesQualityCode[dat$estsciname=="Argyropelecus"]<-"Needs_assessment"
# Microchirus     
dat$estsciname[dat$valid_name=="Microchirus"]<-"Microchirus"
dat$estrank[dat$valid_name=="Microchirus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Microchirus"]<-126129 
dat$SpeciesQualityCode[dat$estsciname=="Microchirus"]<-"Needs_assessment"
#Zoarcidae       
dat$estsciname[dat$valid_name=="Zoarcidae"]<-"Zoarcidae"
dat$estrank[dat$valid_name=="Zoarcidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Zoarcidae"]<-125575 
dat$SpeciesQualityCode[dat$estsciname=="Zoarcidae"]<-"Needs_assessment"
# Lampanyctus    
dat$estsciname[dat$valid_name=="Lampanyctus"]<-"Lampanyctus"
dat$estrank[dat$valid_name=="Lampanyctus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lampanyctus"]<-125825 
dat$SpeciesQualityCode[dat$estsciname=="Lampanyctus"]<-"Needs_assessment"
# change all Lampanyctus to Lampanyctus crocodilus
dat$SpeciesQualityCode[dat$estsciname=="Lampanyctus"]<-"Genus_to_spp"
dat$estsciname[dat$estsciname=="Lampanyctus"]<-"Lampanyctus crocodilus"
dat$estrank[dat$estsciname=="Lampanyctus crocodilus"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lampanyctus crocodilus"]<-126612 
# Triglidae     
dat$estsciname[dat$valid_name=="Triglidae"]<-"Triglidae"
dat$estrank[dat$valid_name=="Triglidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Triglidae"]<-125598 
dat$SpeciesQualityCode[dat$estsciname=="Triglidae"]<-"Needs_assessment"
# Gasterosteidae  
dat$estsciname[dat$valid_name=="Gasterosteidae"]<-"Gasterosteidae"
dat$estrank[dat$valid_name=="Gasterosteidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Gasterosteidae"]<-125476 
dat$SpeciesQualityCode[dat$estsciname=="Gasterosteidae"]<-"Needs_assessment"
#Sebastes      
dat$estsciname[dat$valid_name=="Sebastes"]<-"Sebastes"
dat$estrank[dat$valid_name=="Sebastes"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Sebastes"]<-126175 
dat$SpeciesQualityCode[dat$estsciname=="Sebastes"]<-"Needs_assessment"
# Soleidae      
dat$estsciname[dat$valid_name=="Soleidae"]<-"Soleidae"
dat$estrank[dat$valid_name=="Soleidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Soleidae"]<-125581 
dat$SpeciesQualityCode[dat$estsciname=="Soleidae"]<-"Needs_assessment"
# Scyliorhinus    
dat$estsciname[dat$valid_name=="Scyliorhinus"]<-"Scyliorhinus"
dat$estrank[dat$valid_name=="Scyliorhinus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Scyliorhinus"]<-105729
dat$SpeciesQualityCode[dat$estsciname=="Scyliorhinus"]<-"Needs_assessment"
#Dipturus    
dat$estsciname[dat$valid_name=="Dipturus"]<-"Dipturus"
dat$estrank[dat$valid_name=="Dipturus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Dipturus"]<-105762 
dat$SpeciesQualityCode[dat$estsciname=="Dipturus"]<-"Needs_assessment"
#Clupeidae   
dat$estsciname[dat$valid_name=="Clupeidae"]<-"Clupeidae"
dat$estrank[dat$valid_name=="Clupeidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Clupeidae"]<-125464 
dat$SpeciesQualityCode[dat$estsciname=="Clupeidae"]<-"Needs_assessment"
#Macrouridae        
dat$estsciname[dat$valid_name=="Macrouridae"]<-"Macrouridae"
dat$estrank[dat$valid_name=="Macrouridae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Macrouridae"]<-125471 
dat$SpeciesQualityCode[dat$estsciname=="Macrouridae"]<-"Needs_assessment"
# Lycodes         
dat$estsciname[dat$valid_name=="Lycodes"]<-"Lycodes"
dat$estrank[dat$valid_name=="Lycodes"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lycodes"]<-126104 
dat$SpeciesQualityCode[dat$estsciname=="Lycodes"]<-"Needs_assessment"
# Lophiidae     
dat$estsciname[dat$valid_name=="Lophiidae"]<-"Lophiidae"
dat$estrank[dat$valid_name=="Lophiidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Lophiidae"]<-125493 
dat$SpeciesQualityCode[dat$estsciname=="Lophiidae"]<-"Needs_assessment"
# Zeidae        
dat$estsciname[dat$valid_name=="Zeidae"]<-"Zeidae"
dat$estrank[dat$valid_name=="Zeidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Zeidae"]<-125617 
dat$SpeciesQualityCode[dat$estsciname=="Zeidae"]<-"Needs_assessment"
# Blenniidae     
dat$estsciname[dat$valid_name=="Blenniidae"]<-"Blenniidae"
dat$estrank[dat$valid_name=="Blenniidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Blenniidae"]<-125519
dat$SpeciesQualityCode[dat$estsciname=="Blenniidae"]<-"Needs_assessment"
# Phycidae    
dat$estsciname[dat$valid_name=="Phycidae"]<-"Phycidae"
dat$estrank[dat$valid_name=="Phycidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Phycidae"]<-125475 
dat$SpeciesQualityCode[dat$estsciname=="Phycidae"]<-"Needs_assessment"
# Sparidae       
dat$estsciname[dat$valid_name=="Sparidae"]<-"Sparidae"
dat$estrank[dat$valid_name=="Sparidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Sparidae"]<-125564 
dat$SpeciesQualityCode[dat$estsciname=="Sparidae"]<-"Needs_assessment"
# Stichaeidae        
dat$estsciname[dat$valid_name=="Stichaeidae"]<-"Stichaeidae"
dat$estrank[dat$valid_name=="Stichaeidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Stichaeidae"]<-125566 
dat$SpeciesQualityCode[dat$estsciname=="Stichaeidae"]<-"Needs_assessment"
# Cottidae        
dat$estsciname[dat$valid_name=="Cottidae"]<-"Cottidae"
dat$estrank[dat$valid_name=="Cottidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Cottidae"]<-125589 
dat$SpeciesQualityCode[dat$estsciname=="Cottidae"]<-"Needs_assessment"
#Lepidotrigla    
dat$estsciname[dat$valid_name=="Lepidotrigla"]<-"Lepidotrigla"
dat$estrank[dat$valid_name=="Lepidotrigla"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lepidotrigla"]<-126179
dat$SpeciesQualityCode[dat$estsciname=="Lepidotrigla"]<-"Needs_assessment"
# change all Lepidotrigla to Lepidotrigla dieuzeidei
#find<-dat[dat$genus=="Lepidotrigla"]
dat$SpeciesQualityCode[dat$estsciname=="Lepidotrigla"]<-"Genus_to_spp"
dat$estsciname[dat$estsciname=="Lepidotrigla"]<-"Lepidotrigla dieuzeidei"
dat$estrank[dat$estsciname=="Lepidotrigla dieuzeidei"]<-"Species"
dat$estAphia_Code[dat$estsciname=="Lepidotrigla dieuzeidei"]<-127265 
# Scorpaena   
dat$estsciname[dat$valid_name=="Scorpaena"]<-"Scorpaena"
dat$estrank[dat$valid_name=="Scorpaena"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Scorpaena"]<-125477
dat$SpeciesQualityCode[dat$estsciname=="Scorpaena"]<-"Needs_assessment"
# Scorpaenidae   
dat$estsciname[dat$valid_name=="Scorpaenidae"]<-"Scorpaenidae"
dat$estrank[dat$valid_name=="Scorpaenidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Scorpaenidae"]<-125595 
dat$SpeciesQualityCode[dat$estsciname=="Scorpaenidae"]<-"Needs_assessment"
# Congridae
dat$estsciname[dat$valid_name=="Congridae"]<-"Congridae"
dat$estrank[dat$valid_name=="Congridae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Congridae"]<-dat$AphiaID[dat$estsciname=="Congridae"]
dat$SpeciesQualityCode[dat$estsciname=="Congridae"]<-"Needs_assessment"
# Moridae
dat$estsciname[dat$valid_name=="Moridae"]<-"Moridae"
dat$estrank[dat$valid_name=="Moridae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Moridae"]<-dat$AphiaID[dat$estsciname=="Moridae"]
dat$SpeciesQualityCode[dat$estsciname=="Moridae"]<-"Needs_assessment"
# Alepocephalidae
dat$estsciname[dat$valid_name=="Alepocephalidae"]<-"Alepocephalidae"
dat$estrank[dat$valid_name=="Alepocephalidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Alepocephalidae"]<-dat$AphiaID[dat$estsciname=="Alepocephalidae"]
dat$SpeciesQualityCode[dat$estsciname=="Alepocephalidae"]<-"Needs_assessment"
# Centrolophidae
dat$estsciname[dat$valid_name=="Centrolophidae"]<-"Centrolophidae"
dat$estrank[dat$valid_name=="Centrolophidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Centrolophidae"]<-dat$AphiaID[dat$estsciname=="Centrolophidae"]
dat$SpeciesQualityCode[dat$estsciname=="Centrolophidae"]<-"Needs_assessment"
# Lophius
dat$estsciname[dat$valid_name=="Lophius"]<-"Lophius"
dat$estrank[dat$valid_name=="Lophius"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lophius"]<-dat$AphiaID[dat$estsciname=="Lophius"]
dat$SpeciesQualityCode[dat$estsciname=="Lophius"]<-"Needs_assessment"
#Notoscopelus
dat$estsciname[dat$valid_name=="Notoscopelus"]<-"Notoscopelus"
dat$estrank[dat$valid_name=="Notoscopelus"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Notoscopelus"]<-dat$AphiaID[dat$estsciname=="Notoscopelus"]
dat$SpeciesQualityCode[dat$estsciname=="Notoscopelus"]<-"Needs_assessment"
# Cyclothone
dat$estsciname[dat$valid_name=="Cyclothone"]<-"Cyclothone"
dat$estrank[dat$valid_name=="Cyclothone"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Cyclothone"]<-dat$AphiaID[dat$estsciname=="Cyclothone"]
dat$SpeciesQualityCode[dat$estsciname=="Cyclothone"]<-"Needs_assessment"
# Abramis     
dat$estsciname[dat$valid_name=="Abramis"]<-"Abramis"
dat$estrank[dat$valid_name=="Abramis"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Abramis"]<-dat$AphiaID[dat$estsciname=="Abramis"]
dat$SpeciesQualityCode[dat$estsciname=="Abramis"]<-"Needs_assessment"
# Lobianchia      
dat$estsciname[dat$valid_name=="Lobianchia"]<-"Lobianchia"
dat$estrank[dat$valid_name=="Lobianchia"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Lobianchia"]<-dat$AphiaID[dat$estsciname=="Lobianchia"]
dat$SpeciesQualityCode[dat$estsciname=="Lobianchia"]<-"Needs_assessment"
# Liparidae   
dat$estsciname[dat$valid_name=="Liparidae"]<-"Liparidae"
dat$estrank[dat$valid_name=="Liparidae"]<-"Family"
dat$estAphia_Code[dat$estsciname=="Liparidae"]<-dat$AphiaID[dat$estsciname=="Liparidae"]
dat$SpeciesQualityCode[dat$estsciname=="Liparidae"]<-"Needs_assessment"

summary(factor(dat$estrank))
summary(factor(dat$SpeciesQualityCode))

#saveRDS(dat, "dat_762.rds")

# 4.2.3 Geographical Range ----------------

# Is the species in the apporperate geographica range
# all alosa agone reported by sweden are suspect should be alosa alosa 
find<-dat[dat$valid_name=="Alosa agone",]
plot(find$ShootLong, find$ShootLat)
points(find$ShootLong[find$Country=="SE"], find$ShootLat[find$Country=="SE"], 
       col="blue", pch=19)
dat$estsciname[dat$valid_name=="Alosa agone"&dat$Country=="SE"]<-"Alosa alosa"
dat$estrank[dat$valid_name=="Alosa agone"&dat$Country=="SE"]<-"Species"
dat$estAphia_Code[dat$valid_name=="Alosa agone"&dat$Country=="SE"]<-126413 
dat$SpeciesQualityCode[dat$valid_name=="Alosa agone"&dat$Country=="SE"]<-"Species_changed"
# Abramis - flagged as freshwater genus
# only one record ever found in any survey its a fresh/brackish species!
# remove record
# dat <- dat
# 
# dat <- dat[dat$genus != "Abramis",]

# Flagged as Questionable Distribution 127228	Paraliparis membranaceus
#find<-subset(dat, valid_name=="Paraliparis membranaceus", )
# 37 records
# retain as its okay - WoRMS suggests it is found in EU waters
# Flagged as Questionable Distribution 126503	Urophycis chuss 
# it has got an Atlantic distribution so leave it as is
#find<-subset(dat, valid_name=="Urophycis chuss", )
# 4 records
# Flagged as Questionable Distribution Bathyraja brachyurops
#find<-subset(dat, valid_name=="Bathyraja brachyurops", )
# changed by DP
# Flagged as Questionable Distribution Scomber japonicus
#find<-subset(dat, valid_name=="Scomber japonicus", )
# changed by DP
# Flagged as Questionable Distribution Anarrhichthys ocellatus
#find<-subset(dat, valid_name=="Anarrhichthys ocellatus", )
# changed by DP
# Flagged as Questionable Distribution Hippocampus histrix
#find<-subset(dat, valid_name=="Hippocampus histrix", )
# # changed by DP
# Flagged as Questionable Distribution Leucoraja lentiginosa
#find<-subset(dat, valid_name=="Leucoraja lentiginosa", )
# 1 record -change to Unkown ray and reassign using k-NN procedure 
# Flagged as Questionable Distribution Lycodes vahlii
dat$estsciname[dat$valid_name=="Leucoraja lentiginosa"]<-"Leucoraja"
dat$estrank[dat$valid_name=="Leucoraja"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Leucoraja"]<-105763
dat$SpeciesQualityCode[dat$valid_name=="Leucoraja lentiginosa"]<-"Spp_to_genus"

#find<-subset(dat, valid_name=="Lycodes vahlii", )
# 3853 records 
# outside of native range but habitat is suitable - no reason to change but 
# could be Lycodes gracilis
# Flagged as Questionable Distribution Benthodesmus elongatus
#find<-subset(dat, valid_name=="Benthodesmus elongatus", )
# this has got an Atlantic distribution - retain as is
# Flagged as Questionable Distribution Nezumia bairdii
#find<-subset(dat, valid_name=="Nezumia bairdii", )
# one record -  shouldn't be reporded in this region 
# put to genus level and reassign using  k-NN
dat$estsciname[dat$valid_name=="Nezumia bairdii"]<-"Nezumia"
dat$estrank[dat$valid_name=="Nezumia"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Nezumia"]<-125754
dat$SpeciesQualityCode[dat$valid_name=="Nezumia bairdii"]<-"Spp_to_genus"
# Flagged as Questionable Distribution Spratelloides lewisi
#find<-subset(dat, valid_name=="Spratelloides lewisi", )
# 20 records of a Western Central Pacific species -
# # put to genus level and reassign using  k-NN
dat$estsciname[dat$valid_name=="Spratelloides lewisi"]<-"Spratelloides"
dat$estrank[dat$valid_name=="Spratelloides lewisi"]<-"Genus"
dat$estAphia_Code[dat$estsciname=="Spratelloides"]<-125722
dat$SpeciesQualityCode[dat$valid_name=="Spratelloides lewisi"]<-"Spp_to_genus"
#find<-dat[is.na(dat$estrank)]
summary(dat$estsciname)



# Length Codes Corrections -----------------

#  Standardise DATRAS Codes
# DATRAS allows multiple formats for length (why would they do this.....?)
# There are codes that allow us to distingush the type of data recorded
# All data will be brought to a single code for each type of data
#dat <- dat
dat<-as.data.table(dat)
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

# Abundance Codes Corrections ----------------

# Deal with C data - this needs to be brought back to R
# when DataType = C  HLNoAtLngt/60 * HaulDur
# when DataType = C  TotalNo/60 * HaulDur 
# Adds 3 new Cols
# NewDataType= R ,  NewTotalNo ,  NewHLNoAtLngt
summary(as.factor(dat$DataType))
dat$NewDataType[dat$DataType=="R"]<-"R"
dat$NewDataType[dat$DataType=="C"]<-"R"
dat$NewDataType[dat$DataType=="S"]<-"RS"
summary(as.factor(dat$NewDataType))
dat$TotalNo_N<-as.numeric(dat$TotalNo)
dat$HaulDur<-as.numeric(dat$HaulDur)
summary(as.numeric(dat$TotalNo))
summary(find$SubFactor)
summary(as.factor(find$SubFactor))
# check these are all whole numbers 
#so 1 is not equal to Inf i'll just remove that also the HL Noat Lenght is 1 
# the subfactor is inf - that should be 1
dat$TotalNo[dat$NewUniqueID2=="NIGFS_2004_4_LF_62_ROT"
            &dat$valid_name=="Gobiidae"]<-1
dat$SubFactor[dat$NewUniqueID2=="NIGFS_2004_4_LF_62_ROT"
              &dat$valid_name=="Gobiidae"]<-1
dat$HLNoAtLngt_N<-as.numeric(dat$HLNoAtLngt)
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
dat$NewTotalNo[dat$DataType=="S"]<-dat$TotalNo_N[dat$DataType=="S"]
dat$NewTotalNo[dat$DataType=="C"]<-(dat$TotalNo_N[dat$DataType=="C"])/60*dat$HaulDur[dat$DataType=="C"]
dat$NewHLNoAtLngt[dat$DataType=="R"]<-dat$HLNoAtLngt_N[dat$DataType=="R"]
dat$SubFactor<-as.numeric(dat$SubFactor)
summary(dat$SubFactor)

dat$NewHLNoAtLngt[dat$DataType=="S"]<-dat$HLNoAtLngt_N[dat$DataType=="S"]*dat$SubFactor[dat$DataType=="S"]
dat$NewHLNoAtLngt[dat$DataType=="R"]<-dat$HLNoAtLngt_N[dat$DataType=="R"]*dat$SubFactor[dat$DataType=="R"]
dat$NewHLNoAtLngt[dat$DataType=="C"]<-(dat$HLNoAtLngt_N[dat$DataType=="C"])/60*dat$HaulDur[dat$DataType=="C"]
summary(as.numeric(dat$NewTotalNo))
dat$NewHLNoAtLngt[is.na(dat$HLNoAtLngt_N)]<-0
summary(as.numeric(dat$NewHLNoAtLngt))
summary(dat$NewTotalNo)
#dat$NoMeas<-as.numeric(dat$NoMeas)
summary(dat$NoMeas)
# Lets take a closer look at S cat.
names(dat)
summary(as.numeric(dat$SubFactor[dat$NewDataType=="RS"]))
summary(as.numeric(dat$NewHLNoAtLngt))
# if subfactor is 1` then S should really be R!
dat$NewDataType[dat$DataType=="S"]<-"RS"
dat$NewDataType[dat$DataType=="S"&dat$SubFactor==1]<-"R"
summary(as.factor(dat$NewDataType))
# R         NAs 
# 5885759   55066 

summary((dat$NewTotalNo))
dat$NewTotalNo[is.na(dat$NewTotalNo)]<-0
# 6524 say they have a subfactor but in reality there are more Subfactors 
# in R
dat$HaulDur<-as.numeric(dat$HaulDur)
dat$SubFactor<-as.numeric(dat$SubFactor)
subfactor_size <- subset(dat, NewDataType=="RS", ) ## no obs
summary(subfactor_size$SubFactor)
subfactor_size$Actual_Haul_Duration <- (subfactor_size$HaulDur/subfactor_size$SubFactor)  
summary(subfactor_size$Actual_Haul_Duration)
length((subfactor_size$Actual_Haul_Duration[subfactor_size$Actual_Haul_Duration>12]))
summary(subfactor_size$HaulDur)

# So the Total number is already Raised no need to multiply the Subfactor 
# but the HL_no_at_lenght isnt raised.
# based on the fact that some of the haul times of the subsampled 
# haul is only a fraction of the actual haul time, we must consider that some of 
# these hauls may be missing out on the min haul criteria and should be removed
h <- hauls1 ## hauls data for which there are fish species
smallhauls<- subset(h, HaulDur<12 , ) ## i think we've already removed these hauls
summary(dat$HaulDur)

dat$SubFactor[is.na(dat$SubFactor)]<-1
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
##find<-subset(dat, dat$NewHLNoAtLngt_round> dat$NewTotalNo_roundup,)
dat$NewHLNoAtLngt_round<-NULL
dat$NewTotalNo_roundup<-NULL
#Still some differences between total number and highter no at length measured
# what a total mess
#dat7 <- dat
dat <- as.data.frame(dat)
dat$Density_Km2<-dat$HLNoAtLngtkm2
dat$Density_Km2[which(!is.na(dat$HLNoAtLngtkm2==0))] <- dat$TotalNoKm2[which(!is.na(dat$HLNoAtLngtkm2==0))]
x <- dat[which(is.na(dat$NewHLNoAtLngt)),]
summary(dat$Density_Km2)
#find<-subset(dat, Density_Km2==0, )
# I will address theses 62 fish later on in code
#######################
# Insure Length is TL #
#######################
dat$FishLength_cm<-dat$newLngtClass
# Not all fish are measured to TL some are measured to SL, PAFL and PSCFL 
# These need converting to TL
# Fish measured to SL, PAFL and PSCFL instead of TL
dat$LengthType[dat$family=="Macrouridae"]<-"PAFL"
dat$LengthType[dat$family=="Alepocephalidae"]<-"SL"
dat$LengthType[dat$family=="Chimaeridae"]<-"PSCFL"
dat$LengthType[is.na(dat$LengthType)]<-"TL"
# EVHOE - not measuring to PAFL
# fish measured to SL, PAFL and PSCFL instead of TL
dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="EVHOE"]<-"TL"
# SWC and NS-IBTS didn't inplement PAFL measurements until 1999
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SWC-IBTS"&dat$Year<1999]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="NS-IBTS"&dat$Year<1999]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SP_ARSA"&dat$Year<2000]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SP_PORC"&dat$Year<2000]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SPNGFSC"&dat$Year<2000]<-"TL"
# Hymenocephalus italicus is TL Mindel et al 2015
# saveRDS(dat, "dat_1037.rds")
dat$LengthType[dat$estsciname=="Hymenocephalus italicus"&
                  dat$LengthType=="PAFL"]<-"TL"
dat$LengthType[dat$estsciname=="Hymenocephalus gracilis"&
                  dat$LengthType=="PAFL"]<-"TL"
# in SP-ARSA before 2003 Nezumia aequalis was measured at TL
# unique(dat$Survey)
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_ARSA"&dat$Year<2003]<-"TL"
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_ARSA"&dat$Year==2004&dat$newLngtClass>10]<-"TL"
# in SP-PORC before 2001 Nezumia aequalis was measured at TL
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_PORC"&dat$Year<2001]<-"TL"
# # in SP-PORC up to 2004 Coelorinchus caelorhincus was measured at TL
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SP_PORC"&dat$Year<2005]<-"TL"
# Coelorinchus caelorhincus
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SPNGFS"&dat$Year<2010]<-"TL"
# Coelorinchus caelorhincus
dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
                  dat$Survey=="IE-IGFS"&dat$Year<2008 & dat$newLngtClass>10]<-"TL"
# in SPNGFS up to 2009 Malacocephalus laevis was measured at TL
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SPNGFS"&dat$Year<2010]<-"TL"
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SP_ARSA"&dat$Year<2003]<-"TL"
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SP_ARSA"&dat$Year==2004&dat$newLngtClass>20]<-"TL"
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                  dat$Survey=="IE-IGFS"&dat$Year<2007]<-"TL"
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                  dat$Survey=="NS-IBTS"&dat$Year==2016]<-"TL"
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SP_PORC"&dat$Year<2005]<-"TL"
dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
                  dat$Survey=="IE-IGFS"&dat$Year<2007]<-"TL"
# dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
#                   dat$Survey=="SP_PORC"]<-"TL"
# Macrouridae Mindel et al 2015
dat$LengthType[dat$estsciname=="Macrouridae"&dat$LengthType=="PAFL"]<-"TL"
############### PAFL to TL ###############
# Total Length converter for COC (Coelorinchus caelorhincus) is *2.82
# Total Length converter for MLA (Malacocephalus laevis) is *4.57
summary(as.factor(dat$LengthType))
# need to address the PAFL and PSCFL and use an approperiate conversion factor.
# fisrt check length information as it stands
PAFL_fish<-subset(dat, LengthType=="PAFL",)
write.csv(PAFL_fish, "PAFL_fish.csv")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFF00")

ggplot(PAFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme_classic()

ggplot(PAFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~valid_name, scales="free", ncol=4)+
  theme_classic()
# look closer at Trachyrincus scabrus
find<-subset(PAFL_fish, estsciname=="Trachyrincus scabrus",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
# Trachyrincus murrayi Mindel et al 2015
dat$FishLength_cm[dat$estsciname=="Trachyrincus scabrus"&
                   dat$LengthType=="PAFL"]<-3.1*dat$newLngtClass[dat$estsciname=="Trachyrincus scabrus"&
                                                 dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
                  dat$LengthType=="PAFL"]<-"TL"
# Trachyrincus murrayi Mindel et al 2015
#dat$FishLength_cm[dat$estsciname=="Trachyrincus murrayi"&
#                     dat$LengthType=="PAFL"]<-3.1*dat$newLngtClass[dat$estsciname=="Trachyrincus murrayi"&
#                                                                       dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Trachyrincus murrayi"&
                  dat$LengthType=="PAFL"]<-"TL"
# look closer at Nezumia aequalis
find<-subset(PAFL_fish, estsciname=="Nezumia aequalis",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
# Nezumia aequalis Mindel et al 2015

dat$FishLength_cm[dat$estsciname=="Nezumia aequalis"&
                     dat$LengthType=="PAFL"]<-3.78*dat$newLngtClass[dat$estsciname=="Nezumia aequalis"&
                                                                        dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia aequalis"&
                  dat$LengthType=="PAFL"]<-"TL"
dat$FishLength_cm[dat$estsciname=="Nezumia sclerorhynchus"&
                     dat$LengthType=="PAFL"]<-3.98*dat$newLngtClass[dat$estsciname=="Nezumia sclerorhynchus"&
                                                                        dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia sclerorhynchus"&
                  dat$LengthType=="PAFL"]<-"TL"
#Nezumia sclerorhynchus
dat$FishLength_cm[dat$estsciname=="Nezumia"&
                     dat$LengthType=="PAFL"]<-3.88*dat$newLngtClass[dat$estsciname=="Nezumia"&
                                                                        dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia"&
                  dat$LengthType=="PAFL"]<-"TL"
# look closer at Coelorinchus caelorhincus
find<-subset(fish, estsciname=="Coelorinchus caelorhincus",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Coelorinchus caelorhincus"&
                     dat$LengthType=="PAFL"]<-dat$newLngtClass[dat$estsciname=="Coelorinchus caelorhincus"&
                                                                   dat$LengthType=="PAFL"]*2.82
dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
                  dat$LengthType=="PAFL"]<-"TL"
# look closer at Malacocephalus laevis
find <- subset(PAFL_fish, estsciname=="Malacocephalus laevis",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Malacocephalus laevis"&
                     dat$LengthType=="PAFL"]<-dat$newLngtClass[dat$estsciname=="Malacocephalus laevis"&
                                                                   dat$LengthType=="PAFL"]*4.57
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                  dat$LengthType=="PAFL"]<-"TL"

#Atkinson 1991
dat$FishLength_cm[dat$estsciname=="Macrourus berglax"&
                     dat$LengthType=="PAFL"]<-5.2320+2.3455*dat$newLngtClass[dat$estsciname=="Macrourus berglax"&
                                                                                 dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Macrourus berglax"&
                  dat$LengthType=="PAFL"]<-"TL"

# Atkinson 1981 -Coryphaenoides rupestris
find<-subset(PAFL_fish, estsciname=="Coryphaenoides rupestris",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Coryphaenoides rupestris"&
                     dat$LengthType=="PAFL"]<-4.7399*dat$newLngtClass[dat$estsciname=="Coryphaenoides rupestris"&
                                                                          dat$LengthType=="PAFL"]-1.6368
dat$LengthType[dat$estsciname=="Coryphaenoides rupestris"&
                  dat$LengthType=="PAFL"]<-"TL"

#Coelorinchus labiatus Mindel et al 2015
dat$FishLength_cm[dat$estsciname=="Coelorinchus labiatus"&
                     dat$LengthType=="PAFL"]<-2.5*dat$newLngtClass[dat$estsciname=="Coelorinchus labiatus"&
                                                                       dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Coelorinchus labiatus"&
                  dat$LengthType=="PAFL"]<-"TL"

summary(as.factor(dat$LengthType))
############ SL to TL############
SL_fish<-subset(dat, LengthType=="SL",)
summary(as.factor(SL_fish$estsciname))
## Xenodermichthys copei
dat$FishLength_cm[dat$estsciname=="Xenodermichthys copei"&
                     dat$LengthType=="SL"]<-1.155*dat$newLngtClass[dat$estsciname=="Xenodermichthys copei"&
                                                                               dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Xenodermichthys copei"&
                  dat$LengthType=="SL"]<-"TL"

dat$FishLength_cm[dat$estsciname=="Alepocephalus rostratus"&
                     dat$LengthType=="SL"]<-1.127*dat$newLngtClass[dat$estsciname=="Alepocephalus rostratus"&
                                                                       dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalus rostratus"&
                  dat$LengthType=="SL"]<-"TL"

dat$FishLength_cm[dat$estsciname=="Alepocephalus bairdii"&
                     dat$LengthType=="SL"]<-1.089*dat$newLngtClass[dat$estsciname=="Alepocephalus bairdii"&
                                                                       dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalus bairdii"&
                  dat$LengthType=="SL"]<-"TL"
dat$FishLength_cm[dat$estsciname=="Alepocephalidae"&
                     dat$LengthType=="SL"]<-1.123667*dat$newLngtClass[dat$estsciname=="Alepocephalidae"&
                                                                       dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalidae"&
                  dat$LengthType=="SL"]<-"TL"
#dat7 <- dat
## PSCFL to TL ---------------------- 
PSCFL_fish<-subset(dat, LengthType=="PSCFL",)
write.csv(PSCFL_fish, "PSCFL_fish_07-10-2016.csv")
summary(as.factor(PSCFL_fish$estsciname))
ggplot(PSCFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme_classic()

ggplot(PSCFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=1)+
  theme_classic()

# EVHOE measures TL not PSCFL
dat$LengthType[dat$family=="Chimaeridae"&dat$Survey=="EVHOE"]<-"TL"
dat$FishLength_cm[dat$family=="Chimaeridae"&
                     dat$LengthType=="PSCFL"]<-dat$newLngtClass[dat$family=="Chimaeridae"&
                                                                            dat$LengthType=="PSCFL"]*1.31
dat$FishLength_cm[dat$estsciname=="Hydrolagus mirabilis"&
                     dat$LengthType=="PSCFL"]<-dat$newLngtClass[dat$estsciname=="Hydrolagus mirabilis"&
                                                                            dat$LengthType=="PSCFL"]*1.28
dat$LengthType[dat$family=="Chimaeridae"&
                  dat$LengthType=="PSCFL"]<-"TL"
summary(as.factor(dat$LengthType))

find<-dat[ which(dat$FishLength_cm > dat$newLngtClass),]
summary(as.factor(find$estsciname))
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
ggplot(find, aes(x=FishLength_cm, y=Density_Km2, color=Survey, cex=1))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))

# Check LMax - step 1 -------------------

# List species records which exceed their maximum reported length
names(dat)
# Read in fish Max data
#plot(dat$LmaxFB*1.1, dat$newLngtClass, pch=19, xlim=c(1,500)
#    , ylim=c(1,500), xlab="Species LMax + 10%",
#     ylab="Species Recorded Length",
#     col=cols[dat$valid_name], main="Species Length Classes")
#abline(0, 1, col="red")
#plot(dat$MaxLngt, dat$newLngtClass, pch=19, xlim=c(1,75)
#     , ylim=c(1,75), xlab="Species LMax + 10%",
#     ylab="Species Recorded Length",
#     col=cols[dat$scientificname], main="GOV Species Length Classes")
#abline(0, 1, col="red")
# write a piece of code to capture all records that exceed lmax
summary(dat$FishLength_cm)
summary(dat$LmaxFB)
test1 <- dat[ which(dat$FishLength_cm > dat$LmaxFB*1.4),]
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
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1995_4_SCO2_20_GOV"&
                         dat$AphiaID==127153&
                         dat$newLngtClass==22]<-274304  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1995_4_SCO2_20_GOV"&
                  dat$AphiaID==127153&
                  dat$newLngtClass==22]<-"Microchirus variegatus"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1995_4_SCO2_20_GOV"&
                 dat$AphiaID==127153&dat$newLngtClass==22]<-"Species_changed(DP)"
#length change 4.Buglossidium luteum to Microchirusvariegatus
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                         dat$AphiaID==127153&
                         dat$newLngtClass==22]<-274304  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                  dat$AphiaID==127153&
                  dat$newLngtClass==22]<-"Microchirus variegatus"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2000_1_SCO2_30_GOV"&
                 dat$AphiaID==127153&dat$newLngtClass==22]<-"Species_changed(DP)"
#length change 5.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 6.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==30]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==30]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==30]<-"Species_changed(DP)"
#length change 7.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_65_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_65_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_65_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 8.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 9.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==27]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==27]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_66_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==27]<-"Species_changed(DP)"
#length change 10.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_10_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_10_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_10_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 11.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_38_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_38_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_38_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 12.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_43_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==27]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_43_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==27]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_43_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==27]<-"Species_changed(DP)"
#dat7 <- dat
#length change 13.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_53_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_53_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_SCO2_53_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 14.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==25]<-126792  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==25]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1993_4_SCO2_10_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==25]<-"Species_changed(DP)"
#length change 15.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_39_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 16.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==30]<-126792  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==30]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1994_4_SCO2_1_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==30]<-"Species_changed(DP)"
#length change 17.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1998_1_SCO3_55_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==29]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1998_1_SCO3_55_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==29]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1998_1_SCO3_55_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==29]<-"Species_changed(DP)"
#length change 18.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_49_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_49_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_49_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 19.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==24]<-126792  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==24]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2011_1_SCO3_29_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 20.Callionymus maculatus to Callionymuslyra
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2011_3_SCO3_34_GOV"&
                         dat$AphiaID==126793&
                         dat$newLngtClass==23]<-126792  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2011_3_SCO3_34_GOV"&
                  dat$AphiaID==126793&
                  dat$newLngtClass==23]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2011_3_SCO3_34_GOV"&
                 dat$AphiaID==126793&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 21.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                         dat$AphiaID==150630&
                         dat$newLngtClass==23]<-127082  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                  dat$AphiaID==150630&
                  dat$newLngtClass==23]<-"Trachinus draco"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                 dat$AphiaID==150630&dat$newLngtClass==23]<-"Species_changed(DP)"
#length change 22.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                         dat$AphiaID==150630&
                         dat$newLngtClass==24]<-127082  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                  dat$AphiaID==150630&
                  dat$newLngtClass==24]<-"Trachinus draco"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2005_1_SCO3_32_GOV"&
                 dat$AphiaID==150630&dat$newLngtClass==24]<-"Species_changed(DP)"
#length change 23.Gymnammodytes semisquamatus
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2006_4_SCO3_14_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==145]<-14.5 
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2006_4_SCO3_14_GOV"&
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
dat$QC_Length[dat$NewUniqueID2%in%list&
                 dat$AphiaID==126754]<-"Length_changed(DP)"

dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2006_4_SCO3_14_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==145]<-14.5 

dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-17 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-18
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-18.5 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-19
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-19.5 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-20 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-20.5 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-21
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_23_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-22
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==150]<-15
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==151]<-15.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==152]<-16
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==153]<-16.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==154]<-17
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==155]<-17.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==156]<-18
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==157]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==158]<-19
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==159]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==160]<-20
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==161]<-20.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==162]<-21
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==164]<-22
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2007_3_SCO3_84_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-22.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_1_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-18
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==160]<-16
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==161]<-16.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==162]<-17
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==163]<-17.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==164]<-18
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2008_3_SCO3_62_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==166]<-19
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-17
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==171]<-17.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-18
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-19
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-20
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-20.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-21
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2008_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_1_SCO3_50_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==150]<-15
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==140]<-14
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==151]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==152]<-20
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==153]<-20.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==154]<-21
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_38_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==155]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-18
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==181]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==182]<-19
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==183]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==184]<-20
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==185]<-20.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==186]<-21
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==187]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==188]<-22
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==189]<-22.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==190]<-23
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_41_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==191]<-23.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2009_3_SCO3_65_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==185]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==90]<-9
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==92]<-10
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==93]<-10.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==94]<-11
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==95]<-11.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==96]<-12
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==97]<-12.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==99]<-13.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==101]<-14.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==103]<-15.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==104]<-16
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==105]<-16.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==106]<-17
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==107]<-17.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==108]<-18
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==109]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==110]<-19
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==111]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_3_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==112]<-20
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_4_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==120]<-12
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_4_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==134]<-19
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_56_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==255]<-25.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_36_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==215]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_36_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==217]<-22.5
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_2010_1_SCO3_54_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==220]<-22
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==80]<-8
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==83]<-9.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==84]<-10
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==85]<-10.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==86]<-11
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==87]<-11.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==88]<-12
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==89]<-12.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==90]<-13
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_32_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==92]<-14
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==165]<-16.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==166]<-17
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==167]<-17.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==168]<-18
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==169]<-18.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==170]<-19
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==171]<-19.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==172]<-20
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==173]<-20.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==174]<-21
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==175]<-21.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==176]<-22
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==177]<-22.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==178]<-23
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==179]<-23.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==180]<-24
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2010_1_SCO3_33_GOV"
                  & dat$AphiaID==126754
                  & dat$LngtClass==182]<-25
#change length of Hyperoplus lanceolatus problem in cruisefile
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==540]<-27
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==690]<-28.5
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_55_GOV"
                  & dat$AphiaID==126755
                  & dat$LngtClass==490]<-26.5
#check length of Leucoraja naevus has been reuploaded
check<-dat[dat$Year==1989
            & dat$AphiaID==127139, ]
# reuploaded sucessfully 
#check 54cm Limanda limanda has been removed
check<-dat[dat$NewUniqueID2=="NS-IBTS_2015_3_SCO3_223_GOV"
            & dat$AphiaID==127139, ]
# gone  
dat7 <- dat
#check oversized Pearlside has been removed
check<-dat[dat$NewUniqueID2=="SWC-IBTS_1997_1_SCO2_13_GOV"
            & dat$AphiaID==127312, ]
# still there remove or change to Pearlfish???
#length change 21.Echiichthys vipera to Trachinus draco 
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_53_GOV"&
                         dat$AphiaID==127147&
                         dat$newLngtClass==22]<-127151 
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_53_GOV"&
                  dat$AphiaID==127147&
                  dat$newLngtClass==22]<-"Zeugopterus punctatus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1991_1_SCO2_53_GOV"&
                 dat$AphiaID==127147&dat$newLngtClass==22]<-"Species_changed(DP)"
#Both Species and length change 21.Raja clavata to D. batis 
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==150]<-105869 
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==150]<-"Dipturus batis"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_1_SCO2_15_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==198]<-"Species&Lenght_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_1992_1_SCO2_15_GOV"
                  & dat$AphiaID==105883
                  & dat$LngtClass==150]<-198
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==150]<-105869 
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==150]<-"Dipturus batis"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_27_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==205]<-"Species&Lenght_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="SWC-IBTS_1994_1_SCO2_27_GOV"
                  & dat$AphiaID==105883
                  & dat$LngtClass==150]<-205
#Species change 21.Raja clavata to D. batis 
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==207]<-105869 
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==207]<-"Dipturus batis"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2000_4_SCO3_1_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==207]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==141]<-105869 
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==141]<-"Dipturus batis"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_63_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==141]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                         dat$AphiaID==105883&
                         dat$newLngtClass==128]<-105869 
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                  dat$AphiaID==105883&
                  dat$newLngtClass==128]<-"Dipturus batis"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2009_4_SCO3_74_GOV"&
                 dat$AphiaID==105883&dat$newLngtClass==128]<-"Species_changed(DP)"
#Species change 21.Raja montagui to Raja brachyura
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==95]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==95]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1990_4_SCO2_45_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==95]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==94]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==94]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_7_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==94]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==94]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==94]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==94]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==97]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==97]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==97]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==101]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==101]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_36_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==101]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==96]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==96]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==96]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==103]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==103]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_37_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==103]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==97]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==97]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_1992_4_SCO2_40_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==97]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                         dat$AphiaID==105887&
                         dat$newLngtClass==101]<-367297  
dat$estsciname[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                  dat$AphiaID==105887&
                  dat$newLngtClass==101]<-"Raja brachyura"
dat$QC_Length[dat$NewUniqueID2=="SWC-IBTS_2013_1_SCO3_11_GOV"&
                 dat$AphiaID==105887&dat$newLngtClass==101]<-"Species_changed(DP)"
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                         dat$AphiaID==127204&
                         dat$newLngtClass==25]<-127203  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                  dat$AphiaID==127204&
                  dat$newLngtClass==25]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==25]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                         dat$AphiaID==127204&
                         dat$newLngtClass==27]<-127203  
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                  dat$AphiaID==127204&
                  dat$newLngtClass==27]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1987_1_SCO2_34_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==27]<-"Species_changed(DP)"
# Norway corrections
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==24]<-154675   
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==24]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==24]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==26]<-154675   
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==26]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==26]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==27]<-154675   
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==27]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==27]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==28]<-154675   
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==28]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==28]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                         dat$AphiaID==127072 &
                         dat$newLngtClass==29]<-154675   
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                  dat$AphiaID==127072 &
                  dat$newLngtClass==29]<-"Lumpenus lampretaeformis"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_3_MIC_556_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==29]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1996_1_MIC_5_GOV"&
                         dat$AphiaID==271564  &
                         dat$newLngtClass==70]<-105870    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1996_1_MIC_5_GOV"&
                  dat$AphiaID==271564  &
                  dat$newLngtClass==70]<-"Dipturus linteus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1996_1_MIC_5_GOV"&
                 dat$AphiaID==271564 &dat$newLngtClass==70]<-"Species_changed(DP)"
# remove record of Lycodes gracilis
dat<-dat[ !(dat$NewUniqueID2=="NS-IBTS_2006_3_JHJ_287_GOV"&
                dat$AphiaID==274100   &
                dat$newLngtClass==56),]
# remove record of Sebastes viviparus
dat<-dat[ !(dat$NewUniqueID2=="ROCKALL_2014_3_SCO3_311_GOV"&
                dat$AphiaID==127255   &
                dat$newLngtClass==50),]
# EVHOE Length Corrections
# change Species
dat$estAphia_Code[dat$NewUniqueID2=="EVHOE_2014_4_THA2_120_GOV"&
                         dat$AphiaID==272278  &
                         dat$newLngtClass==12]<-272728     
dat$estsciname[dat$NewUniqueID2=="EVHOE_2014_4_THA2_120_GOV"&
                  dat$AphiaID==272278  &
                  dat$newLngtClass==12]<-"Notoscopelus kroyeri"
dat$QC_Length[dat$NewUniqueID2=="EVHOE_2014_4_THA2_120_GOV"&
                 dat$AphiaID==272278 & 
                 dat$newLngtClass==12]<-"Species_changed(DP)"
# Change length 
check<-dat[dat$NewUniqueID2=="EVHOE_2002_4_THA2_41_GOV"&
              dat$AphiaID==126976,]
# only 2 that need changing
dat$QC_Length[dat$NewUniqueID2=="EVHOE_2002_4_THA2_41_GOV"&
                 dat$AphiaID==126976]<-"Length_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="EVHOE_2002_4_THA2_41_GOV"
                  & dat$AphiaID==126976
                  & dat$LngtClass==93]<-33 
dat$FishLength_cm[dat$NewUniqueID2=="EVHOE_2002_4_THA2_41_GOV"
                  & dat$AphiaID==126976
                  & dat$LngtClass==96]<-36 
# Change length 
check<-dat[dat$NewUniqueID2=="EVHOE_2013_4_THA2_23_GOV"&
              dat$AphiaID==126413,]
# only 1 that need changing
dat$QC_Length[dat$NewUniqueID2=="EVHOE_2013_4_THA2_23_GOV"&
                 dat$AphiaID==126413]<-"Length_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="EVHOE_2013_4_THA2_23_GOV"
                  & dat$AphiaID==126413
                  & dat$LngtClass==260]<-26 

# Change length 
check<-dat[dat$NewUniqueID2=="EVHOE_2013_4_THA2_21_GOV"&
              dat$AphiaID==126415,]
# only 1 that need changing
dat$QC_Length[dat$NewUniqueID2=="EVHOE_2013_4_THA2_21_GOV"&
                 dat$AphiaID==126415]<-"Length_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="EVHOE_2013_4_THA2_21_GOV"
                  & dat$AphiaID==126415
                  & dat$LngtClass==420]<-42
# England corrections  check
# Probably wrongly swiped on measuring board. 
# Could not delete length from FSS, as this would affect weight.
check<-dat[dat$NewUniqueID2=="BTS-VIIa_2007_3_COR_45_BT4A"&
              dat$AphiaID==127126,]
# still in database
#delete 
dat<-dat[ !(dat$NewUniqueID2=="BTS-VIIa_2007_3_COR_45_BT4A"&
                dat$AphiaID==127126   &
                dat$newLngtClass==44),]
# Probable typo. Probably SDR (spotted ray) and not SPR. 
# The catch was also sexed. Changed to SDR on FSS
# Check
check<-dat[dat$NewUniqueID2=="BTS_2009_4_CAR_55_BT4A" & 
              dat$AphiaID==126425,]
# Ship CAR already removed
check<-dat[dat$NewUniqueID2=="BTS_2003_3_COR_99_BT4A" & 
              dat$AphiaID==126445 &
              dat$newLngtClass==88,]
# Probably wrongly swiped on measuring board. 
# Correction captured previously. Deleted from FSS
# needs to be removed from dataset
dat<-dat[!(dat$NewUniqueID2=="BTS_2003_3_COR_99_BT4A" & 
               dat$AphiaID==126445 &
               dat$newLngtClass==88),]
check<-dat[dat$NewUniqueID2=="BTS_2013_3_END_90_BT4A" & 
              dat$AphiaID==127126 &
              dat$newLngtClass==43,]
dat<-dat[!(dat$NewUniqueID2=="BTS_2013_3_END_90_BT4A" & 
               dat$AphiaID==127126 &
               dat$newLngtClass==43),]

check<-dat[dat$NewUniqueID2=="BTS_2008_3_END_80_BT4A" & 
              dat$AphiaID==127153 &
              dat$newLngtClass==50,]
dat<-dat[!(dat$NewUniqueID2=="BTS_2008_3_END_80_BT4A" & 
               dat$AphiaID==127153 &
               dat$newLngtClass==50),]
check<-dat[dat$NewUniqueID2=="BTS_2010_3_END_4_BT4A" & 
              dat$AphiaID==127153,]
dat<-dat[!(dat$NewUniqueID2=="BTS_2010_3_END_4_BT4A" & 
               dat$AphiaID==127153 &
               dat$newLngtClass==38),]
# gone
check<-dat[dat$NewUniqueID2=="BTS_2002_3_COR_12_BT4A" & 
              dat$AphiaID==274304,]
dat<-dat[!(dat$NewUniqueID2=="BTS_2002_3_COR_12_BT4A" & 
               dat$AphiaID==274304 &
               dat$newLngtClass==56),]
# CGFS - errors in lmax
# yves has sent me lots of corrections 
# get these included.
# Change Alosa agone to Alosa fallax
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_53_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==46]<-126415     
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_53_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==46]<-"Alosa fallax"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_53_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==46]<-"Species_changed(DP)"
# Change Alosa agone to Alosa fallax
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_16_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==43]<-126415     
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_16_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==43]<-"Alosa fallax"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_16_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==43]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2009_1_THA2_79_GOV"&
                         dat$AphiaID==416357 &
                         dat$newLngtClass==43]<-126415     
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2009_1_THA2_79_GOV"&
                  dat$AphiaID==416357 &
                  dat$newLngtClass==43]<-"Alosa fallax"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2009_1_THA2_79_GOV"&
                 dat$AphiaID==416357&dat$newLngtClass==43]<-"Species_changed(DP)"
# change Sprattus sprattus to Clupea Harengus
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1983_1_THA_8_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==18]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1983_1_THA_8_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==18]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1983_1_THA_8_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==18]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==19.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==19.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==19.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==20]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==20]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==20]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==20.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==20.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==20.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==21]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==21]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==21]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==21.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==21.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==21.5]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==22]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==22]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==22]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==22.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==22.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==22.5]<-"Species_changed(DP)"
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==23]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==23]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==23]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==23.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==23.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==23.5]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                         dat$AphiaID==126425 &
                         dat$newLngtClass==24.5]<-126417    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                  dat$AphiaID==126425 &
                  dat$newLngtClass==24.5]<-"Clupea harengus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_17_GOV"&
                 dat$AphiaID==126425&dat$newLngtClass==24.5]<-"Species_changed(DP)"
# change Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==21]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==21]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==21]<-"Species_changed(DP)"


dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==23]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==23]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_31_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==23]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_39_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass==30]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_39_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass==30]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1992_1_THA_39_GOV"&
                 dat$AphiaID==127204&dat$newLngtClass==30]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_24_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_24_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_24_GOV"&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

check<-test[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_24_GOV"&
              dat$AphiaID==127204,]

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1995_1_THA_26_GOV"&
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
#dat7 <- dat
dat$estAphia_Code[dat$NewUniqueID2%in%list&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203    
dat$estsciname[dat$NewUniqueID2%in%list&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2%in%list&
                 dat$AphiaID>127204&dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2004_1_THA2_29_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass>24.9]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2004_1_THA2_29_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass>24.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2004_1_THA2_29_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass>24.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_41_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass>26.9]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_41_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass>26.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2006_1_THA2_41_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass>26.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2014_1_THA2_87_GOV"&
                         dat$AphiaID==127202 &
                         dat$newLngtClass==25]<-127203    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2014_1_THA2_87_GOV"&
                  dat$AphiaID==127202 &
                  dat$newLngtClass==25]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2014_1_THA2_87_GOV"&
                 dat$AphiaID>127202&dat$newLngtClass==25]<-"Species_changed(DP)"
# Nerophis ophidion - Possible confusion with Entelurus aequoreus
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_42_GOV"&
                         dat$AphiaID==127385 &
                         dat$newLngtClass==38]<-127379    
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_42_GOV"&
                  dat$AphiaID==127385 &
                  dat$newLngtClass==38]<-"Entelurus aequoreus"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2005_1_THA2_42_GOV"&
                 dat$AphiaID==127385&dat$newLngtClass==38]<-"Species_changed(DP)"

list<-c('NS-IBTS_2005_1_THA2_42_GOV', 'NS-IBTS_2005_1_THA2_44_GOV',
        'NS-IBTS_2005_1_THA2_47_GOV', 'NS-IBTS_2005_1_THA2_55_GOV',
        'NS-IBTS_2005_1_THA2_58_GOV', 'NS-IBTS_2005_1_THA2_59_GOV',
        'NS-IBTS_2005_1_THA2_67_GOV', 'NS-IBTS_2005_1_THA2_70_GOV')

dat$estAphia_Code[dat$NewUniqueID2%in%list&
                         dat$AphiaID==127385 &
                         dat$newLngtClass>35.9]<-127379    
dat$estsciname[dat$NewUniqueID2%in%list&
                  dat$AphiaID==127385 &
                  dat$newLngtClass==35.9]<-"Entelurus aequoreus"
dat$QC_Length[dat$NewUniqueID2%in%list&
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

dat$estAphia_Code[dat$NewUniqueID2%in%list&
                         dat$AphiaID==126752 &
                         dat$newLngtClass>22.9]<-126755     
dat$estsciname[dat$NewUniqueID2%in%list&
                  dat$AphiaID==126752 &
                  dat$newLngtClass>22.9]<-"Hyperoplus immaculatus"
dat$QC_Length[dat$NewUniqueID2%in%list&
                 dat$AphiaID==126752&dat$newLngtClass>22.9]<-"Species_changed(DP)"
# more Taurulus bubalis to Myoxocephalus scorpius
dat$estAphia_Code[dat$NewUniqueID2=="FR-CGFS_1997_4_GWD_1_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203
dat$estsciname[dat$NewUniqueID2=="FR-CGFS_1997_4_GWD_1_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="FR-CGFS_1997_4_GWD_1_GOV"&
                 dat$AphiaID==127203 & dat$newLngtClass>19.9]<-"Species_changed(DP)"

dat$estAphia_Code[dat$NewUniqueID2=="FR-CGFS_2005_4_GWD_11_GOV"&
                         dat$AphiaID==127204 &
                         dat$newLngtClass>19.9]<-127203
dat$estsciname[dat$NewUniqueID2=="FR-CGFS_2005_4_GWD_11_GOV"&
                  dat$AphiaID==127204 &
                  dat$newLngtClass>19.9]<-"Myoxocephalus scorpius"
dat$QC_Length[dat$NewUniqueID2=="FR-CGFS_2005_4_GWD_11_GOV"&
                 dat$AphiaID==127203 & dat$newLngtClass>19.9]<-"Species_changed(DP)"
# Liparis montagui is probably Liparis liparis 
list<-c('NS-IBTS_1999_1_THA2_3_GOV',  'NS-IBTS_2000_1_THA2_43_GOV',
        'NS-IBTS_2002_1_THA2_68_GOV', 'NS-IBTS_2002_1_THA2_69_GOV')

dat$estAphia_Code[dat$NewUniqueID2%in%list&dat$AphiaID==127220 
                       & dat$newLngtClass>13.9]<-293624 
dat$estsciname[dat$NewUniqueID2%in%list & dat$AphiaID==127220 
                & dat$newLngtClass>13.9]<-"Liparis liparis liparis"
dat$QC_Length[dat$NewUniqueID2%in%list & dat$AphiaID==127220
               & dat$newLngtClass>13.9]<-"Species_changed(DP)"
# Buglossidium luteum is probably Solea solea
dat$estAphia_Code[dat$NewUniqueID2=="NS-IBTS_2000_1_THA2_68_GOV"
                       &dat$AphiaID==127153
                       & dat$newLngtClass>19.9]<-127160 
dat$estsciname[dat$NewUniqueID2=="NS-IBTS_2000_1_THA2_68_GOV"
                & dat$AphiaID==127153
                & dat$newLngtClass>19.9]<-"Solea solea"
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2000_1_THA2_68_GOV"
               & dat$AphiaID==127153
               & dat$newLngtClass>19.9]<-"Species_changed(DP)"
# Symphodus roissali probably another Symphodus spp 
dat$estAphia_Code[dat$NewUniqueID2=="FR-CGFS_2014_4_GWD_6_GOV"
                       &dat$AphiaID==273573
                       & dat$newLngtClass>28.9]<-126023 
dat$estsciname[dat$NewUniqueID2=="FR-CGFS_2014_4_GWD_6_GOV"
                & dat$AphiaID==273573
                & dat$newLngtClass>28.9]<-"Symphodus"
dat$QC_Length[dat$NewUniqueID2=="FR-CGFS_2014_4_GWD_6_GOV"
               & dat$AphiaID==273573
               & dat$newLngtClass>28.9]<-"Species_changed_to_genus(DP)"
# two lenghts need changing 
dat$FishLength_cm[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_61_GOV"
                  & dat$AphiaID==126415
                  & dat$LngtClass==110]<-11 
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_1999_1_THA2_61_GOV"&
                 dat$AphiaID==126415&dat$LngtClass==110]<-"Length_changed(DP)"
dat$FishLength_cm[dat$NewUniqueID2=="FR-CGFS_2006_4_GWD_47_GOV"
                  & dat$AphiaID==127126
                  & dat$LngtClass==150]<-15 
dat$QC_Length[dat$NewUniqueID2=="FR-CGFS_2006_4_GWD_47_GOV"&
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
find<-dat[dat$NewUniqueID2%in%list &
             dat$AphiaID==126426]
dat$QC_Length[dat$NewUniqueID2%in%list&
                 dat$AphiaID==126426]<-"Length/10"
find<-dat[dat$NewUniqueID2=="PT-IBTS_2013_4_NOR_73_NCT" &
             dat$AphiaID==126426 & dat$newLngtClass==155]
dat$QC_Length[dat$NewUniqueID2=="PT-IBTS_2013_4_NOR_73_NCT" &
                 dat$AphiaID==126426 & dat$newLngtClass==155]<-"Length/10"
find<-subset(dat, dat$NewUniqueID2=="NS-IBTS_1995_1_WAH3_1_GOV",)
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
find <- dat[dat$NewUniqueID2%in%list &
             dat$AphiaID==126793]
# more likely to be C. Lyra at that size not C.maculatus
dat$estAphia_Code[dat$NewUniqueID2%in%list&
                         dat$AphiaID==126793 &
                         dat$newLngtClass>16]<-126792      
dat$estsciname[dat$NewUniqueID2%in%list&
                  dat$AphiaID==126793  &
                  dat$newLngtClass>16]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2%in%list&
                 dat$AphiaID==126793 &
                 dat$newLngtClass>16]<-"Species_changed(DP)"

cr <- subset(dat, AphiaID==126795,)
plot(cr$Use_Lenght_cm, cr$HLNoAtLngtkm2)
list<-c('NS-IBTS_1991_1_TRI2_20_GOV','NS-IBTS_2004_1_THA2_24_GOV',
        'IE-IGFS_2004_4_CEXP_19_GOV','NS-IBTS_1991_1_TRI2_7_GOV',
        'IE-IGFS_2005_4_CEXP_3_GOV','NS-IBTS_2004_3_HAV_314_GOV',
        'IE-IGFS_2005_4_CEXP_4_GOV','NS-IBTS_2001_3_CIR_69_GOV',
        'IE-IGFS_2005_3_CEXP_4_GOV','IE-IGFS_2005_3_CEXP_3_GOV')
find <- dat[dat$NewUniqueID2%in%list &
             dat$AphiaID==126795&dat$Use_Lenght_cm>11,]

dat$estAphia_Code[dat$NewUniqueID2%in%list&
                         dat$AphiaID==126795 &
                         dat$newLngtClass>14]<-126792      
dat$estsciname[dat$NewUniqueID2%in%list&
                  dat$AphiaID==126795  &
                  dat$newLngtClass>14]<-"Callionymus lyra"
dat$QC_Length[dat$NewUniqueID2%in%list&
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

find<-dat[dat$NewUniqueID2=="NS-IBTS_2016_1_SCO3_46_GOV" &
             dat$AphiaID==126792 & dat$newLngtClass==95]
#dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2016_1_SCO3_46_GOV" &
#                 dat$AphiaID==126792 & dat$newLngtClass==95]<-"Length_10"
find<-dat[dat$NewUniqueID2=="NS-IBTS_2000_1_WAH3_59_GOV" &
             dat$AphiaID==126928 & dat$newLngtClass==55]
dat$QC_Length[dat$NewUniqueID2=="NS-IBTS_2000_1_WAH3_59_GOV" &
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

find<-dat[dat$NewUniqueID2=="NIGFS_2007_4_CO_19_ROT" &
             dat$AphiaID==126417 & dat$newLngtClass==935]
dat$QC_Length[dat$NewUniqueID2=="NIGFS_2007_4_CO_19_ROT" &
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
#find<-subset(dat, dat$FishLength_cm_below<3,)
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
#find2<-subset(find2, !family=="Chimaeridae",)
#find2<-subset(find2, !family=="Macrouridae",)
#find2<-subset(find2, !family=="Alepocephalidae",)
#find2<-subset(find2, !family=="Searsidae",)
write.csv(find2, "Very_small_fish_11-10-2016.csv")
unique(as.factor(find2$family))
summary(as.factor(find2$SciName))
summary(find1$NewHLNoAtLngt)
check<-subset(dat, dat$NewUniqueID2=="EVHOE/1997/4/THA2/76/GOV",)

check1<-subset(dat, dat$NewUniqueID2=="EVHOE/2014/4/THA2/19/GOV")
#find<-subset(dat, dat$estsciname=="Bathysolea profundicola" &
               FishLength_cm_below<5,)
# length code error - fix lengths
dat$FishLength_cm_below[dat$estsciname=="Bathysolea profundicola" &
                               dat$FishLength_cm_below<5]<-dat$LngtClass[dat$estsciname=="Bathysolea profundicola" &
                                                                             dat$FishLength_cm_below<5]

check1<-subset(dat, dat$NewUniqueID2=="NS-IBTS/2006/1/THA2/59/GOV")
# corrections from DPS
# netherlands
dat$FishLength_cm_below[dat$estsciname=="Clupea harengus" &
                               dat$NewUniqueID2=="NS-IBTS/2000/1/TRI2/42/GOV" &
                               dat$LngtClass==20]<-20
dat$FishLength_cm_below[dat$estsciname=="Clupea harengus" &
                               dat$NewUniqueID2=="NS-IBTS/2000/1/TRI2/8/GOV" &
                               dat$LngtClass==25]<-25
dat$FishLength_cm_below[dat$estsciname=="Callionymus reticulatus" &
                               dat$NewUniqueID2=="NS-IBTS/2007/1/TRI2/5/GOV" &
                               dat$LngtClass==2]<-12

# 2 corrections from scotland 
#NS-IBTS/2009/3/SCO3/1/GOV Agonuscataphractus 1 should be 15cm
#find<-subset(dat, dat$estsciname=="Agonus cataphractus" &
               dat$NewUniqueID2=="NS-IBTS/2009/3/SCO3/1/GOV"&
               dat$FishLength_cm_below==1,)

dat$FishLength_cm_below[dat$estsciname=="Agonus cataphractus" &
                               dat$NewUniqueID2=="NS-IBTS/2009/3/SCO3/1/GOV"&
                               dat$FishLength_cm_below==1]<-15

#NS-IBTS/2001/1/SCO3/7/GOV Sardinapilchardus 1 should be 10cm
#find<-subset(dat, dat$estsciname=="Sardina pilchardus" &
               dat$NewUniqueID2=="NS-IBTS/2001/1/SCO3/7/GOV"&
               dat$FishLength_cm_below==1,)

dat$FishLength_cm_below[dat$estsciname=="Sardina pilchardus" &
                               dat$NewUniqueID2=="NS-IBTS/2001/1/SCO3/7/GOV"&
                               dat$FishLength_cm_below==1]<-10
# 3 Sweden corrections

dat$FishLength_cm_below[dat$estsciname=="Trisopterus esmarkii" &
                               dat$NewUniqueID2=="NS-IBTS/2016/1/DANS/46/GOV"&
                               dat$FishLength_cm_below==2]<-11


# NS-IBTS/1995/1/ARG/18/GOV	Platichthysflesus	should be 20

dat$FishLength_cm_below[dat$estsciname=="Platichthys flesus" &
                               dat$NewUniqueID2=="NS-IBTS/1995/1/ARG/18/GOV"&
                               dat$FishLength_cm_below==2]<-20

# NS-IBTS/2007/3/ARG/33/GOV	Scophthalmusmaximus	should be 20
dat$FishLength_cm_below[dat$estsciname=="Scophthalmus maximus" &
                               dat$NewUniqueID2=="NS-IBTS/2007/3/ARG/33/GOV"&
                               dat$FishLength_cm_below==2]<-20


# a very odd thing has happened with the use of length codes in this haul
#find<-subset(dat, dat$estsciname=="Amblyraja radiata" &
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
#find<-subset(dat, dat$estsciname=="Ciliata mustela" &
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
#find<-subset(dat, dat$estsciname=="Alosa agone" &
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

#find<-subset(dat, dat$estsciname=="Alosa alosa" &
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
#find<-subset(dat, dat$estsciname=="Clupea harengus" & 
               dat$Year==1985 & dat$Country=="NOR",)
#find<-subset(dat, dat$estsciname=="Clupea harengus" ,)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2)
#find<-subset(dat, dat$estsciname=="Zeus faber" & 
               dat$NewUniqueID2=="SWC-IBTS/2003/1/SCO3/99/GOV" ,)

#Chelidonichthysobscurus
#find<-subset(dat, dat$estsciname=="Trisopterus esmarkii" & 
               dat$Year==2013 & dat$Country=="NOR",)
plot(find$LngtClass, find$HLNoAtLngtkm2)
# really looks like some of the samples lost the 1 at the front of them

#find<-subset(dat, dat$estsciname=="Squalus acanthias" & 
               dat$NewUniqueID2=="SWC-IBTS/2007/1/SCO3/64/GOV",)

plot(find$LngtClass, find$HLNoAtLngtkm2)
# let length = 1 equal length 0 and estimate sensible length
dat$FishLength_cm_below[dat$estsciname=="Squalus acanthias" &
                               dat$LngtClass=="1"]<-0

# Rajaclavata
#find<-subset(dat, dat$estsciname=="Raja clavata" & 
               dat$NewUniqueID2=="FR-CGFS/2003/4/GWD/62/GOV",)
# estimate the lenght
# let length = 1 equal length 0 and estimate sensible length
dat$FishLength_cm_below[dat$estsciname=="Raja clavata" &
                               dat$NewUniqueID2=="FR-CGFS/2003/4/GWD/62/GOV"&
                               dat$Use_Lenght_cm=="10"]<-0


# poor cod
#find<-subset(dat, dat$estsciname=="Trisopterus minutus" & 
               dat$Survey=="ROCKALL",)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2, pch=19)

#find<-subset(dat, dat$estsciname=="Sprattus sprattus" & 
               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/15/GOV",)
plot(find$FishLength_cm_below, find$HLNoAtLngtkm2, pch=19)
# should be 12	NS-IBTS/1995/1/MIC/15/GOV
dat$FishLength_cm_below[dat$estsciname=="Sprattus sprattus" &
                               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/15/GOV"&
                               dat$Use_Lenght_cm=="1"]<-12


#range 10-39 cm, length code error	NS-IBTS/1995/1/MIC/23/GOV
dat$FishLength_cm_below[dat$estsciname=="Micromesistius poutassou" &
                               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/23/GOV"&
                               dat$Use_Lenght_cm=="1.5"]<-15
#find<-subset(dat, dat$estsciname=="Micromesistius poutassou" & 
               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/23/GOV",)


# range 13-35 cm, length code error	NS-IBTS/1995/1/MIC/31/GOV
#find<-subset(dat, dat$estsciname=="Micromesistius poutassou" & 
               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/31/GOV",)
dat$FishLength_cm_below[dat$estsciname=="Micromesistius poutassou" &
                               dat$NewUniqueID2=="NS-IBTS/1995/1/MIC/31/GOV"&
                               dat$Use_Lenght_cm=="1.5"]<-15


# Copy the working file
write.csv(dat, "dat_workingHL_11-10-2016.csv")
#
dat<-read.csv("dat_workingHL_11-10-2016.csv")
#find<-subset(dat, dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/44/GOV")

dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/44/GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-19
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/44/GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==19]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/43/GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-17
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/43/GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==17]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/42/GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-14
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/42/GOV"&
              dat$estsciname=="Spinachia spinachia"&
              dat$FishLength_cm_below==14]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/34/GOV"&
                           dat$estsciname=="Spinachia spinachia"&
                           dat$Filter=="LFD"]<-15
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2005/1/DAN2/34/GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==15]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/43/GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-18
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/43/GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==18]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/40/GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-20
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/40/GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==20]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/38/GOV"&
                         dat$estsciname=="Spinachia spinachia"&
                         dat$Filter=="LFD"]<-20
dat$Filter[dat$NewUniqueID2=="NS-IBTS/2004/3/DAN2/38/GOV"&
            dat$estsciname=="Spinachia spinachia"&
            dat$FishLength_cm_below==20]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="NIGFS/2007/4/CO/29/ROT"&
                         dat$estsciname=="Phrynorhombus norvegicus"&
                         dat$Filter=="LFD"]<-7
dat$Filter[dat$NewUniqueID2=="NIGFS/2007/4/CO/29/ROT"&
            dat$estsciname=="Phrynorhombus norvegicus"&
            dat$FishLength_cm_below==7]<-"OK"
dat$FishLength_cm_below[dat$NewUniqueID2=="EVHOE/2015/4/THA2/46/GOV"&
                         dat$estsciname=="Merlangius merlangus"&
                         dat$Filter=="LFD"]<-37
dat$Filter[dat$NewUniqueID2=="EVHOE/2015/4/THA2/46/GOV"&
            dat$estsciname=="Merlangius merlangus"&
            dat$FishLength_cm_below==37]<-"OK"
# Syngnathus rostellatus which is more likely to be S. acus based on size
list<-c("BTS/2004/3/COR/100/BT4","BTS/2006/3/COR/37/BT4","BTS/2006/3/COR/39/BT4")

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
###################
# ADD  FLITER FOR #
# BASELINE        #
###################
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
###########################
# 4.2.4 Abundance of Fish #
###########################
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

dat7<-subset(dat, !is.na(LWRa)&!is.na(LWRb)&!LWRa==0&!LWRb==0&!CatCatchWgt_per_km2==0&!HLNoAtLngtkm2==0&!FishLength_cm_below==0,)           
names(dat7)
summary(as.factor(dat7$Filter))
summary(dat7$FishLength_cm_below)
dat7$X.1<-NULL
dat7$X<-NULL
dat7$AphiaID.1<-NULL                 
dat7$LWRa<-NULL
dat7$AphiaID.2<-NULL
dat7$valid_name<-NULL
dat7$order<-NULL
dat7$family<-NULL
dat7$genus<-NULL
dat7$rank<-NULL
dat7$LmaxFB<-NULL
dat7$LWRa.1<-NULL
dat7$LWRb<-NULL
dat7$Lmin<-NULL
dat7$Lmax1.1<-NULL                        
dat7.1<-merge(dat7, traits, by="AphiaID")
names(dat7.1)
summary(dat7.1$FishLength_cm_below)
summary(dat7.1$Density_Km2)
Weight_Check<-ddply(dat7.1, c("NewUniqueID2", "Survey", "Country",
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
Weight_Check1<-ddply(dat7, c("NewUniqueID2", "Survey", "Country",
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
##############################
# Change CatchWeights to LFD #
##############################
#find<-subset(dat, dat$Filter=="CatchWeight",)
dat7<-subset(dat, !(dat$Filter=="CatchWeight"&is.na(dat$CatCatchWgt)),)
nrow(dat)-nrow(dat7)
#find<-subset(dat7, dat7$Filter=="CatchWeight",)
list<-unique(as.factor(dat7$estsciname[dat7$Filter=="CatchWeight"]))
#find<-subset(dat7, dat7$Density_Km2==0,)

meancatchweight<-ddply(summaryweight1,
                       c("Survey", "Country","Year", "estsciname"),      
                        summarise, meanAggCatCatchWgt_per_km2=mean(AggCatCatchWgt_per_km2))
meancatchweight1<-subset(meancatchweight, meancatchweight$estsciname%in%list,)
list<-unique(as.factor(dat7$Year[dat7$Filter=="CatchWeight"]))
meancatchweight1<-subset(meancatchweight1, meancatchweight1$Year%in%list,)
list<-unique(as.factor(dat7$Survey[dat7$Filter=="CatchWeight"]))
meancatchweight1<-subset(meancatchweight1, meancatchweight1$Survey%in%list,)

write.csv(find, "Catch_Weight_only_11-10-2016.csv")
write.csv(meancatchweight, "mean_catchweight-11-10-2016.csv")
#find<-subset(dat7, Filter=="CatchWeight",)
# Fix Catch weights
summary(as.factor(dat7$Filter))
ceiling(268456.3758/283215.5222)
1/0.017284
summary(dat7$Density_Km2)
dat7$Filter[dat7$Density_Km2==0]<-"CatchWeight"
summary(as.factor(dat7$Filter))
# Scyliorhinus canicula 2002
dat7$Density_Km2[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/24/BT4A"&
               dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-57.85698
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/24/BT4A"&
                 dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-57.85698
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/24/BT4A"&
                     dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-105
dat7$Filter[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/24/BT4A"&
                           dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-"0K"
# Scyliorhinus canicula
50447.2512769591/22478.960064874
2/0.0475348
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2014/4/NOR/61/NCT"&
                 dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-42.07444
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2014/4/NOR/61/NCT"&
                     dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-42.07444
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2014/4/NOR/61/NCT"&
                           dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-89
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2014/4/NOR/61/NCT"&
              dat7$estsciname=="Scyliorhinus canicula"&dat7$Filter=="CatchWeight"]<-"0K"
#Syngnathidae
7619.90139/778.508066614135
10/0.008924
dat7$Density_Km2[dat7$NewUniqueID2=="BTS/2015/3/END/55/BT4A"&
                 dat7$estsciname=="Syngnathidae"&dat7$Filter=="CatchWeight"]<-1120.574
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS/2015/3/END/55/BT4A"&
                     dat7$estsciname=="Syngnathidae"&dat7$Filter=="CatchWeight"]<-1120.574
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS/2015/3/END/55/BT4A"&
                           dat7$estsciname=="SSyngnathidae"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="BTS/2015/3/END/55/BT4A"&
              dat7$estsciname=="Syngnathidae"&dat7$Filter=="CatchWeight"]<-"SCLFD"
#Conger conger
25792.7239485665/12283.83003
2/0.0447413
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/17/NCT"&
                 dat7$estsciname=="Conger conger"&dat7$Filter=="CatchWeight"]<-44.70143
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/17/NCT"&
                     dat7$estsciname=="Conger conger"&dat7$Filter=="CatchWeight"]<-44.70143
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/17/NCT"&
                           dat7$estsciname=="Conger conger"&dat7$Filter=="CatchWeight"]<-86
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/17/NCT"&
              dat7$estsciname=="Conger conger"&dat7$Filter=="CatchWeight"]<-"0K"
# Trisopterus esmarkii  NS-IBTS/2011/3/JHJ/301/GOV
236.7954889/117938.5798
#1 fish ?
1/0.067568855
dat7$Density_Km2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/301/GOV"&
                 dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-14.79972
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/301/GOV"&
                     dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-14.79972
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/301/GOV"&
                           dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-12
dat7$Filter[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/301/GOV"&
              dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-"0K"
# Trisopterus esmarkii NS-IBTS/2011/3/JHJ/260/GOV
1380.359397/117938.5798
#1 fish?
1/0.062676914
dat7$Density_Km2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/260/GOV"&
                 dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-15.95484
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/260/GOV"&
                     dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-15.95484
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/260/GOV"&
                           dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-21
dat7$Filter[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/260/GOV"&
              dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-"0K"
# Trisopterus luscus BTS-VIIa/2002/3/COR/8/BT4A
12462.61216/35414.47696
# 1 Fish
1/0.016048
dat7$Density_Km2[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/8/BT4A"&
                 dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-62.31306
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/8/BT4A"&
                     dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-62.31306
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/8/BT4A"&
                           dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-23
dat7$Filter[dat7$NewUniqueID2=="BTS-VIIa/2002/3/COR/8/BT4A"&
              dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-"0K"
# Trisopterus luscus PT-IBTS/2013/4/NOR/47/NCT
32710.2699295729/68232.24859
1/0.0447413
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/47/NCT"&
                 dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-22.35071
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/47/NCT"&
                     dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-22.35071
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/47/NCT"&
                           dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-43
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2013/4/NOR/47/NCT"&
              dat7$estsciname=="Trisopterus luscus"&dat7$Filter=="CatchWeight"]<-"0K"
# Scomber scombrus PT-IBTS/2008/3/NOR/81/NCT
33.6845226583415/244739.3541
1/0.0223631
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2008/3/NOR/81/NCT"&
                 dat7$estsciname=="Scomber scombrus"&dat7$Filter=="CatchWeight"]<-44.71652
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2008/3/NOR/81/NCT"&
                     dat7$estsciname=="Scomber scombrus"&dat7$Filter=="CatchWeight"]<-44.71652
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2008/3/NOR/81/NCT"&
                           dat7$estsciname=="Scomber scombrus"&dat7$Filter=="CatchWeight"]<-33
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2008/3/NOR/81/NCT"&
              dat7$estsciname=="Scomber scombrus"&dat7$Filter=="CatchWeight"]<-"0K"

# Boops boops PT-IBTS/2006/3/NOR/7/NCT
117111.0489/76370.49092
2/0.0503283
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/7/NCT"&
                 dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-39.73907
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/7/NCT"&
                     dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-39.73907
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/7/NCT"&
                           dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/7/NCT"&
              dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-"0K"

# Boops boops  PT-IBTS/2010/4/NOR/12/NCT
29044.75283/116611.0045
1/0.0503283
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                 dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-19.86954
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                     dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-19.86954
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                           dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-49
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
              dat7$estsciname=="Boops boops"&dat7$Filter=="CatchWeight"]<-"0K"

# Spondyliosoma cantharus  PT-IBTS/2009/4/NOR/72/NCT
37951.14316/64202.09959
1/0.0475348
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2009/4/NOR/72/NCT"&
                 dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-21.03722
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2009/4/NOR/72/NCT"&
                     dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-21.03722
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2009/4/NOR/72/NCT"&
                           dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-54
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2009/4/NOR/72/NCT"&
              dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-"0K"

# Spondyliosoma cantharus  PT-IBTS/2010/4/NOR/12/NCT
82027.12036/64202.09959
1/0.0447413
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                 dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-22.35071
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                     dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-22.35071
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
                           dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-68
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2010/4/NOR/12/NCT"&
              dat7$estsciname=="Spondyliosoma cantharus"&dat7$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS-VIIa/2014/3/END/134/BT4A
11784.7411444142/21951.93821
1/0.01468
dat7$Density_Km2[dat7$NewUniqueID2=="BTS-VIIa/2014/3/END/134/BT4A"&
                 dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-68.11989
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS-VIIa/2014/3/END/134/BT4A"&
                     dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-68.11989
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS-VIIa/2014/3/END/134/BT4A"&
                           dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-23
dat7$Filter[dat7$NewUniqueID2=="BTS-VIIa/2014/3/END/134/BT4A"&
              dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS/2013/3/END/108/BT4A
1344.430218/32855.76347
1/0.01562
dat7$Density_Km2[dat7$NewUniqueID2=="BTS/2013/3/END/108/BT4A"&
                 dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-64.02049
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS/2013/3/END/108/BT4A"&
                     dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-64.02049
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS/2013/3/END/108/BT4A"&
                           dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-11
dat7$Filter[dat7$NewUniqueID2=="BTS/2013/3/END/108/BT4A"&
              dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-"0K"

# Trachinus draco BTS/2013/3/END/25/BT4A
5264.543301/32855.76347
1/0.015196

dat7$Density_Km2[dat7$NewUniqueID2=="BTS/2013/3/END/25/BT4A"&
                 dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-65.80679
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS/2013/3/END/25/BT4A"&
                     dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-65.80679
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS/2013/3/END/25/BT4A"&
                           dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-18
dat7$Filter[dat7$NewUniqueID2=="BTS/2013/3/END/25/BT4A"&
              dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-"0K"

# Trachinus draco  BTS/2013/3/END/2/BT4A
3584.743332/32855.76347
1/0.013948
dat7$Density_Km2[dat7$NewUniqueID2=="BTS/2013/3/END/2/BT4A"&
                 dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-71.69487
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS/2013/3/END/2/BT4A"&
                     dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-71.69487
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS/2013/3/END/2/BT4A"&
                           dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-15
dat7$Filter[dat7$NewUniqueID2=="BTS/2013/3/END/2/BT4A"&
              dat7$estsciname=="Trachinus draco"&dat7$Filter=="CatchWeight"]<-"0K"

# Lepidopus caudatus PT-IBTS/2006/3/NOR/15/NCT
317912.586/111309.5422
3/0.0503283
dat7$Density_Km2[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/15/NCT"&
                 dat7$estsciname=="Lepidopus caudatus"&dat7$Filter=="CatchWeight"]<-19.86954
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/15/NCT"&
                     dat7$estsciname=="Lepidopus caudatus"&dat7$Filter=="CatchWeight"]<-19.86954
dat7$FishLength_cm_below[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/15/NCT"&
                           dat7$estsciname=="Lepidopus caudatus"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="PT-IBTS/2006/3/NOR/15/NCT"&
              dat7$estsciname=="Lepidopus caudatus"&dat7$Filter=="CatchWeight"]<-"LFD"

# Hippoglossoides platessoides NS-IBTS/2011/3/JHJ/249/GOV
3479.014221/3564.508492
1/0.056164826

dat7$Density_Km2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/249/GOV"&
                 dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-17.80474
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/249/GOV"&
                     dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-17.80474
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/249/GOV"&
                           dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-28
dat7$Filter[dat7$NewUniqueID2=="NS-IBTS/2011/3/JHJ/249/GOV"&
              dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-"0K"

# Limanda limanda BTS/2003/3/SOL/23/BT7
1385289.75/2585.386748
536/0.0263952
dat7$Density_Km2[dat7$NewUniqueID2=="BTS/2003/3/SOL/23/BT7"&
                 dat7$estsciname=="Limanda limanda"&dat7$Filter=="CatchWeight"]<-20306.72
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="BTS/2003/3/SOL/23/BT7"&
                     dat7$estsciname=="Limanda limanda"&dat7$Filter=="CatchWeight"]<-20306.72
dat7$FishLength_cm_below[dat7$NewUniqueID2=="BTS/2003/3/SOL/23/BT7"&
                           dat7$estsciname=="Limanda limanda"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="BTS/2003/3/SOL/23/BT7"&
              dat7$estsciname=="Limanda limanda"&dat7$Filter=="CatchWeight"]<-"LFD"

1/0.04944979
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2001/4/LF/33/ROT"&
                   dat7$estsciname=="Gobiidae"&dat7$Filter=="CatchWeight"]<-20.22253
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2001/4/LF/33/ROT"&
                     dat7$estsciname=="Gobiidae"&dat7$Filter=="CatchWeight"]<-20.22253
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2001/4/LF/33/ROT"&
                           dat7$estsciname=="Gobiidae"&dat7$Filter=="CatchWeight"]<-10
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2001/4/LF/33/ROT"&
              dat7$estsciname=="Gobiidae"&dat7$Filter=="CatchWeight"]<-"0K"
1/0.04909500
dat7$Density_Km2[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                   dat7$estsciname=="Osmerus eperlanus"&dat7$Filter=="CatchWeight"]<-20.36867
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                     dat7$estsciname=="Osmerus eperlanus"&dat7$Filter=="CatchWeight"]<-20.36867
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                           dat7$estsciname=="Osmerus eperlanus"&dat7$Filter=="CatchWeight"]<-8
dat7$Filter[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
              dat7$estsciname=="Osmerus eperlanus"&dat7$Filter=="CatchWeight"]<-"0K"

dat7$Density_Km2[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                   dat7$estsciname=="Alosa agone"&dat7$Filter=="CatchWeight"]<-20.36867
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                     dat7$estsciname=="Alosa agone"&dat7$Filter=="CatchWeight"]<-20.36867
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
                           dat7$estsciname=="Alosa agone"&dat7$Filter=="CatchWeight"]<-8
dat7$Filter[dat7$NewUniqueID2=="NS-IBTS/2012/1/THA2/58/GOV"&
              dat7$estsciname=="Alosa agone"&dat7$Filter=="CatchWeight"]<-"0K"
1/0.08594431
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2004/1/LF/20/ROT"&
                   dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-11.63544
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2004/1/LF/20/ROT"&
                     dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-11.63544
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2004/1/LF/20/ROT"&
                           dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-39
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2004/1/LF/20/ROT"&
              dat7$estsciname=="Hippoglossoides platessoides"&dat7$Filter=="CatchWeight"]<-"0K"

summary(dat7$Density_Km2)
# NIGFS/2007/1/CO/10/ROT Callionymus maculatus
77/936.296272*100
8/0.07590857
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2007/1/CO/10/ROT"&
                   dat7$estsciname=="Callionymus maculatus"&dat7$Filter=="CatchWeight"]<-105.3899
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2007/1/CO/10/ROT"&
                     dat7$estsciname=="Callionymus maculatus"&dat7$Filter=="CatchWeight"]<-105.3899
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2007/1/CO/10/ROT"&
                           dat7$estsciname=="Callionymus maculatus"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2007/1/CO/10/ROT"&
              dat7$estsciname=="Callionymus maculatus"&dat7$Filter=="CatchWeight"]<-"LFD"


#NIGFS/2006/4/CO/1/ROT Sprattus sprattus
9760/678379.4712		*100
1/0.0227783
#		Sprattus sprattus
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2006/4/CO/1/ROT"&
                   dat7$estsciname=="Sprattus sprattus"&dat7$Filter=="CatchWeight"]<-43.90143
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2006/4/CO/1/ROT"&
                     dat7$estsciname=="Sprattus sprattus"&dat7$Filter=="CatchWeight"]<-43.90143
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2006/4/CO/1/ROT"&
                           dat7$estsciname=="Sprattus sprattus"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2006/4/CO/1/ROT"&
              dat7$estsciname=="Sprattus sprattus"&dat7$Filter=="CatchWeight"]<-"LFD"

#NIGFS/2003/1/LF/21/ROT Trisopterus esmarkii
34959/172535.0079*100
20/0.08768564
#2003		Trisopterus esmarkii
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2003/1/LF/21/ROT"&
                   dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-228.0875
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2003/1/LF/21/ROT"&
                     dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-228.0875
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2003/1/LF/21/ROT"&
                           dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2003/1/LF/21/ROT"&
              dat7$estsciname=="Trisopterus esmarkii"&dat7$Filter=="CatchWeight"]<-"LFD"
#NIGFS/2003/1/LF/11/ROT Platichthys flesus
11430/192406.4217*100
6/0.08140275
#2003		Platichthys flesus
dat7$Density_Km2[dat7$NewUniqueID2=="NIGFS/2003/1/LF/11/ROT"&
                   dat7$estsciname=="Platichthys flesus"&dat7$Filter=="CatchWeight"]<-73.70758
dat7$HLNoAtLngtkm2[dat7$NewUniqueID2=="NIGFS/2003/1/LF/11/ROT"&
                     dat7$estsciname=="Platichthys flesus"&dat7$Filter=="CatchWeight"]<-73.70758
dat7$FishLength_cm_below[dat7$NewUniqueID2=="NIGFS/2003/1/LF/11/ROT"&
                           dat7$estsciname=="Platichthys flesus"&dat7$Filter=="CatchWeight"]<-0
dat7$Filter[dat7$NewUniqueID2=="NIGFS/2003/1/LF/11/ROT"&
              dat7$estsciname=="Platichthys flesus"&dat7$Filter=="CatchWeight"]<-"0K"


summary(dat7$Density_Km2)
summary(dat7$HLNoAtLngtkm2)
summary(dat7$HLNoAtLngt_N)
##################################################
# Check inconistent abundances and catch weights #
##################################################
summary(dat7$fish_len_est_catch_weight)
names(dat7)
dat7$Derived_Indiv_Fish_weight_At_Lng<-dat7$LWRa*(dat7$FishLength_cm_below^dat7$LWRb)
summary(dat7$Derived_Indiv_Fish_weight_At_Lng)
dat7$Derived_Indiv_Fish_weight_At_Lng[dat7$Derived_Indiv_Fish_weight_At_Lng==0]<-NA

summary(dat7$Density_Km2)
summary(dat7$TotalNoKm2)
summary(dat7$CatCatchWgt_per_km2)
dat7$TotalNo_km2_derived_from_weight<-dat7$CatCatchWgt_per_km2/dat7$Derived_Indiv_Fish_weight_At_Lng
dat7$Difference_in_total_weight<-dat7$TotalNoKm2-dat7$TotalNo_km2_derived_from_weight
dat7$Difference_in_Density_weight<-sqrt((dat7$Density_Km2-dat7$TotalNo_km2_derived_from_weight)^2)

summary(dat7$TotalNo_km2_derived_from_weight)
summary(dat7$TotalNoKm2)
summary(dat7$Difference_in_Density_weight)
summary(dat7$Difference_in_total_weight)

boxplot(dat7$Difference_in_total_weight~dat7$Survey)

ggplot(dat7, aes(x=CatCatchWgt_per_km2, y=TotalNo_km2_derived_from_weight, colour=Survey))+
  geom_jitter()

ggplot(dat7, aes(x=TotalNoKm2, y=TotalNo_km2_derived_from_weight, colour=Survey))+
  geom_jitter()


#find<-subset(dat7, dat7$Difference_in_total_weight>1000000,)
summary(as.factor(find$Survey))
########
#find<-subset(dat7, Density_Km2==0,)
#find<-subset(dat7, dat7$HLNoAtLngtkm2>dat7$TotalNoKm2)
summary(dat7$Density_Km2)
#summary(dat7$Density_Km21)
#Weight_Check<-ddply(dat7, c("NewUniqueID2", "Survey", "Country",
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

#for (cat in unique(summaryweight1$estsciname)){
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
#Weight_Check1<-ddply(dat7, c("NewUniqueID2", "Survey", "Country",
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

#for (cat in unique(Weight_Check2$estsciname)){
  mypath <- file.path(paste("C1_Density_at_Weight_Check_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(Weight_Check2, estsciname == cat)
  plot(d$SummedCatCatchWgt, d$SummedHLNoLng, 
       main=unique(d$estsciname), pch=19, xlab="Recorded Catch Weight (g/Km2)", 
       ylab="Density (no/km2)")
  dev.off()
}

# plot Lenght frequency distributions per species per counrty per year.
for (cat in unique(dat7$estsciname)){
  mypath <- file.path(paste("C2_Length_Frequency_", cat, ".jpeg", sep = ""))
  jpeg(file=mypath)
  d <- subset(dat7, estsciname == cat)
  plot(d$FishLength_cm_below, d$Density_Km22, 
       main=unique(d$estsciname), pch=19, xlab="Length (cm)", 
       ylab="Density at Lenght (km2)")
  dev.off()
}

#summary(dat7$Density_Km21)
#dat7$Density_Km22<-dat7$Density_Km21
#dat7$Density_Km22[dat7$Density_Km21==0]<-dat7$HLNoAtLngtkm2[dat7$Density_Km21==0]
#summary(dat7$Density_Km22)
################
# Check Filter #
################
summary(dat7$Density_Km2)

summary(as.factor(dat7$estrank))
list<-c("Species", "Subspecies")
dat7$Filter[dat7$estrank%in%list]<-"OK"
dat7$Filter[!dat7$estrank%in%list]<-"SC"
dat7$Filter[dat7$FishLength_cm_below=="0"&dat7$estrank=="Species"]<-"LFD"
dat7$Filter[dat7$FishLength_cm_below=="0"&dat7$estrank=="Subspecies"]<-"LFD"
dat7$Filter[dat7$FishLength_cm_below=="0"&dat7$estrank=="Genus"]<-"SCLFD"
dat7$Filter[dat7$FishLength_cm_below=="0"&dat7$estrank=="Family"]<-"SCLFD"
dat7$Filter[dat7$estsciname=="Notoscopelus kroyeri"&!dat7$FishLength_cm_below=="0"]<-"OK"
dat7$Filter[dat7$estsciname=="Gobiidae"&!dat7$FishLength_cm_below=="0"]<-"OK"
dat7$Filter[dat7$estsciname=="Ammodytidae"&!dat7$FishLength_cm_below=="0"]<-"OK"
dat7$Filter[dat7$estsciname=="Gobiidae"&dat7$FishLength_cm_below=="0"]<-"LFD"
dat7$Filter[dat7$estsciname=="Ammodytidae"&dat7$FishLength_cm_below=="0"]<-"LFD"

summary(as.factor(dat7$Filter))

summary(as.factor(dat7$estsciname[dat7$Filter=="SC"]))
summary(as.factor(dat7$estsciname[dat7$Filter=="SCLFD"]))
summary(as.factor(dat7$estsciname[dat7$Filter=="LFD"]))
####################################
# All checks now complete for fish #
# next step is to remove all fish  #
# less than 2 cm                   #
####################################
smallfish<-subset(dat7, FishLength_cm_below<3&!FishLength_cm_below==0,)
summary(smallfish$FishLength_cm_below)
summary(dat7$FishLength_cm_below)
summary(dat7$HLNoAtLngt_N)
names(dat7)
summary(dat7$TotalNo_N)
summary(dat7$HLNoAtLngtkm2)
summary(dat7$TotalNoKm2)
write.csv(smallfish, "Small_fish_removed_from_DP_12-10-2016.csv")
dat8<-subset(dat7, FishLength_cm_below>2|FishLength_cm_below==0,)
dat8$LmaxFB

test1<-dat8[which(dat8$FishLength_cm_below > dat8$LmaxFB*1.1),]
# 1962 observations 

test2<-dat8[which(dat8$FishLength_cm_below > dat8$LmaxFB*1.4),]
summary(as.factor(dat8$FishLength_cm_below[dat8$estsciname=="Lepadogaster lepadogaster"]))

write.csv(dat8, "dat8_HL_16-11-2016.csv")

dat8<-read.csv("dat8_HL_08-11-2016.csv")
memory.size(10000000000000)
names(dat8)
summary(dat8$FishLength_cm_below)
summary(dat8$HLNoAtLngt_N)
names(dat8)

dat8$X.1<-NULL
dat8$X<-NULL                            
dat8$AphiaID.1<-NULL
dat8$LWRa<-NULL                            
dat8$AphiaID.2<-NULL                  
dat8$valid_name<-NULL
dat8$order<-NULL                         
dat8$family<-NULL
dat8$genus<-NULL                           
dat8$rank<-NULL
dat8$LmaxFB<-NULL                          
dat8$LWRa.1<-NULL
dat8$LWRb<-NULL                          
dat8$Lmin<-NULL
dat8$Lmax1.1<-NULL                        
summary(as.factor(dat8$estsciname))
names(traits)
traits$estsciname<-traits$valid_name
dat8_1<-merge(dat8,traits, by="estsciname")
nrow(dat8)-nrow(dat8_1)
dat8_1$LmaxFB[dat8_1$valid_name=="Torpedo nobiliana"]<-180
dat8_1$LmaxFB[dat8_1$valid_name=="Urophycis chuss"]<-66
dat8_1$LmaxFB[dat8_1$valid_name=="Howella sherborni"]<-8

test1<-dat8_1[which(dat8_1$FishLength_cm_below > dat8_1$LmaxFB*1.1),]
# 1962 observations 

dat8_1$estsciname[dat8_1$estsciname=="Callionymus risso"&dat8_1$FishLength_cm_below>15]<-"Callionymus"
dat8_1$estrank[dat8_1$estsciname=="Callionymus"]<-"Genus"
dat8_1$LmaxFB[dat8_1$estsciname=="Callionymus"]<-30

test2<-dat8_1[which(dat8_1$FishLength_cm_below > dat8_1$LmaxFB*1.4),]
dat8_1$FishLength_cm_below[dat8_1$FishLength_cm_below > dat8_1$LmaxFB*1.4]<-0
diff<-test2$newLngtClass-test2$FishLength_cm_below
summary(dat8_1$FishLength_cm_below[dat8_1$estsciname=="Limanda limanda"])
