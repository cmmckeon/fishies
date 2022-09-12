## 8a_source_taxomony_range_adjustments.R

## sourcable section dealing with taxonmy issues bashed on the actually fish
## knowledge of Meadhbh

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
gc()
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
gc()
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

#Aggragate to Genus/Family Level#

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
gc()
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
gc()
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
gc()
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
gc()
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
gc()
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
gc()
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
gc()
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

##Geographical Range ----------------

# Is the species in the apporperate geographica range
# all alosa agone reported by sweden are suspect should be alosa alosa 
find<-dat[dat$valid_name=="Alosa agone",]
# plot(find$ShootLong, find$ShootLat)
# points(find$ShootLong[find$Country=="SE"], find$ShootLat[find$Country=="SE"], 
#       col="blue", pch=19)
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
gc()
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

