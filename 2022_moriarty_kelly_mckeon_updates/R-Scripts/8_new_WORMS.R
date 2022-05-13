## 8_new_WORMS script

## Meadhbh's script (2016) relies on a package that is no longer maintained. Trying my own version using a more recent package. CM 2022

# This is Script 8 of 13 
# Getting species list

# LOAD PACKAGES --------------
library("worms")
library(plyr)
library(rfishbase)


# LOAD DATA ------------------------ 

length(unique(HL1$Valid_Aphia)) ## 1383 unique species
num_ids <- unique(HL1$Valid_Aphia) 

bio <- HL1
## test
#x <- wormsbyid(num_ids[1:10], verbose = TRUE, ids = FALSE, sleep_btw_chunks_in_sec = 0.01)

## gotta catch 'em all - get full taxonomic record for the 1383 aphia id records
AphiaRecords <- wormsbyid(num_ids, verbose = TRUE, ids = FALSE, sleep_btw_chunks_in_sec = 0.01)

head(AphiaRecords)
# write a table to save the information
write.csv(AphiaRecords, "biodiversity/AphiaRecords_all_species.csv")

# attach the Aphia records to the biological data 
bio$AphiaID <- bio$Valid_Aphia 
MySpecList <- merge(bio, AphiaRecords, by="AphiaID")

head(MySpecList)
# write a table to save the all the information
write.csv(MySpecList, "biodiversity/HL_tax_all_species.csv") 

# Delete non fish records from bio file (MySpecList)
MyFishList <- subset(MySpecList, phylum=="Chordata")
MyFishList$totalno <- as.numeric(MyFishList$TotalNo)

write.csv(MyFishList, "biodiversity/HL_tax_fish_species.csv") 
fish <- read.csv("biodiversity/HL_tax_fish_species.csv")

length(unique(MyFishList$AphiaID)) ## only 457 species in chordata....
#length(unique(AphiaRecords$AphiaID))

levels(MyFishList$Country)

summaryfish <- ddply(MyFishList, c( "Country", "Survey", "Quarter", "Year",
                                  "Valid_Aphia","rank","scientificname" ), 
                   summarise, sumtotalno=sum(totalno))
nospecies <- ddply(summaryfish, c("Country", "Survey", "Quarter", 
                                "rank", "Year"), summarise, 
                 countofspecies=length(Valid_Aphia) )

nospeciescountryyear <- ddply(summaryfish, c("Country", "Survey",
                                           "rank", "Year"), summarise, 
                            countofspecies=length(Valid_Aphia) )

fish <- as.data.frame(MyFishList)

# k <- ggplot(nospecies, aes(x=Year, y=countofspecies, colour = factor(Quarter))) +
#   geom_point() + facet_grid(rank ~ Country + Survey, margins=F, drop=T) 
# k
# 
# k <- ggplot(nospeciescountryyear, aes(x=Year, y=countofspecies, colour=rank, group=rank)) + geom_line() +facet_grid(rank~Survey+Country, margins=F, drop=T)
# 
# k1 <- ggplot(subset(nospecies, rank%in%"Species")) + geom_line(aes(x=Year, y=countofspecies, colour=factor(Quarter), group=Quarter, size=rank)) + 
#   facet_wrap(~Survey+Country, ncol=3, drop=T) + 
#   theme(axis.text.x = element_text(angle=90, vjust=1))
# k1
# 
# l1 <- ggplot(subset(nospeciescountryyear, rank%in%"Species")) + geom_line(aes(x=Year, y=countofspecies, colour=rank, group=rank, size=rank)) + 
#   facet_wrap(~Survey+Country, ncol=3, drop=T) + theme(axis.text.x = element_text(angle=90, vjust=1))
# l1


# Get full record information for all other surveys eg Spanish?

## package rfishbase is out of data so this doesn't work
# MyFishDistribution <- distribution("Clupea harengus")
# 
# MyFishLW <- length_weight("Clupea harengus")
# 
# Denmark83_86 <- subset(MyFishList, Year<1987 & Survey%in%"NS-IBTS" & Country%in%"DEN")
# 
# head(Denmark83_86)
# write.csv(Denmark83_86, "denmarkDATRASDATA.csv")
# herring<-species("Clupea harengus")
# tables<-docs()
# faoareas<-faoareas("Clupea harengus")


# Whats the story with the gobies in each survey? # i don't know

# names(MyFishList)
# levels(factor(MyFishList$family))
# gobies <- subset(MyFishList, family%in%"Gobiidae")
# 
# summarygobies <- ddply(gobies, c("Country", "Survey",
#                                "rank", "Year"), summarise, 
#                      countofspecies=length(Valid_Aphia) )

# # My species list 
# head(AphiaRecords)
# 
# 
# # load rfishbase lib

# 
# MyFishBaseFishList <- species(MyAphiaFishList$scientificname)
# write.csv(MyFishBaseFishList, "FishBaseSpeciesData.csv")
# MyFishBaseFishDistribution <- distribution(MyFishBaseFishList$sciname)
# MyFishBaseLengthWeight <- length_weight(MyFishBaseFishList$sciname)


# str(MyFishList)
# ## too slow won't run
# summary_fish_numbers<-ddply(MyFishList, c("Country","Survey", "Quarter", 
#                                           "Year", "Ship", "HaulNo", 
#                                           "scientificname", "Sex", "SpecVal",
#                                           "totalno"), summarise, 
#                             sumnoatlngt=sum(as.numeric(HLNoAtLngt)*as.numeric(SubFactor)))
# 
# str(summary_fish_numbers)
# match <- subset(summary_fish_numbers, 
#               sumnoatlngt=totalno, )
# nomatch_fish_numbers<-subset(summary_fish_numbers, 
#                              sumnoatlngt>totalno,
# )
# nomatch_fish_numbers<-subset(nomatch_fish_numbers, 
#                              totalno>0,
# )
# nomatch_fish_numbers1<-subset(summary_fish_numbers, 
#                               sumnoatlngt<totalno,
# )
# 
# levels(as.factor(nomatch_fish_numbers1$Survey))
# levels(as.factor(nomatch_fish_numbers$Survey))
# write.csv(nomatch_fish_numbers1, "nomatch_fish_numbers1.csv")
# write.csv(nomatch_fish_numbers, "nomatch_fish_numbers.csv")
# summaryfish<-ddply(MyFishList, c( "Country", "Survey", "Quarter", "Year",
#                                   "Valid_Aphia","rank","scientificname" ), 
#                    summarise, sumtotalno=sum(totalno))
# head(summaryfish)
# write.csv(summaryfish, "summary_of_fish_per_country_per_Year.csv")
