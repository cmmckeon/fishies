##### obtaining data for rerun of OSPAR scripts ###

## last run on: 02/02/22
#beep(sound = 1, expr = NULL) ## for when you are clicking through the script but also furiously typing in the background

### This script was run separately from the main modelling process
## such that data files obtained on this date (08/09 of June 2021)
## can be stored for reproducibility purposes. 

### script to download necessary HH and HL files from Datras
# including all available years from surveys included in Moriarty
# et al.  

# The Spanish surveys are not currently included in the data product, 
# and were not included in Moriarty et al 2017. These would require additional
# work and testing which is beyond the scope of this rerun. 

# Rockall and Scottish surveys are now split into two data files, based on changes
# to survey in 2010/2011 respectively

# BTS 7a is now included in BTS survey data folder. 


## Note on running the whole script through in one go - 
## because you are creating such heavy traffic along the web connection to the DATRAS website (vague pedestrian understanding), you get connection errors
## i.e. Error in download.file(url, destfile = tmp, quiet = TRUE, method = "curl") : 'curl' call had nonzero exit status
## using gc() helps
## but also if you shift to another connection i.e. hotspot off your phone; problem solved.

#install.packages("icesDatras")
library(icesDatras)
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")


### 08_06_2021 ###

## HH refers to information about the haul
## HL refers to information about the fish
## there are two seperate data products rproduced by this project 
## one with data on fishing activity and
## one with data on fish caught  

### NI ground fish
HH_NIGFS <- getDATRAS(record = "HH", survey = "NIGFS",
                      years = 2008:2020, quarters = c(1,2,3,4))
HL_NIGFS <- getDATRAS(record = "HL", survey = "NIGFS",
                      years = 2008:2020, quarters = c(1,2,3,4))


write.csv(HH_NIGFS, "Raw_Data/DATRAS/NIGFS/HH_data_NIGFS.csv", row.names = FALSE )
write.csv(HL_NIGFS, "Raw_Data/DATRAS/NIGFS/HL_data_NIGFS.csv", row.names = FALSE )

### French EVHOE

HH_EVHOE <- getDATRAS(record = "HH", survey = "EVHOE",
                      years = 1997:2020, quarters = c(1,2,3,4))
HL_EVHOE <- getDATRAS(record = "HL", survey = "EVHOE",
                      years = 1997:2020, quarters = c(1,2,3,4))

write.csv(HH_EVHOE, "Raw_Data/DATRAS/EVHOE/HH_data_EVHOE.csv", row.names = FALSE )
write.csv(HL_EVHOE, "Raw_Data/DATRAS/EVHOE/HL_data_EVHOE.csv", row.names = FALSE )


### France Channel Ground Fish
HH_FR <- getDATRAS(record = "HH", survey = "FR-CGFS",
                   years = 1988:2020, quarters = c(1,2,3,4))
HL_FR <- getDATRAS(record = "HL", survey = "FR-CGFS",
                   years = 1988:2020, quarters = c(1,2,3,4))

write.csv(HH_FR, "Raw_Data/DATRAS/FR-CGFS/HH_data_FRCGFS.csv", row.names = FALSE )
write.csv(HL_FR, "Raw_Data/DATRAS/FR-CGFS/HL_data_FRCGRS.csv", row.names = FALSE )

### Irish Ground Fish
HH_IE <- getDATRAS(record = "HH", survey = "IE-IGFS",
                   years = 2003:2020, quarters = c(1,2,3,4))
HL_IE <- getDATRAS(record = "HL", survey = "IE-IGFS",
                   years = 2003:2020, quarters = c(1,2,3,4))

write.csv(HH_IE, "Raw_Data/DATRAS/IE-IGFS/HH_data_IEIGFS.csv", row.names = FALSE )
write.csv(HL_IE, "Raw_Data/DATRAS/IE-IGFS/HL_data_IEIGFS.csv", row.names = FALSE )


### Scottish IBTS - now split into 2 files to reflect change in survey in 2011
HH_SWC1 <- getDATRAS(record = "HH", survey = "SWC-IBTS",
                     years = 1985:2010, quarters = c(1,2,3,4))

# HL_SWC1 <- getDATRAS(record = "HL", survey = "SWC-IBTS",
#                      years = 1985:2010, quarters = c(1,2,3,4))


## there was an error with the R download of this file so I downloaded it directly from the Datras webpage and 
## saved it in the folder below

write.csv(HH_SWC1, "Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS1.csv", row.names = FALSE )
#write.csv(HL_SWC1, "Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS1.csv", row.names = FALSE )

HH_SWC2 <- getDATRAS(record = "HH", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))
HL_SWC2 <- getDATRAS(record = "HL", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))

write.csv(HH_SWC2, "Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS2.csv", row.names = FALSE )
write.csv(HL_SWC2, "Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS2.csv", row.names = FALSE )

### Portuguese GroundFish
HH_PT <- getDATRAS(record = "HH", survey = "PT-IBTS",
                   years = 2002:2021, quarters = c(1,2,3,4))
HL_PT <- getDATRAS(record = "HL", survey = "PT-IBTS",
                   years = 2002:2021, quarters = c(1,2,3,4))

write.csv(HH_PT, "Raw_Data/DATRAS/PT-IBTS/HH_data_PTIBTS.csv", row.names = FALSE )
write.csv(HL_PT, "Raw_Data/DATRAS/PT-IBTS/HL_data_PTIBTS.csv", row.names = FALSE )

### Rockall - split into 2 files to reflect changes to survey in 2011

HH_ROC1 <-  getDATRAS(record = "HH", survey = "ROCKALL",
          years = 1999:2009, quarters = c(1,2,3,4))

HL_ROC1 <-  getDATRAS(record = "HL", survey = "ROCKALL",
                      years = 1999:2009, quarters = c(1,2,3,4))


write.csv(HH_ROC1, "Raw_Data/DATRAS/ROCKALL/HH_data_ROC1.csv", row.names = FALSE )
write.csv(HL_ROC1, "Raw_Data/DATRAS/ROCKALL/HL_data_ROC1.csv", row.names = FALSE )

HH_ROC2 <-  getDATRAS(record = "HH", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))

HL_ROC2 <-  getDATRAS(record = "HL", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))


write.csv(HH_ROC2, "Raw_Data/DATRAS/ROCKALL/HH_data_ROC2.csv", row.names = FALSE )
write.csv(HL_ROC2, "Raw_Data/DATRAS/ROCKALL/HL_data_ROC2.csv", row.names = FALSE )


#### Beam Trawl


HH_BTS <-  getDATRAS(record = "HH", survey = "BTS",
                      years = 1985:2020, quarters = c(1,2,3,4))

# HL_BTS <-  getDATRAS(record = "HL", survey = "BTS",
#                      years = 1985:2020, quarters = c(1,2,3,4))
#curl error - download mannually.

write.csv(HH_BTS, "Raw_Data/DATRAS/BTS/HH_data_BTS.csv", row.names = FALSE )
#write.csv(HL_BTS, "Raw_Data/DATRAS/BTS/HL_data_BTS.csv", row.names = FALSE )


# Error in download of this file, so I directly downloaded it from the 
# Datras web download site
# HH_SP <- getDATRAS(record = "HH", survey = "SP-PORC",
#                    years = 2011:2020, quarters = c(1,2,3,4))
# HL_SP <- getDATRAS(record = "HL", survey = "SP-PORC",
#                    years = 2002:2020, quarters = c(1,2,3,4))

#write.csv(HH_SP, "HH_data_SP_PORC.csv", row.names = FALSE )
#write.csv(HL_SP, "HL_data_SP_PORC.csv", row.names = FALSE )



HH_NSIBTS <- getDATRAS(record = "HH", survey = "NS-IBTS",
                       years = 1965:2021, quarters = c(1,2,3,4))
gc()

HL_NSIBTS_80 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                       years = 1965:1980, quarters = c(1,2,3,4))

gc()
HL_NSIBTS_90 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 1981:1990, quarters = c(1,2,3,4))

gc()
HL_NSIBTS_00 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 1991:2000, quarters = c(1,2,3,4))

gc()
Sys.sleep(60)

HL_NSIBTS_10 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 2001:2010, quarters = c(1,2,3,4))
gc()
HL_NSIBTS_21 <- getDATRAS(record = "HL", survey = "NS-IBTS",
                          years = 2011:2021, quarters = c(1,2,3,4))
gc()
Sys.sleep(60)


HL_NSIBTS <- rbind(HL_NSIBTS_00,HL_NSIBTS_10,
                   HL_NSIBTS_21, HL_NSIBTS_80,
                   HL_NSIBTS_90)


head(HL_NSIBTS)
write.csv(HH_NSIBTS, "Raw_Data/DATRAS/NS-IBTS/HH_NSIBTS.csv")

write.csv(HL_NSIBTS, "Raw_Data/DATRAS/NS-IBTS/HL_NSIBTS.csv")
gc()

### other beam trawls included in the Datras WGBeam product for possible inclusion. 

# Sole Net surveys

HH_SNS <- getDATRAS(record = "HH", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))

HLSNS <- getDATRAS(record = "HL", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))

write.csv(HH_SNS, "Raw_Data/DATRAS/Beam_oth/HH_SNS.csv")
write.csv(HLSNS, "Raw_Data/DATRAS/Beam_oth/HL_SNS.csv")

gc()
Sys.sleep(60)

# BTS area VIII - France

HH_BT8 <- getDATRAS(record = "HH", survey = "BTS-VIII",
                    years = 2011:2020, quarters = c(1,2,3,4))

HL_BT8<- getDATRAS(record = "HL", survey = "BTS-VIII",
                   years = 2011:2020, quarters = c(1,2,3,4))

write.csv(HH_BT8, "Raw_Data/DATRAS/Beam_oth/HH_BTS8.csv")
write.csv(HL_BT8, "Raw_Data/DATRAS/Beam_oth/HL_BTS8.csv")

gc()
Sys.sleep(60)

# DYFS - Inshore Beam Trawl (young fish survey)


HH_DYFS <- getDATRAS(record = "HH", survey = "DYFS",
                    years = 2002:2020, quarters = c(1,2,3,4))

HL_DYFS<- getDATRAS(record = "HL", survey = "DYFS",
                   years = 2002:2020, quarters = c(1,2,3,4))

write.csv(HH_DYFS, "Raw_Data/DATRAS/Beam_oth/HH_DYFS.csv")
write.csv(HL_DYFS, "Raw_Data/DATRAS/Beam_oth/HL_DYFS.csv")

### Spanish Bottom Trawl surveys for reference. 

# Spanish Porcupine survey

HH_SPP <- getDATRAS(record = "HH", survey = "SP-PORC",
                     years = 2001:2020, quarters = c(1,2,3,4))

HL_SPP<- getDATRAS(record = "HL", survey = "SP-PORC",
                    years = 2001:2020, quarters = c(1,2,3,4))

write.csv(HH_SPP, "Raw_Data/DATRAS/Spain/HH_SP_PORC.csv")
write.csv(HL_SPP, "Raw_Data/DATRAS/Spain/HL_SP_PORC.csv")

gc()
Sys.sleep(60)

# Spanish North Coast

HH_SPN <- getDATRAS(record = "HH", survey = "SP-NORTH",
                    years = 1990:2020, quarters = c(1,2,3,4))

HL_SPN<- getDATRAS(record = "HL", survey = "SP-NORTH",
                   years = 1990:2020, quarters = c(1,2,3,4))

write.csv(HH_SPN, "Raw_Data/DATRAS/Spain/HH_SP_NORTH.csv")
write.csv(HL_SPN, "Raw_Data/DATRAS/Spain/HL_SP_NORTH.csv")

gc()
Sys.sleep(60)

# Spanish Gulf of Cadiz

HH_SPA <- getDATRAS(record = "HH", survey = "SP-ARSA",
                    years = 1996:2020, quarters = c(1,2,3,4))

HL_SPA<- getDATRAS(record = "HL", survey = "SP-ARSA",
                   years = 1996:2020, quarters = c(1,2,3,4))

write.csv(HH_SPA, "Raw_Data/DATRAS/Spain/HH_SP_ARSA.csv")
write.csv(HL_SPA, "Raw_Data/DATRAS/Spain/HL_SP_ARSA.csv")

gc()
beep(sound = 1, expr = NULL)
