# title: "1_Housekeeping.R"
# date started: "18/6/2019"
# last updated: "07/04/2022"

#beep(sound = 1, expr = NULL) ## for when you are clicking through the script but also furiously typing in the background


### script to download necessary HH and HL files from Datras

### This script was run separately from the main modelling process such that 
### data files obtained on this date (07/04/22) can be stored for reproducibility purposes. 

## there are two separate data products produced by this project 
## one with data on fishing activity and
## one with data on fish caught  
## in raw data files HH refer to information about the haul
## in raw data files HL refers to information about the fish

## Groups of surveys present in DATRAS according to (WKSAE-DATRAS) 2021

# NEA-IBTS
# NS-IBTS
# BTS


## Surveys included from DATRAS data portal

# NIGFS
# EVHOE
# FR-CGFS
# IE-IGFS
# SWC-IBTS
# SCOWCGFS
# ROCKALL
# SCOROC
# BTS
# NS-IBTS
# SNS
# BTS-VIII
# DYFS



## Note on running the whole script through in one go - 
## because you are creating such heavy traffic along the web connection to the DATRAS website (vague pedestrian understanding), you get connection errors
## i.e. Error in download.file(url, destfile = tmp, quiet = TRUE, method = "curl") : 'curl' call had nonzero exit status
## using gc() helps
## but also if you shift to another connection i.e. hotspot off your phone; problem solved.

## set up ------------
#install.packages("icesDatras")
library(icesDatras)
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")


### NIGFS (Northern Ireland Ground Fish Survey) ----------------
HH_NIGFS <- getDATRAS(record = "HH", survey = "NIGFS",
                      years = 2005:2021, quarters = c(1,2,3,4))
HL_NIGFS <- getDATRAS(record = "HL", survey = "NIGFS",
                      years = 2005:2021, quarters = c(1,2,3,4))

write.csv(HH_NIGFS, "Raw_Data/DATRAS/NIGFS/HH_data_NIGFS.csv", row.names = FALSE )
write.csv(HL_NIGFS, "Raw_Data/DATRAS/NIGFS/HL_data_NIGFS.csv", row.names = FALSE )
gc()

### EVHOE (French Southern Atlantic Bottom Trawl Survey) ------------------

HH_EVHOE <- getDATRAS(record = "HH", survey = "EVHOE",
                      years = 1997:2021, quarters = c(1,2,3,4))
HL_EVHOE <- getDATRAS(record = "HL", survey = "EVHOE",
                      years = 1997:2021, quarters = c(1,2,3,4))

write.csv(HH_EVHOE, "Raw_Data/DATRAS/EVHOE/HH_data_EVHOE.csv", row.names = FALSE )
write.csv(HL_EVHOE, "Raw_Data/DATRAS/EVHOE/HL_data_EVHOE.csv", row.names = FALSE )
gc()

### FR-CGFS (France Channel Ground Fish Survey) -----------------
HH_FR <- getDATRAS(record = "HH", survey = "FR-CGFS",
                   years = 1988:2021, quarters = c(1,2,3,4))
HL_FR <- getDATRAS(record = "HL", survey = "FR-CGFS",
                   years = 1988:2021, quarters = c(1,2,3,4))

write.csv(HH_FR, "Raw_Data/DATRAS/FR-CGFS/HH_data_FRCGFS.csv", row.names = FALSE )
write.csv(HL_FR, "Raw_Data/DATRAS/FR-CGFS/HL_data_FRCGRS.csv", row.names = FALSE )
gc()

### IE-IGFS (Irish Ground Fish Survey) --------------------
HH_IE <- getDATRAS(record = "HH", survey = "IE-IGFS",
                   years = 2003:2021, quarters = c(1,2,3,4))
HL_IE <- getDATRAS(record = "HL", survey = "IE-IGFS",
                   years = 2003:2021, quarters = c(1,2,3,4))

write.csv(HH_IE, "Raw_Data/DATRAS/IE-IGFS/HH_data_IEIGFS.csv", row.names = FALSE )
write.csv(HL_IE, "Raw_Data/DATRAS/IE-IGFS/HL_data_IEIGFS.csv", row.names = FALSE )
gc()

### SWC-IBTS (Scottish West Coast Bottom Trawl Survey) 1985 - 2010 -------------------
HH_SWC1 <- getDATRAS(record = "HH", survey = "SWC-IBTS",
                     years = 1985:2010, quarters = c(1,2,3,4))
HL_SWC1 <- getDATRAS(record = "HL", survey = "SWC-IBTS",
                     years = 1985:2010, quarters = c(1,2,3,4))

write.csv(HH_SWC1, "Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS1.csv", row.names = FALSE )
write.csv(HL_SWC1, "Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS1.csv", row.names = FALSE )
gc()

### SCOWCGFS (Scottish West Coast Ground Fish Survey) 2011 - 2021 ----------------------
HH_SWC2 <- getDATRAS(record = "HH", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))
HL_SWC2 <- getDATRAS(record = "HL", survey = "SCOWCGFS",
                     years = 2011:2021, quarters = c(1,2,3,4))

write.csv(HH_SWC2, "Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS2.csv", row.names = FALSE )
write.csv(HL_SWC2, "Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS2.csv", row.names = FALSE )
gc()

### ROCKALL (Scottish Rockall Survey) 1999 - 2009 -----------------
HH_ROC1 <-  getDATRAS(record = "HH", survey = "ROCKALL",
          years = 1999:2009, quarters = c(1,2,3,4))
HL_ROC1 <-  getDATRAS(record = "HL", survey = "ROCKALL",
                      years = 1999:2009, quarters = c(1,2,3,4))

write.csv(HH_ROC1, "Raw_Data/DATRAS/ROCKALL/HH_data_ROC1.csv", row.names = FALSE )
write.csv(HL_ROC1, "Raw_Data/DATRAS/ROCKALL/HL_data_ROC1.csv", row.names = FALSE )
gc()

### SCOROC (Scottish Rockall Survey 2011 - 2021) ------------------
HH_ROC2 <-  getDATRAS(record = "HH", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))
HL_ROC2 <-  getDATRAS(record = "HL", survey = "SCOROC",
                      years = 2011:2021, quarters = c(1,2,3,4))

write.csv(HH_ROC2, "Raw_Data/DATRAS/ROCKALL/HH_data_ROC2.csv", row.names = FALSE )
write.csv(HL_ROC2, "Raw_Data/DATRAS/ROCKALL/HL_data_ROC2.csv", row.names = FALSE )
gc()


### BTS (Beam Trawl Survey) ----------------
HH_BTS <-  getDATRAS(record = "HH", survey = "BTS",
                      years = 1985:2021, quarters = c(1,2,3,4))
# Repeated curl error in download of this next file, so I directly downloaded it from the Datras web download site
# HL_BTS <-  getDATRAS(record = "HL", survey = "BTS",
#                      years = 1985:2021, quarters = c(1,2,3,4))

write.csv(HH_BTS, "Raw_Data/DATRAS/BTS/HH_data_BTS.csv", row.names = FALSE )
#write.csv(HL_BTS, "Raw_Data/DATRAS/BTS/HL_data_BTS.csv", row.names = FALSE )
gc()



### NS-IBTS (North Sea International Bottom Trawl Survey) ---------------------
HH_NSIBTS <- getDATRAS(record = "HH", survey = "NS-IBTS",
                       years = 1965:2021, quarters = c(1,2,3,4))
gc()

# Repeated curl error in download of these files; downloaded from the Datras web download site

# HL_NSIBTS_80 <- getDATRAS(record = "HL", survey = "NS-IBTS",
#                        years = 1965:1980, quarters = c(1,2,3,4))
# Sys.sleep(60)
# gc()
# HL_NSIBTS_90 <- getDATRAS(record = "HL", survey = "NS-IBTS",
#                           years = 1981:1990, quarters = c(1,2,3,4))
# Sys.sleep(60)
# gc()
# HL_NSIBTS_00 <- getDATRAS(record = "HL", survey = "NS-IBTS",
#                           years = 1991:2000, quarters = c(1,2,3,4))
# Sys.sleep(60)
# gc()
# HL_NSIBTS_10 <- getDATRAS(record = "HL", survey = "NS-IBTS",
#                           years = 2001:2010, quarters = c(1,2,3,4))
# Sys.sleep(60)
# gc()
# HL_NSIBTS_21 <- getDATRAS(record = "HL", survey = "NS-IBTS",
#                           years = 2011:2021, quarters = c(1,2,3,4))
# gc()

HL_NSIBTS_80 <- read.csv("Raw_Data/DATRAS/HL_NSIBTS_80.csv")
HL_NSIBTS_90 <- read.csv("Raw_Data/DATRAS/HL_NSIBTS_90.csv")
HL_NSIBTS_00 <- read.csv("Raw_Data/DATRAS/HL_NSIBTS_00.csv")
HL_NSIBTS_10 <- read.csv("Raw_Data/DATRAS/HL_NSIBTS_10.csv")
HL_NSIBTS_21 <- read.csv("Raw_Data/DATRAS/HL_NSIBTS_21.csv")

HL_NSIBTS <- rbind(HL_NSIBTS_00,HL_NSIBTS_10,
                   HL_NSIBTS_21, HL_NSIBTS_80,
                   HL_NSIBTS_90)

write.csv(HH_NSIBTS, "Raw_Data/DATRAS/NS-IBTS/HH_NSIBTS.csv")
write.csv(HL_NSIBTS, "Raw_Data/DATRAS/NS-IBTS/HL_NSIBTS.csv")
gc()


### SNS (Sole Net Survey) ---------------

HH_SNS <- getDATRAS(record = "HH", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))
HLSNS <- getDATRAS(record = "HL", survey = "SNS",
                          years = 1985:2021, quarters = c(1,2,3,4))

write.csv(HH_SNS, "Raw_Data/DATRAS/Beam_oth/HH_SNS.csv")
write.csv(HLSNS, "Raw_Data/DATRAS/Beam_oth/HL_SNS.csv")
gc()

### BTS-VIII (Beam Trawl Survey - Bay of Biscay (VIII)) --------------

HH_BT8 <- getDATRAS(record = "HH", survey = "BTS-VIII",
                    years = 2011:2021, quarters = c(1,2,3,4))
HL_BT8<- getDATRAS(record = "HL", survey = "BTS-VIII",
                   years = 2011:2021, quarters = c(1,2,3,4))

write.csv(HH_BT8, "Raw_Data/DATRAS/Beam_oth/HH_BTS8.csv")
write.csv(HL_BT8, "Raw_Data/DATRAS/Beam_oth/HL_BTS8.csv")

gc()
beep(sound = 1, expr = NULL)

### DYFS (Inshore Beam Trawl) (young fish survey) ----------------
HH_DYFS <- getDATRAS(record = "HH", survey = "DYFS",
                    years = 2002:2021, quarters = c(1,2,3,4))
HL_DYFS<- getDATRAS(record = "HL", survey = "DYFS",
                   years = 2002:2021, quarters = c(1,2,3,4))

write.csv(HH_DYFS, "Raw_Data/DATRAS/Beam_oth/HH_DYFS.csv")
write.csv(HL_DYFS, "Raw_Data/DATRAS/Beam_oth/HL_DYFS.csv")
gc()


beep(sound = 1, expr = NULL)



### end ---------



