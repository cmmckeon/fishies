# title: "2_Load_Data.R"
# last updated: "08/04/2022"

# This is Script 2 of 7
# The purpose of this script is to load the raw data into R from the various folders.

## set up
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

# Load data ----------------

# load("./script1_output.rda")
# load("./script2_output.rda")

## OR

# Load Survey Data (Datras HH files)---------------------------

# Read in haul header data

## list of Surverys we are working with
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

## NIGFS -----------
HH_NIGFS<-read.csv("Raw_Data/DATRAS/NIGFS/HH_data_NIGFS.csv")

## EVHOE
HH_EVHOE<-read.csv("Raw_Data/DATRAS/EVHOE/HH_data_EVHOE.csv")

## FR-CGFS -----------------
HH_FRCGFS<-read.csv("Raw_Data/DATRAS/FR-CGFS/HH_data_FRCGFS.csv")

## IE-IGFS ------------------
HH_IGFS<-read.csv("Raw_Data/DATRAS/IE-IGFS/HH_data_IEIGFS.csv")

## SWC-IBTS and SCOWCGFS---------------
## scottish files are now split into two on Datras (RK)
HH_SWC1<-read.csv("./Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS1.csv")
HH_SWC2<-read.csv("./Raw_Data/DATRAS/SWC-IBTS/HH_data_SWC_IBTS2.csv")
HH_SWC <- rbind(HH_SWC1,HH_SWC2)
rm(HH_SWC1)
rm(HH_SWC2)

## Rockall and SCOROC ----------
HH_ROCK1 <-read.csv("Raw_Data/DATRAS/ROCKALL/HH_data_ROC1.csv")
HH_ROCK2 <-read.csv("Raw_Data/DATRAS/ROCKALL/HH_data_ROC2.csv")
HH_ROCK <- rbind(HH_ROCK1, HH_ROCK2)
rm(HH_ROCK1, HH_ROCK2)

## BTS ---------
HH_BTS<-read.csv("Raw_Data/DATRAS/BTS/HH_data_BTS.csv")

## NS-IBTS ------------
HH_NSIBTS <-read.csv("Raw_Data/DATRAS/NS-IBTS/HH_NSIBTS.csv",row.names = "X")

## SNS -------------
HH_SNS <-read.csv("./Raw_Data/DATRAS/Beam_oth/HH_SNS.csv") 
HH_SNS <- HH_SNS[, which(names(HH_SNS) != "X")]

## BTS-VIII ----------
HH_BTS8 <-read.csv("./Raw_Data/DATRAS/Beam_oth/HH_BTS8.csv") 
HH_BTS8 <- HH_BTS8[, which(names(HH_BTS8) != "X")]


## DYFS -------------
HH_DYFS <-read.csv("./Raw_Data/DATRAS/Beam_oth/HH_DYFS.csv") 
HH_DYFS <- HH_DYFS[, which(names(HH_DYFS) != "X")]

# Load biological data (Datras HL files) --------------


## NIGFS ----------------
HL_NIGFS<-read.csv("Raw_Data/DATRAS/NIGFS/HL_data_NIGFS.csv")

## EVHOE ----------------
HL_EVHOE<-read.csv("Raw_Data/DATRAS/EVHOE/HL_data_EVHOE.csv")

## FR-CGFS ----------------
HL_FRCGFS<-read.csv("Raw_Data/DATRAS/FR-CGFS/HL_data_FRCGRS.csv")

## IE-IGFS ----------------
HL_IGFS<-read.csv("Raw_Data/DATRAS/IE-IGFS/HL_data_IEIGFS.csv")

## SWC-IBTS ---------
HL_SWC1<-read.csv("Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS1.csv")

## SCOWCGFS --------
HL_SWC2<-read.csv("Raw_Data/DATRAS/SWC-IBTS/HL_data_SWC_IBTS2.csv")

HL_SWC <- rbind(HL_SWC1, HL_SWC2)
rm(HL_SWC1, HL_SWC2)

## ROCKALL ----------------
HL_ROCK1<-read.csv("Raw_Data/DATRAS/ROCKALL/HL_data_ROC1.csv")

## SCOROC ----------------
HL_ROCK2<-read.csv("Raw_Data/DATRAS/ROCKALL/HL_data_ROC2.csv")

HL_ROCK <- rbind(HL_ROCK1,HL_ROCK2)
rm(HL_ROCK1,HL_ROCK2)

## BTS ----------------
HL_BTS <-read.csv("Raw_Data/DATRAS/BTS/HL_data_BTS.csv")
HL_BTS <- HL_BTS[, which(names(HL_BTS) != "ScientificName_WoRMS")]

## NS-IBTS ----------------
HL_NSIBTS <-read.csv("Raw_Data/DATRAS/NS-IBTS/HL_NSIBTS.csv", row.names = "X")
HL_NSIBTS <- HL_NSIBTS[, which(names(HL_NSIBTS) != "ScientificName_WoRMS")]

## SNS ----------------
HL_SNS <-read.csv("./Raw_Data/DATRAS/Beam_oth/HL_SNS.csv")
HL_SNS <- HL_SNS[, which(names(HL_SNS) != "X")]

## BTS-VIII ----------------
HL_BTS8 <-read.csv("./Raw_Data/DATRAS/Beam_oth/HL_BTS8.csv")
HL_BTS8 <- HL_BTS8[, which(names(HL_BTS8) != "X")]

## DYFS ----------------
HL_DYFS <-read.csv("./Raw_Data/DATRAS/Beam_oth/HL_DYFS.csv")
HL_DYFS <- HL_DYFS[, which(names(HL_DYFS) != "X")]


# Add national submitted data -------

# Danish Data

# Add corrections of data from National Data providers

# Denmark earliest years of survey were missing species
NS_DEN_sp_1986<-read.csv("./Raw_Data/Corrections/DNK_IBTS1_1986_GOV.CSV", header=F, stringsAsFactors = FALSE)

# Northern Ireland early data not available on Datras
NI_extra<-read.csv("./Raw_Data/National Submissions/Datras_MSFD_NI/Datras_MSFD3.txt", header=F) ## updated from Datras_MSFD1.csv to lastest data product CM

save(list=ls(all=T), file = "./script2_output.rda")
#load("./script2_output.rda")

beep(sound = 1, expr = NULL)

# end ---------
