## 15_daignostics

### set up ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("lme4", "plyr",  "tidyverse", "glmmTMB", "DHARMa")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data ------------

cwm <- readRDS("Data_cwm_PCA.rds")

PC1_null <- readRDS("cwm_models/PC1_null.rds")
PC2_null <- readRDS("cwm_models/PC2_null.rds")
PC3_null <- readRDS("cwm_models/PC3_null.rds")

PC1_full <- readRDS("cwm_models/PC1_full.rds")
PC2_full <- readRDS("cwm_models/PC2_full.rds")
PC3_full <- readRDS("cwm_models/PC3_full.rds")

PC1_2way <- readRDS("cwm_models/PC1_2way.rds")
PC2_2way <- readRDS("cwm_models/PC2_2way.rds")
PC3_2way <- readRDS("cwm_models/PC3_2way.rds")


## summaries -------------------

summary(PC1_null) ## dg AIC 11874.3
summary(PC1_full) ## dg AIC  3423.4 
summary(PC1_2way) ## dg AIC  4659.8

summary(PC2_null) ## dg AIC 41863.4
summary(PC2_full) ## dg AIC 38646.7
summary(PC2_2way) ## dg AIC 39055.5

summary(PC3_null) ## dg AIC 108068.8
summary(PC3_full) ## dg AIC  97818.6
summary(PC3_2way) ## dg AIC  98707.7


## diagnostics ------------------

m <- PC3_2way
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
hist(residuals(simulationOutput))
plot(simulationOutput)
beep()


## 5 res ------------

PC1_null <- readRDS("cwm_5/PC1_null.rds")
PC2_null <- readRDS("cwm_5/PC2_null.rds")
PC3_null <- readRDS("cwm_5/PC3_null.rds")

PC1_full <- readRDS("cwm_5/PC1_full.rds")
PC2_full <- readRDS("cwm_5/PC2_full.rds")
PC3_full <- readRDS("cwm_5/PC3_full.rds")

PC1_2way <- readRDS("cwm_5/PC1_2way.rds")
PC2_2way <- readRDS("cwm_5/PC2_2way.rds")
PC3_2way <- readRDS("cwm_5/PC3_2way.rds")

summary(PC1_null) ## dg AIC 2496.5
summary(PC1_full) ## 
summary(PC1_2way) ##

summary(PC2_null) ## dg AIC 41863.4
summary(PC2_full) ## dg AIC 38646.7
summary(PC2_2way) ## dg AIC 39055.5

summary(PC3_null) ## dg AIC 108068.8
summary(PC3_full) ## dg AIC  97818.6
summary(PC3_2way) ## dg AIC  98707.7


## diagnostics ------------------

m <- PC1_null
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
hist(residuals(simulationOutput))
plot(simulationOutput)
beep()






