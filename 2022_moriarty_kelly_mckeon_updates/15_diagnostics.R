## 15_daignostics

### set up ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("lme4", "plyr",  "tidyverse", "glmmTMB") #, "DHARMa")

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


## diagnostics ------------------

# simulationOutput <- simulateResiduals(fittedModel = PC3_full, plot = F)
# residuals(simulationOutput)
# plot(simulationOutput)
# 
# 
# summary(rel_ab_beta_full_model)