## 14cwm_analysis
# setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")
# source("R_Scripts/14cwm_analysis.R")

### set up ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("lme4", "plyr",  "tidyverse", "glmmTMB") #, "DHARMa")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data ------------

cwm_main <- readRDS("Data_cwm_PCA.rds")

cwm <- unique(cwm_main[, c("Year", "HaulID", "PC1_cwm", "PC2_cwm", "PC3_cwm", "Gear", "Quarter", "DepthNew", 
                      "SNSP",  "SNWI", "fp", "gear_ship", "gear_ship_loc", 
                      "sst_var")])


## PC1 ---------------------

#null model

PC1_null <- glmmTMB(PC1_cwm ~  1 +
                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                     data = cwm)

saveRDS(PC1_null, "cwm_models/PC1_null.rds")

# full model
# PC1_full <- glmmTMB(PC1_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                      data = cwm)
# 
# saveRDS(PC1_full, "cwm_models/PC1_full.rds")

## 2 way
PC1_2way <- glmmTMB(PC1_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      
                      DepthNew*fp +
                      
                      Year + Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)

saveRDS(PC1_2way, "cwm_models/PC1_2way.rds")

## PC2 ------------

# null model
PC2_null <- glmmTMB(PC2_cwm ~  1 +
                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                     data = cwm)
saveRDS(PC2_null, "cwm_models/PC2_null.rds")

# full model
# PC2_full <- glmmTMB(PC2_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                        (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                      data = cwm)
# saveRDS(PC2_full, "cwm_models/PC2_full.rds")

## 2 way
PC2_2way <- glmmTMB(PC2_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      
                      DepthNew*fp +
                      
                      Year + Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
saveRDS(PC2_2way, "cwm_models/PC2_2way.rds")

## PC3 ------------

# null model
PC3_null <- glmmTMB(PC3_cwm ~  1 +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)

saveRDS(PC3_null, "cwm_models/PC3_null.rds")

# full model
# PC3_full <- glmmTMB(PC3_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                     data = cwm)
# saveRDS(PC3_full, "cwm_models/PC3_full.rds")

## 2 way
PC3_2way <- glmmTMB(PC3_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      
                      DepthNew*fp +
                      
                      Year + Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
saveRDS(PC3_2way, "cwm_models/PC3_2way.rds")



## end ---------





















