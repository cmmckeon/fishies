## 14cwm_analysis
#source("R-scripts/14b_analysis.R")

### set up ---------------

# set working directory
setwd("~/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("lme4", "plyr",  "tidyverse", "glmmTMB") #, "DHARMa")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data ------------

cwm <- readRDS("Data_cwm_PCA.rds")

## PC1 ---------------------

null model
Sys.time()
PC1_null <- glmmTMB(PC1_cwm ~  1 +
                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = cwm)
Sys.time()

summary(PC1_null)

# full model
Sys.time()
PC1_full <- glmmTMB(PC1_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp*Year + Quarter +
                     (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = cwm)
Sys.time()
beep()

summary(PC1_full)

## 2 way
Sys.time()
PC1_2way <- glmmTMB(PC1_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      SNSP*Year +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      SNWI*Year +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      sst_var*Year +
                      
                      DepthNew*fp +
                      DepthNew*Year +
                      
                      fp*Year +
                      
                      Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
Sys.time()
beep()

summary(PC1_2way)

## PC2 ------------

# null model
Sys.time()
PC2_null <- glmmTMB(PC2_cwm ~  1 +
                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = cwm)
Sys.time()

summary(PC2_null)

# full model
Sys.time()
PC2_full <- glmmTMB(PC2_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp*Year + Quarter +
                       (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = cwm)
Sys.time()
beep()

summary(PC2_full)

## 2 way
Sys.time()
PC2_2way <- glmmTMB(PC2_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      SNSP*Year +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      SNWI*Year +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      sst_var*Year +
                      
                      DepthNew*fp +
                      DepthNew*Year +
                      
                      fp*Year +
                      
                      Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
Sys.time()
beep()

summary(PC2_2way)

## PC3 ------------

# null model
Sys.time()
PC3_null <- glmmTMB(PC3_cwm ~  1 +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
Sys.time()

summary(PC3_null)

# full model
Sys.time()
PC3_full <- glmmTMB(PC3_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
Sys.time()
beep()

summary(PC3_full)

Sys.time()
PC3_2way <- glmmTMB(PC3_cwm ~ 
                      SNSP*SNWI +
                      SNSP*sst_var +
                      SNSP*DepthNew +
                      SNSP*fp +
                      SNSP*Year +
                      
                      SNWI*sst_var +
                      SNWI*DepthNew +
                      SNWI*fp +
                      SNWI*Year +
                      
                      sst_var*DepthNew +
                      sst_var*fp +
                      sst_var*Year +
                      
                      DepthNew*fp +
                      DepthNew*Year +
                      
                      fp*Year +
                      
                      Quarter +
                      (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
Sys.time()
beep()

summary(PC3_2way)



## diagnostics ------------------

simulationOutput <- simulateResiduals(fittedModel = PC3_full, plot = F)
residuals(simulationOutput)
plot(simulationOutput)


summary(rel_ab_beta_full_model)





















