#14c_10scale_analysis
# setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")
# source("R_Scripts/14c_10scale_analysis.R")

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

# par(mfrow=c(4,4))
# for (i in names(Filter(is.numeric, cwm_main))) {
#   hist(cwm_main[,i], breaks = 1000, main = paste(i))
#   gc()
# }

cwm <- unique(cwm_main[, c("res10", "PC1_cwm10", "PC2_cwm10", "PC3_cwm10", 
                           "Year", "Gear", "Quarter", "DepthNew", "SNSP", "SNWI", "fp", 
                           "sst_var")])

for(i in c("DepthNew", "SNSP", "SNWI", "fp", "sst_var")){
  for(j in unique(cwm$res10)){
    for(k in unique(cwm$Year)){
    cwm[,i][cwm$res10 == j & cwm$Year == k] <- mean(cwm[,i][cwm$res10 == j & cwm$Year == k])
  }}}

cwm <- unique(cwm)
## renames to save me retyping everything 
names(cwm) <- c("res10", "PC1_cwm", "PC2_cwm", "PC3_cwm", "Year", "Gear", 
                "Quarter", "DepthNew", "SNSP", "SNWI", "fp", "sst_var")


setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates/cwm_10")

## PC1 ---------------------

#null model
print("pc1 null model")

PC1_null <- glmmTMB(PC1_cwm ~  1 +
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)

saveRDS(PC1_null, "PC1_null.rds")

# full model
# print("pc1 full model")
# PC1_full <- glmmTMB(PC1_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                       (1|Gear),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                     data = cwm)
# 
# saveRDS(PC1_full, "PC1_full.rds")

## 2 way
print("pc1 2 way model")
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
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 100000000, eval.max = 100000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)

saveRDS(PC1_2way, "PC1_2way.rds")

## PC2 ------------

# null model
print("pc2 null model")
PC2_null <- glmmTMB(PC2_cwm ~  1 +
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
saveRDS(PC2_null, "PC2_null.rds")

# full model
# print("pc2 full model")
# PC2_full <- glmmTMB(PC2_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                       (1|Gear),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                     data = cwm)
# saveRDS(PC2_full, "PC2_full.rds")

## 2 way
print("pc2 2 way model")
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
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
saveRDS(PC2_2way, "PC2_2way.rds")

## PC3 ------------

# null model
print("pc3 null model")
PC3_null <- glmmTMB(PC3_cwm ~  1 +
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)

saveRDS(PC3_null, "PC3_null.rds")

# full model
# print("pc3 full model")
# PC3_full <- glmmTMB(PC3_cwm ~ SNSP*SNWI*sst_var*DepthNew*fp + Year + Quarter +
#                       (1|Gear),
#                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                              profile = FALSE, collect = FALSE),
#                     data = cwm)
# saveRDS(PC3_full, "PC3_full.rds")

## 2 way
print("pc3 2 way model")
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
                      (1|Gear),
                    control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                             profile = FALSE, collect = FALSE),
                    data = cwm)
saveRDS(PC3_2way, "PC3_2way.rds")

# ## year_interaction
# PC1_year_interaction <- glmmTMB(PC1_cwm ~
#                                   SNSP*SNWI +
#                                   SNSP*sst_var +
#                                   SNSP*DepthNew +
#                                   SNSP*fp +
#                                   
#                                   SNWI*sst_var +
#                                   SNWI*DepthNew +
#                                   SNWI*fp +
#                                   
#                                   sst_var*DepthNew +
#                                   sst_var*fp +
#                                   
#                                   DepthNew*fp +
#                                   Year*fp +
#                                   
#                                   Quarter +
#                                   (1|Gear),
#                                 control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                                          profile = FALSE, collect = FALSE),
#                                 data = cwm)
# 
# PC2_year_interaction <- glmmTMB(PC2_cwm ~
#                                   SNSP*SNWI +
#                                   SNSP*sst_var +
#                                   SNSP*DepthNew +
#                                   SNSP*fp +
#                                   
#                                   SNWI*sst_var +
#                                   SNWI*DepthNew +
#                                   SNWI*fp +
#                                   
#                                   sst_var*DepthNew +
#                                   sst_var*fp +
#                                   
#                                   DepthNew*fp +
#                                   Year*fp +
#                                   
#                                   Quarter +
#                                   (1|Gear),
#                                 control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                                          profile = FALSE, collect = FALSE),
#                                 data = cwm)
# 
# PC3_year_interaction <- glmmTMB(PC3_cwm ~
#                                   SNSP*SNWI +
#                                   SNSP*sst_var +
#                                   SNSP*DepthNew +
#                                   SNSP*fp +
#                                   
#                                   SNWI*sst_var +
#                                   SNWI*DepthNew +
#                                   SNWI*fp +
#                                   
#                                   sst_var*DepthNew +
#                                   sst_var*fp +
#                                   
#                                   DepthNew*fp +
#                                   Year*fp +
#                                   
#                                   Quarter +
#                                   (1|Gear),
#                                 control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
#                                                          profile = FALSE, collect = FALSE),
#                                 data = cwm)

## end ---------














