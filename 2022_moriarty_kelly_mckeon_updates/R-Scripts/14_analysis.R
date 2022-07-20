## 14_analysis
#source("R-scripts/14_analysis.R")

### set up ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

list<-c("ggplot2", "data.table", "reshape2", "arm","car", "DMwR", "Hmisc",
        "lme4", "plyr", "plotrix", "colorspace", "plot3D", "plot3D", "rgl","MuMIn",
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "raster", "ncdf4", "marmap", "rgdal", "foreign",
        "sf", "glmmTMB", "DHARMa")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

# ## pairs function
# upper.panel<-function(x, y){
#   points(x,y, pch=21, col=c("grey"), cex = 0.5)
#   r <- round(cor(x, y), digits=2)
#   txt <- paste0("R = ", r)
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   text(0.8, 0.9, txt, cex =0.7)
# }



## read in data ------------

mydata <- readRDS("Data_modeldf_abundance.rds")
abundance <- drop_na(mydata)


## abund ~ sst*DepthNew*fp*Year*Quarter + (1|Gear) + (1|ship) + (1|stNo)

## modelling --------------


## model with fishing pressure yes/no ----------

## model with fishing pressure as continuous -------------

## for beta_family models
#abundance$rel_ab <- abundance$rel_ab - 1.727584e-08

## null model
# Sys.time()
# cont_null <- glmmTMB(rel_ab ~ 1 + (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
#                         family = beta_family,
#                         data = abundance)
# Sys.time()
# summary(cont_null)
# #saveRDS(cont_null, "rel_ab_null_model.rds")


## full model
Sys.time()
cont_full <- glmmTMB(rel_ab ~ SNSP*SNWI*sst_var*DepthNew*fp*Year*PC1 + SNSP*SNWI*sst_var*DepthNew*fp*Year*PC2 + Quarter +
                       (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     family = beta_family,
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()

saveRDS(cont_full, "rel_ab_full_model.rds")
beep()


# mod2_optim <- update(cont_full,
#                      control=glmmTMBControl(optimizer=optim,
#                                             optArgs=list(method="BFGS")))

#saveRDS(mod2_optim, "nbinom2_falseconvergence_update_full_model.rds")




# cont_null <- readRDS("rel_ab_null_model.rds")
# summary(cont_null)







## diagnostics -------------

# testDispersion(cont_full)
# par(mfrow = c(2,2))
# simulationOutput <- simulateResiduals(fittedModel = rel_ab_null_beta_model, plot = F)
# residuals(simulationOutput)
# plot(simulationOutput)
# 
# simulationOutput_beta <- simulateResiduals(fittedModel = rel_ab_null_model, plot = F)
# residuals(simulationOutput)
# plot(simulationOutput_beta)

