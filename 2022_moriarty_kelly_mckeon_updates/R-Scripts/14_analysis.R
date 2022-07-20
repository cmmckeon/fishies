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

## null model
Sys.time()
cont_null <- glmmTMB(rel_ab ~ 1 + (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                        family = betabinomial,
                        data = abundance)
Sys.time()
summary(cont_null)
#saveRDS(cont_null, "null_model.rds")


abundance <- abundance[abundance$Quarter > 0,]

## full model
Sys.time()
cont_full <- glmmTMB(rel_ab ~ sst*DepthNew*fp*Year*PC1 + sst*DepthNew*fp*Year*PC2 +
                       (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     family = betabinomial,
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()

#saveRDS(cont_full, "winter_sciname_traits_poisson_model.rds")
beep()


Sys.time()
cont_full <- glmmTMB(resp ~ sst*DepthNew*fp*Year + 
                       tl +
                       age.maturity +
                       length.max+ 
                       fecundity +
                       (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     family = poisson,
                     # family = nbinom2, ## this is no better than poisson in diagnostics, but much slower
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()

#saveRDS(cont_full, "plus_winter_sciname_traits_poisson_model.rds")
beep()

# mod2_optim <- update(cont_full,
#                      control=glmmTMBControl(optimizer=optim,
#                                             optArgs=list(method="BFGS")))

#saveRDS(cont_full, "nbinom2_full_model.rds")
beep()

#saveRDS(mod2_optim, "nbinom2_falseconvergence_update_full_model.rds")



par(mfrow = c(3,3))
plot(abundance$resp ~ abundance$tl)
plot(abundance$resp ~ abundance$body.shape)
plot(abundance$resp ~ abundance$offspring.size)
plot(abundance$resp ~ abundance$age.maturity)
plot(abundance$resp ~ abundance$age.max)
plot(abundance$resp ~ abundance$growth.coefficient)
plot(abundance$resp ~ abundance$length.max)


cont_full <- readRDS("fp_cont_full_model.rds")
summary(cont_full)

cont_full <- readRDS("fp_cont_full_model_sciname.rds")
summary(cont_full)

cont_full <- readRDS("nbinom2_full_model.rds")
summary(cont_full)


cont_full <- readRDS("winter_sciname_traits_poisson_model.rds")
summary(cont_full)

# abundance <- unique(abundance[, c("Year", "Quarter", "HaulID", "Gear", "DepthNew",
#                            "ShootLat", "ShootLong", "sst", "fp", 
#                            "gear_ship", "gear_ship_loc", "fp_yn","total", 
#                            "log_total")])

Sys.time()
null_total <- glmmTMB(log_total ~ 1 +  (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    # family = poisson,
                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()
beep()


## full model with summed total abundance
Sys.time()
cont_full <- glmmTMB(log_total ~ sst*DepthNew*fp*Year*Quarter + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                    # family = poisson,
                     control = glmmTMBControl(optCtrl = list(iter.max = 10000000, eval.max = 10000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()

#saveRDS(cont_full, "full_total.rds")
beep()
full_total <- readRDS("full_total.rds")
summary(full_total)

# x <- unique(abundance[abundance$SciName == "Trisopterus esmarkii",])
# 
# plot(x$resp ~ x$fp)

summary(full)

## diagnostics -------------

testDispersion(cont_full)
par(mfrow = c(2,2))
simulationOutput <- simulateResiduals(fittedModel = cont_full, plot = F)
residuals(simulationOutput)
plot(simulationOutput)



