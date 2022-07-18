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


# m <- drop_na(mydata)
# pairs(m[, which(names(m) %nin% c("Year", "Quarter", "HaulID", "Gear",  "SciName", 
#                                              "ShootLat", "ShootLong", 
#                                              "gear_ship", "gear_ship_loc"))], lower.panel = NULL, upper.panel = upper.panel)
# 
# rcorr(as.matrix(m[, which(names(m) %in% c("DepthNew", "Total_DensAbund_N_Sqkm", "sst", "fp", 
#                                           "fp_yn", "abund"))]))

# # Numeric variables
# par(mfrow=c(3,3))
# for (i in names(Filter(is.numeric, m))) {
#   hist(m[,i], breaks = 1000, main = paste(i))
#   gc()
# }

#md <- sample_n(mydata, 10)



## abund ~ sst*DepthNew*fp*Year*Quarter + (1|Gear) + (1|ship) + (1|stNo)

## modelling --------------


## model with fishing pressure yes/no ----------

## null model 
Sys.time()
fishies_null <- glmmTMB(resp ~ 1 + (1|SciName) + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                family = poisson,
                data = mydata)
Sys.time()
summary(fishies_null)

## full model converges - warnings about NA/NaN function evaluation can be ignored as it just means the model went into an invalid region of parameter space at some point,
## not a problem so long as it leaves this region and converges in the end
Sys.time()
fishies_full <- glmmTMB(resp ~ sst*DepthNew*fp_yn*Year*Quarter + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
              family = poisson,
              control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                       profile = FALSE, collect = FALSE),
             data = mydata)
Sys.time()

#saveRDS(fishies_full, "fp_yn_full_model.rds")
beep()

fishies_full <- readRDS("fp_yn_full_model.rds")
summary(fishies_full)

## model with fishing pressure as continuous -------------

## null model
Sys.time()
cont_null <- glmmTMB(resp ~ 1 + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                        family = poisson,
                        data = abundance)
Sys.time()
summary(cont_null)
#saveRDS(cont_null, "fp_cont_null_model.rds")


## full model
Sys.time()
cont_full <- glmmTMB(resp ~ sst*DepthNew*fp*Year*Quarter + SciName + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                        family = poisson,
                        control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                                 profile = FALSE, collect = FALSE),
                        data = abundance)
Sys.time()

Sys.time()
cont_full <- glmmTMB(resp ~ sst*DepthNew*fp*Year*Quarter + (1|Gear) + (1|gear_ship) + (1|gear_ship_loc),
                     family = nbinom2,
                     control = glmmTMBControl(optCtrl = list(iter.max = 1000000, eval.max = 1000000),
                                              profile = FALSE, collect = FALSE),
                     data = abundance)
Sys.time()

mod2_optim <- update(cont_full,
                     control=glmmTMBControl(optimizer=optim,
                                            optArgs=list(method="BFGS")))

#saveRDS(cont_full, "nbinom2_full_model.rds")
beep()

#saveRDS(mod2_optim, "nbinom2_falseconvergence_update_full_model.rds")
beep()

cont_full <- readRDS("fp_cont_full_model.rds")
summary(cont_full)

cont_full <- readRDS("fp_cont_full_model_sciname.rds")
summary(cont_full)

cont_full <- readRDS("nbinom2_full_model.rds")
summary(cont_full)


## abundance by location
Sys.time()
for(i in unique(mydata$HaulID)){
  print(i)
  mydata$total[mydata$HaulID == i] <- sum(mydata$Total_DensAbund_N_Sqkm[mydata$HaulID == i])
    }
Sys.time()
beep()

#mydata <- readRDS("summed_abundance.rds")
mydata$log_total <- log(mydata$total)

abundance <- drop_na(mydata)
abundance <- abundance[abundance$Quarter != "SNAU",]

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



