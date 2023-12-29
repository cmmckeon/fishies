## 15_daignostics

### set up ---------------

# set working directory
setwd("..")

list<-c("lme4", "plyr",  "tidyverse", "glmmTMB", "DHARMa", "performace")

lapply(list, require, character.only=T)
#lapply(list, citation)

## create "not in" operator
'%nin%' = Negate('%in%')

## read in data ------------

cwm <- readRDS("Data_cwm_PCA.rds")

PC1_null <- readRDS("cwm_1/PC1_null.rds")
PC2_null <- readRDS("cwm_1/PC2_null.rds")
PC3_null <- readRDS("cwm_1/PC3_null.rds")

# PC1_full <- readRDS("cwm_1/PC1_full.rds")
# PC2_full <- readRDS("cwm_1/PC2_full.rds")
# PC3_full <- readRDS("cwm_1/PC3_full.rds")

PC1_2way <- readRDS("cwm_1/PC1_2way.rds")
PC2_2way <- readRDS("cwm_1/PC2_2way.rds")
PC3_2way <- readRDS("cwm_1/PC3_2way.rds")


## summaries -------------------

summary(PC1_null) 
summary(PC1_full)
summary(PC1_2way)

summary(PC2_null) 
summary(PC2_full) 
summary(PC2_2way) 

summary(PC3_null) 
summary(PC3_full)
summary(PC3_2way) 


## diagnostics ------------------

m <- PC1_2way
m <- PC2_2way
m <- PC3_2way
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
#hist(residuals(simulationOutput))
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

summary(PC1_null) 
summary(PC1_full) 
summary(PC1_2way) 

summary(PC2_null) 
summary(PC2_full) 
summary(PC2_2way) 

summary(PC3_null)
summary(PC3_full) 
summary(PC3_2way) 


## diagnostics ------------------

m <- PC1_2way
m <- PC2_2way
m <- PC3_2way
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m, plot = F)
#hist(residuals(simulationOutput))
plot(simulationOutput)
beep()


## 10 res ------------

PC1_null <- readRDS("cwm_10/PC1_null.rds")
PC2_null <- readRDS("cwm_10/PC2_null.rds")
PC3_null <- readRDS("cwm_10/PC3_null.rds")

PC1_full <- readRDS("cwm_10/PC1_full.rds")
PC2_full <- readRDS("cwm_10/PC2_full.rds")
PC3_full <- readRDS("cwm_10/PC3_full.rds")

PC1_2way <- readRDS("cwm_10/PC1_2way.rds")
PC2_2way <- readRDS("cwm_10/PC2_2way.rds")
PC3_2way <- readRDS("cwm_10/PC3_2way.rds")

summary(PC1_null) 
summary(PC1_full) 
summary(PC1_2way) 

summary(PC2_null) 
summary(PC2_full)
summary(PC2_2way) 

summary(PC3_null) 
summary(PC3_full) 
summary(PC3_2way) 


## diagnostics ------------------

m <- PC1_2way
m <- PC2_2way
m <- PC3_2way
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
#hist(residuals(simulationOutput))
plot(simulationOutput)
beep()


## 20 res ------------

PC1_null <- readRDS("cwm_20/PC1_null.rds")
PC2_null <- readRDS("cwm_20/PC2_null.rds")
PC3_null <- readRDS("cwm_20/PC3_null.rds")

PC1_full <- readRDS("cwm_20/PC1_full.rds")
PC2_full <- readRDS("cwm_20/PC2_full.rds")
PC3_full <- readRDS("cwm_20/PC3_full.rds")

PC1_2way <- readRDS("cwm_20/PC1_2way.rds")
PC2_2way <- readRDS("cwm_20/PC2_2way.rds")
PC3_2way <- readRDS("cwm_20/PC3_2way.rds")

summary(PC1_null) 
summary(PC1_full) 
summary(PC1_2way) 

summary(PC2_null) 
summary(PC2_full) 
summary(PC2_2way) 

summary(PC3_null) 
summary(PC3_full) 
summary(PC3_2way) 


## diagnostics ------------------

m <- PC1_2way
m <- PC2_2way
m <- PC3_2way
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
#hist(residuals(simulationOutput))
plot(simulationOutput)
beep()
gc()

## 50 res ------------

PC1_null <- readRDS("cwm_50/PC1_null.rds")
PC2_null <- readRDS("cwm_50/PC2_null.rds")
PC3_null <- readRDS("cwm_50/PC3_null.rds")

PC1_full <- readRDS("cwm_50/PC1_full.rds")
PC2_full <- readRDS("cwm_50/PC2_full.rds")
PC3_full <- readRDS("cwm_50/PC3_full.rds")

PC1_2way <- readRDS("cwm_50/PC1_2way.rds")
PC2_2way <- readRDS("cwm_50/PC2_2way.rds")
PC3_2way <- readRDS("cwm_50/PC3_2way.rds")

summary(PC1_null) 
summary(PC1_full) 
summary(PC1_2way) 

summary(PC2_null) 
summary(PC2_full) 
summary(PC2_2way) 

summary(PC3_null)
summary(PC3_full) 
summary(PC3_2way) 


## diagnostics ------------------

m <- PC1_2way
m <- PC2_2way
m <- PC3_2way
simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
#hist(residuals(simulationOutput))
plot(simulationOutput)
beep()
gc()



## r2 calculations


# r2_nakagawa(pc1)
# r2_nakagawa(pc2)
# r2_nakagawa(pc3)
# 
# r2_nakagawa(pc1_5)
# r2_nakagawa(pc2_5)
# r2_nakagawa(pc3_5)
# 
# r2_nakagawa(pc1_10)
# r2_nakagawa(pc2_10)
# r2_nakagawa(pc3_10)
# 
# r2_nakagawa(pc1_20)
# r2_nakagawa(pc2_20)
# r2_nakagawa(pc3_20)
# 
# r2_nakagawa(pc1_50)
# r2_nakagawa(pc2_50)
# r2_nakagawa(pc3_50)
# 
# 
# MuMIn::r.squaredGLMM(pc1)
# MuMIn::r.squaredGLMM(pc2)
# MuMIn::r.squaredGLMM(pc3)
# 
# MuMIn::r.squaredGLMM(pc1)
# MuMIn::r.squaredGLMM(pc2)
# MuMIn::r.squaredGLMM(pc3)


## start
# ## null
# rsqrd <- as.data.frame(r)
# for(i in r){
#   mmF <- m_metric_null[[i]][["hf"]][[1]]
#   
#   # MCMCglmm - marginal with crebile intervals
#   vmVarF<-numeric(1000)
#   for(j in 1:1000){
#     Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
#     vmVarF[j]<-Var}
#   
#   R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
#   rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
#   rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
#   rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
#   rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
#   
#   # MCMCglmm - conditional with crebile intervals
#   R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
#   rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
#   rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
#   rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
#   rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
# }
# 
# r2_null <- rsqrd
# r2_null$model <- "null"

## end ---------------












