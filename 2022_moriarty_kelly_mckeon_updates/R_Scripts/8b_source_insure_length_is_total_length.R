## 8b_source_insure_length_is_total_length.R

## this script makes sure all fish lengths == total length by converting 
## the measurements of taxa that were measured in non-total length ways


dat$FishLength_cm <- dat$newLngtClass
# Not all fish are measured to TL some are measured to SL, PAFL and PSCFL 
# These need converting to TL
# Fish measured to SL, PAFL and PSCFL instead of TL
dat$LengthType[dat$family=="Macrouridae"]<-"PAFL"
dat$LengthType[dat$family=="Alepocephalidae"]<-"SL"
dat$LengthType[dat$family=="Chimaeridae"]<-"PSCFL"
dat$LengthType[is.na(dat$LengthType)]<-"TL"
# EVHOE - not measuring to PAFL
# fish measured to SL, PAFL and PSCFL instead of TL
dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="EVHOE"]<-"TL"
# SWC and NS-IBTS didn't inplement PAFL measurements until 1999
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SWC-IBTS"&dat$Year<1999]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="NS-IBTS"&dat$Year<1999]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SP_ARSA"&dat$Year<2000]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SP_PORC"&dat$Year<2000]<-"TL"
# dat$LengthType[dat$family=="Macrouridae"&dat$Survey=="SPNGFSC"&dat$Year<2000]<-"TL"
# Hymenocephalus italicus is TL Mindel et al 2015
# saveRDS(dat, "dat_1037.rds")
dat$LengthType[dat$estsciname=="Hymenocephalus italicus"&
                 dat$LengthType=="PAFL"]<-"TL"
dat$LengthType[dat$estsciname=="Hymenocephalus gracilis"&
                 dat$LengthType=="PAFL"]<-"TL"
# in SP-ARSA before 2003 Nezumia aequalis was measured at TL
# unique(dat$Survey)
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_ARSA"&dat$Year<2003]<-"TL"
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_ARSA"&dat$Year==2004&dat$newLngtClass>10]<-"TL"
# in SP-PORC before 2001 Nezumia aequalis was measured at TL
# dat$LengthType[dat$estsciname=="Nezumia aequalis"&
#                   dat$Survey=="SP_PORC"&dat$Year<2001]<-"TL"
# # in SP-PORC up to 2004 Coelorinchus caelorhincus was measured at TL
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SP_PORC"&dat$Year<2005]<-"TL"
# Coelorinchus caelorhincus
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SPNGFS"&dat$Year<2010]<-"TL"
# Coelorinchus caelorhincus
dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
                 dat$Survey=="IE-IGFS"&dat$Year<2008 & dat$newLngtClass>10]<-"TL"
# in SPNGFS up to 2009 Malacocephalus laevis was measured at TL
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SPNGFS"&dat$Year<2010]<-"TL"
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SP_ARSA"&dat$Year<2003]<-"TL"
# dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
#                   dat$Survey=="SP_ARSA"&dat$Year==2004&dat$newLngtClass>20]<-"TL"
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                 dat$Survey=="IE-IGFS"&dat$Year<2007]<-"TL"
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                 dat$Survey=="NS-IBTS"&dat$Year==2016]<-"TL"
# dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
#                   dat$Survey=="SP_PORC"&dat$Year<2005]<-"TL"
dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
                 dat$Survey=="IE-IGFS"&dat$Year<2007]<-"TL"
# dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
#                   dat$Survey=="SP_PORC"]<-"TL"
# Macrouridae Mindel et al 2015
dat$LengthType[dat$estsciname=="Macrouridae"&dat$LengthType=="PAFL"]<-"TL"

## PAFL to TL -------------------------
# Total Length converter for COC (Coelorinchus caelorhincus) is *2.82
# Total Length converter for MLA (Malacocephalus laevis) is *4.57
summary(as.factor(dat$LengthType))
# need to address the PAFL and PSCFL and use an approperiate conversion factor.
# fisrt check length information as it stands
PAFL_fish<-subset(dat, LengthType=="PAFL",)
write.csv(PAFL_fish, "PAFL_fish.csv")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFF00")

ggplot(PAFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme_classic()

ggplot(PAFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~valid_name, scales="free", ncol=4)+
  theme_classic()
# look closer at Trachyrincus scabrus
find<-subset(PAFL_fish, estsciname=="Trachyrincus scabrus",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
# Trachyrincus murrayi Mindel et al 2015
dat$FishLength_cm[dat$estsciname=="Trachyrincus scabrus"&
                    dat$LengthType=="PAFL"]<-3.1*dat$newLngtClass[dat$estsciname=="Trachyrincus scabrus"&
                                                                    dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Trachyrincus scabrus"&
                 dat$LengthType=="PAFL"]<-"TL"
# Trachyrincus murrayi Mindel et al 2015
#dat$FishLength_cm[dat$estsciname=="Trachyrincus murrayi"&
#                     dat$LengthType=="PAFL"]<-3.1*dat$newLngtClass[dat$estsciname=="Trachyrincus murrayi"&
#                                                                       dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Trachyrincus murrayi"&
                 dat$LengthType=="PAFL"]<-"TL"
# look closer at Nezumia aequalis
find<-subset(PAFL_fish, estsciname=="Nezumia aequalis",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
# Nezumia aequalis Mindel et al 2015

dat$FishLength_cm[dat$estsciname=="Nezumia aequalis"&
                    dat$LengthType=="PAFL"]<-3.78*dat$newLngtClass[dat$estsciname=="Nezumia aequalis"&
                                                                     dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia aequalis"&
                 dat$LengthType=="PAFL"]<-"TL"
dat$FishLength_cm[dat$estsciname=="Nezumia sclerorhynchus"&
                    dat$LengthType=="PAFL"]<-3.98*dat$newLngtClass[dat$estsciname=="Nezumia sclerorhynchus"&
                                                                     dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia sclerorhynchus"&
                 dat$LengthType=="PAFL"]<-"TL"
#Nezumia sclerorhynchus
dat$FishLength_cm[dat$estsciname=="Nezumia"&
                    dat$LengthType=="PAFL"]<-3.88*dat$newLngtClass[dat$estsciname=="Nezumia"&
                                                                     dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Nezumia"&
                 dat$LengthType=="PAFL"]<-"TL"
# look closer at Coelorinchus caelorhincus
find<-subset(dat, estsciname=="Coelorinchus caelorhincus",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Coelorinchus caelorhincus"&
                    dat$LengthType=="PAFL"]<-dat$newLngtClass[dat$estsciname=="Coelorinchus caelorhincus"&
                                                                dat$LengthType=="PAFL"]*2.82
dat$LengthType[dat$estsciname=="Coelorinchus caelorhincus"&
                 dat$LengthType=="PAFL"]<-"TL"
# look closer at Malacocephalus laevis
find <- subset(PAFL_fish, estsciname=="Malacocephalus laevis",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Malacocephalus laevis"&
                    dat$LengthType=="PAFL"]<-dat$newLngtClass[dat$estsciname=="Malacocephalus laevis"&
                                                                dat$LengthType=="PAFL"]*4.57
dat$LengthType[dat$estsciname=="Malacocephalus laevis"&
                 dat$LengthType=="PAFL"]<-"TL"

#Atkinson 1991
dat$FishLength_cm[dat$estsciname=="Macrourus berglax"&
                    dat$LengthType=="PAFL"]<-5.2320+2.3455*dat$newLngtClass[dat$estsciname=="Macrourus berglax"&
                                                                              dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Macrourus berglax"&
                 dat$LengthType=="PAFL"]<-"TL"

# Atkinson 1981 -Coryphaenoides rupestris
find<-subset(PAFL_fish, estsciname=="Coryphaenoides rupestris",)
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
dat$FishLength_cm[dat$estsciname=="Coryphaenoides rupestris"&
                    dat$LengthType=="PAFL"]<-4.7399*dat$newLngtClass[dat$estsciname=="Coryphaenoides rupestris"&
                                                                       dat$LengthType=="PAFL"]-1.6368
dat$LengthType[dat$estsciname=="Coryphaenoides rupestris"&
                 dat$LengthType=="PAFL"]<-"TL"

#Coelorinchus labiatus Mindel et al 2015
dat$FishLength_cm[dat$estsciname=="Coelorinchus labiatus"&
                    dat$LengthType=="PAFL"]<-2.5*dat$newLngtClass[dat$estsciname=="Coelorinchus labiatus"&
                                                                    dat$LengthType=="PAFL"]
dat$LengthType[dat$estsciname=="Coelorinchus labiatus"&
                 dat$LengthType=="PAFL"]<-"TL"

summary(as.factor(dat$LengthType))
## SL to TL -------------------------------------
SL_fish<-subset(dat, LengthType=="SL",)
summary(as.factor(SL_fish$estsciname))
## Xenodermichthys copei
dat$FishLength_cm[dat$estsciname=="Xenodermichthys copei"&
                    dat$LengthType=="SL"]<-1.155*dat$newLngtClass[dat$estsciname=="Xenodermichthys copei"&
                                                                    dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Xenodermichthys copei"&
                 dat$LengthType=="SL"]<-"TL"

dat$FishLength_cm[dat$estsciname=="Alepocephalus rostratus"&
                    dat$LengthType=="SL"]<-1.127*dat$newLngtClass[dat$estsciname=="Alepocephalus rostratus"&
                                                                    dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalus rostratus"&
                 dat$LengthType=="SL"]<-"TL"

dat$FishLength_cm[dat$estsciname=="Alepocephalus bairdii"&
                    dat$LengthType=="SL"]<-1.089*dat$newLngtClass[dat$estsciname=="Alepocephalus bairdii"&
                                                                    dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalus bairdii"&
                 dat$LengthType=="SL"]<-"TL"
dat$FishLength_cm[dat$estsciname=="Alepocephalidae"&
                    dat$LengthType=="SL"]<-1.123667*dat$newLngtClass[dat$estsciname=="Alepocephalidae"&
                                                                       dat$LengthType=="SL"]
dat$LengthType[dat$estsciname=="Alepocephalidae"&
                 dat$LengthType=="SL"]<-"TL"
#dat7 <- dat
# PSCFL to TL---------------------- 
PSCFL_fish<-subset(dat, LengthType=="PSCFL",)
write.csv(PSCFL_fish, "PSCFL_fish_07-10-2016.csv")
summary(as.factor(PSCFL_fish$estsciname))
ggplot(PSCFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme_classic()

ggplot(PSCFL_fish, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=1)+
  theme_classic()

# EVHOE measures TL not PSCFL
dat$LengthType[dat$family=="Chimaeridae"&dat$Survey=="EVHOE"]<-"TL"
dat$FishLength_cm[dat$family=="Chimaeridae"&
                    dat$LengthType=="PSCFL"]<-dat$newLngtClass[dat$family=="Chimaeridae"&
                                                                 dat$LengthType=="PSCFL"]*1.31
dat$FishLength_cm[dat$estsciname=="Hydrolagus mirabilis"&
                    dat$LengthType=="PSCFL"]<-dat$newLngtClass[dat$estsciname=="Hydrolagus mirabilis"&
                                                                 dat$LengthType=="PSCFL"]*1.28
dat$LengthType[dat$family=="Chimaeridae"&
                 dat$LengthType=="PSCFL"]<-"TL"
summary(as.factor(dat$LengthType))

find<-dat[ which(dat$FishLength_cm > dat$newLngtClass),] 
summary(as.factor(find$estsciname))
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
ggplot(find, aes(x=FishLength_cm, y=Density_Km2, color=Survey, cex=1))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ estsciname, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))
ggplot(find, aes(x=newLngtClass, y=Density_Km2, color=Survey))+
  scale_colour_manual(values=cbbPalette)+
  geom_point()+
  facet_wrap(~ Year, scales="free", ncol=4)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(vjust=1))