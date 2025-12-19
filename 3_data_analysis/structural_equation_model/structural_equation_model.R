## Header ##
## 
## Structural Equation Model
##
## Purpose: Run the structural equation model and test the fit of the model.
##
## Author: Carmen Watkins

# Set Up ####
## load packages 
library(piecewiseSEM)
library(lmerTest)
library(lme4)
library(GGally)
library(multcompView)

## load AII & treatment data
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")

## load nutrient data
source("1_data_cleaning/clean_CN_dat.R")
rm(CN_clean) ## this contains multiple timepoints; use only final time point in analyses  

## prep data ####
## join intrxn & nutrient data together
test = left_join(brho_AII, CN_final[,1:7], by = c("unique.ID")) %>%
  filter(!is.na(delta13C))
## get rid of these data for the moment... 
## can't think of a way of including them yet

## will be using only brho AII for this as we do not have leaf nutrient data for ACAM

## check for correlation, VIF, and multicolinearity
## with all data
ggpairs(test,
        columns = c(10,17, 21, 22),
        upper = list(continuous = "cor"))
ggsave("corrmatrixall.png", height = 6, width = 8)

## then with cows model
ggpairs(test,
        columns = c(10,17, 21, 22),
        upper = list(continuous = "cor"),
        mapping = aes(color = water), 
        title = "Water")
#ggsave("corrmatrixcows.png", height = 6, width = 8)

## possible model structure
    ## this is concerning as delta13C and WtN are not present in all rows; how does this affect the way the model runs...? may need to specify some way of having the model not remove data
    ## currently starting with an additive model and seeing if water level affects the multigroup analysis
    ## shoudl think if there is scientific reason for interactions between factors and justify before running model either way...

sem1 = psem(lmer(NIntA ~ water + microbe + num.bg.indiv + delta13C + WtN + (1|block), data = test),

    # lm(NIntA ~ delta13C + WtN, data = test),
     
     lmer(WtN ~ water + microbe + num.bg.indiv + (1|block), data = test),

     lmer(delta13C ~ water + microbe + num.bg.indiv + (1|block), data = test),

     lmer(num.bg.indiv ~ water + (1|block), data = test),
     
     data = test)
summary(sem1)

(pmultigroup <- multigroup(pmodel, group = "group"))


t1 = lmer(NIntA ~ water + microbe + num.bg.indiv + delta13C + WtN + (1|block), data = test)
summary(t1)


### Multi-group analysis
## Goal: to identify whether a single global model is sufficient to describe the data, or whether some or all paths vary by some grouping variable

## unclear if this can be done with a categorical variable with 3 levels...?


## Cows: with richness -> synchrony pathway
cows_psem_w <- psem(
  
  lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(cows_psem_w)

vif(lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10))




















