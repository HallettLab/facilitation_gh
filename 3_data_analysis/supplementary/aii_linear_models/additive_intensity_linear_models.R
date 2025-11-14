## Header ##
## 
## Additive Intensity Index Linear Models
##
## Purpose: Run Linear models to test the effect of density, water, and microbial treatment on the additive intensity index
##
## Author: Carmen Watkins


# Set up ####
## read in data
source("1_data_cleaning/calc_additive_intensity_index.R")

## load packages
library(lmerTest)
library(performance)
library(multcomp)
library(emmeans)
library(car)


# BRHO ####
hist(brho_RII$NIntA)

mod = brho_RII %>%
  filter(ACAM != 0)

## model all together
#m1 = lm(NIntA ~ water + num.bg.indiv + microbe + num.bg.indiv:water + water:microbe + microbe:num.bg.indiv, data = brho_RII)
#summary(m1) 

## don't use this model, no signif interactions, so no sense keeping them in the model

m2 = lm(NIntA ~ num.bg.indiv + water + microbe, data = brho_RII)
summary(m2)

as.data.frame(m2$coefficients)

#m2 = lmer(NIntA ~ ACAM * water * microbe + (1|block), data = brho_RII)
#summary(m2)

#AIC(m1) ## this model is preferred by having more negative AIC value
#AIC(m2)

brho_coeff = as.data.frame(summary(m2)$coeff) %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(brho_coeff, "tables/brho_AII_dens_water_lm_coeff_tab_20250429.csv", row.names = F)

#check_model(m2)

brho_tab = as.data.frame(Anova(m2, type = 2, test.statistic = "F")) %>%
  mutate(species = "B. hordeaceus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
#  select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(brho_tab, "tables/brho_AII_ANOVA_tab_20250429.csv", row.names = F)

# ACAM ####
m1a = lm(NIntA ~ num.bg.indiv + water, data = acam_RII)
summary(m1a)

acam_tab = as.data.frame(Anova(m1a, type = 2, test.statistic = "F")) %>%
  mutate(species = "A. americanus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
  select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(acam_tab, "data_analysis/RII/tables/acam_ANOVA_tab.csv", row.names = F)

