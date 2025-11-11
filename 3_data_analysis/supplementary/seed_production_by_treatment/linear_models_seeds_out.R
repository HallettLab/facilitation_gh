
# Set up ####
## read in data
source("data_cleaning/clean_model_dat.R")

## load packages
library(lmerTest)
library(performance)
library(multcomp)
library(emmeans)
library(car)


## read in data
#source("data_analysis/RII/RII_figures.R")

hist(binter_for_RII_seed_analyses$seeds.out.percap)
hist(log(binter_for_RII_seed_analyses$seeds.out.percap))
## log transformation could help this
hist(ainter$seeds.out.percap)
hist(log(ainter$seeds.out.percap))

# BRHO ####
## model all together
mbL1 = lm(log(seeds.out.percap) ~ num.bg.indiv + water + microbe + num.bg.indiv:water, data = binter_for_RII_seed_analyses)
summary(mbL1)
## no signif interactions

mb1 = lm(seeds.out.percap ~ num.bg.indiv + water + microbe + num.bg.indiv:water, data = binter_for_RII_seed_analyses)
summary(mb1)
## no signif interactions

AIC(mbL1)
AIC(mb1)
## oh wow, log transformed model has WAY lower AIC. seems like that is the obvi choice

check_model(mbL1)
## high VIF, but this is just due to interactions in the model.

brho_tab = as.data.frame(Anova(mbL1, type = 3, test.statistic = "F")) %>%
  mutate(species = "B. hordeaceus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
 # select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(brho_tab, "data_analysis/seeds/tables/brho_seeds_ANOVA_tab.csv", row.names = F)

# ACAM ####
## model all together
maL1 = lm(log(seeds.out.percap) ~ num.bg.indiv + water, data = ainter)
summary(maL1)

acam_tab = as.data.frame(Anova(maL1, type = 3, test.statistic = "F")) %>%
  mutate(species = "A. americanus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
  # select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(acam_tab, "data_analysis/seeds/tables/acam_seeds_ANOVA_tab.csv", row.names = F)

# Try GAMS ####
model <- gam(log(seeds.out.percap) ~ s(year_trt, sp = 10) + s(site_code, bs = 're'),
             data = binter_for_RII_seed_analyses)

# Predict y-values from model
y_plot <- predict(model, data.frame(year_trt = x_plot), exclude = "s(site_code)",
                  se.fit = TRUE, newdata.guaranteed=TRUE)

# Store predictions in output data frame
GAM_plot[(1 + out*pred_len):(pred_len + out*pred_len),] <- data.frame(x_plot, y_plot$fit, y_plot$se.fit, trt = treat[k], type = rich_type[j], metric = dat_type[i])


mbL1 = lm(log(seeds.out.percap) ~ num.bg.indiv * water * microbe, data = binter_for_RII_seed_analyses)
summary(mbL1)


