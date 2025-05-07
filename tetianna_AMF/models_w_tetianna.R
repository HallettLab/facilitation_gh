
## load data in
source("calculating_perc_colonization.R")

library(car)

## use AMF Results df

## Questions: 
## is there a difference in AMF colonization between ACAM densities?
## is there a difference in AMF colonization between water levels?

## AMF colonization (response) - continuous
## water (pred) - categorical (just 2 values)
## density (pred) - categorical (3 values)

## tests to consider
## ANOVA 
## linear model

check_num_reps = AMF_results %>%
  group_by(ACAM_density, water) %>%
  summarise(reps = n())

dens24 = AMF_results %>%
  filter(water == 0.6, ACAM_density == 24)

test = draftAMF %>%
  filter(Slide_ID %in% c("246", "246 B"))

hist(log(AMF_results$percent_colonization))


## need to fix later
set.seed(0)

m1 = aov(percent_colonization ~ as.factor(water)*as.factor(ACAM_density), data = AMF_results)

summary(m1)
AIC(m1)
## why are anova vals slightly different?

m2 = aov(percent_colonization ~ as.factor(water)+as.factor(ACAM_density), data = AMF_results)

summary(m2)
AIC(m2)
anova.df = as.data.frame(Anova(m2)) %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(anova.df, "tetianna_AMF/tables/anova_perc_coln.csv")
## other response variables
## leaf N data
## biomass data





