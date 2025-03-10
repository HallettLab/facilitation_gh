
## Carmen's code for making the data fram
## this won't run on your computer
# source("data_cleaning/clean_model_dat.R")


#dat = binter_for_RII_seed_analyses %>%
 # select(unique.ID, block, water, microbe, rep, num.focal.indiv.in.bag, num.addl.focals, total.bio.percap, seeds.out.percap, ACAM, num.bg.indiv) %>%
  #filter(microbe == 1, water %in% c(0.6, 1),
   #      ACAM %in% c(0, 12, 24))

#write.csv(dat, "data/biomass_seed_data_for_Tetianna.csv", row.names = F)
library(tidyverse)
theme_set(theme_classic())

dat = read.csv("data/biomass_seed_data_for_Tetianna.csv")

## read in Data here!! 


## plot biomass by water treatment
dat %>%
  mutate(trt = paste0(water, "_", ACAM)) %>%
ggplot(aes(x=as.factor(water), y=total.bio.percap, color = as.factor(ACAM))) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Water") +
  ylab("Aboveground Biomass per-capita (g)")

ggsave("biomass_water.png", width = 4, height = 3)
