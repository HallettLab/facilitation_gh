## Header ##
## 
## Clean Focal species data ##
## 
## Author: Carmen Watkins

# Set Up ####
library(tidyverse)

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## Read in data
file_loc = "/Users/carme/University of Oregon Dropbox/Carmen Watkins/Facilitation_GH/data/biomass/Adult/"

brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))

abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

# Clean Data ####
## brho focal ####
names(brho)

hist(brho$total.biomass.g)
hist(brho$inflor.g)
unique(brho$processing.notes)

unique(brho$water)
unique(brho$microbe)
unique(brho$bkgrd)
unique(brho$ACAM)

## calculate per capita biomass
names(abkgrd)
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, num.resprouted.BRHO.focals)

brho_clean = brho %>%
  mutate(tot.bio.percap = total.biomass.g/num.focal.indiv, 
         unique.ID = X0)  %>%
  select(-X0) %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))

names(brho_clean)

## unique.ID
## block, water, microbe, rep, 
## num.focal.indiv
## total.biomass.g
## num.bg.indiv

brho.model = brho_clean %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, total.biomass.g, inflor.g, num.bg.indiv) %>%
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  filter(water == 1, microbe == 1) %>%
  filter(!is.na(total.biomass.g))  %>% ## there is one NA value, remove & figure out why it is missing later!
  mutate(seeds.out = total.biomass.g*951.729666331095)

## 951.729666331095 slope of BRHO allo 

ggplot(brho.model, aes(x=seeds.out)) +
  geom_histogram(bins = 100)





