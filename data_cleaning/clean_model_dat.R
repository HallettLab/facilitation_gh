## Header ##
## 
## Clean Focal species data 
##
## Purpose: clean biomass data, calculate seed output from allometric relationship, put in correct format for population models; also create a df for use in calculating RII later on
## 
## Author: Carmen Watkins

# Set Up ####
library(tidyverse)

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## Read in data ####
file_loc = "/Users/carme/University of Oregon Dropbox/Carmen Watkins/Facilitation_GH/data/biomass/Adult/"

## for BRHO models
brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

## for ACAM models
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))
abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))

## allometry data from mega-comp
allo = read.csv("data/allometry_for_GH_facilMS.csv")

# Clean Data ####
## brho ####
## join in acam background data 
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, num.resprouted.BRHO.focals)

brho_clean = brho %>%
  mutate(unique.ID = X0) %>%
  select(-X0) %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))

## Necessary columns
## unique.ID
## block, water, microbe, rep, 
## ACAM - the planted density
## num.focal.indiv
## total.biomass.g
## num.bg.indiv

## select necessary columns; translate biomass to seeds out
alloB = allo[allo$Species == "BRHO",]$slope ## get slope of allo relationship

## contaminated samples to remove
rm.contaminated = c(15, 25, 85, 114)
    ## some contamination info in the nodule counts data sheet
    ## some in the experimental design spreadsheet still located in the google drive
    ## this will eventually be updated when nodule counts are finished

binter = brho_clean %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, total.biomass.g, num.bg.indiv, ACAM) %>%
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  filter(!is.na(total.biomass.g), !unique.ID %in% rm.contaminated) %>% ## there is one NA value, remove & figure out why it is missing later!
  mutate(seeds.out = total.biomass.g*alloB)

## plot distribution of seeds out
ggplot(binter, aes(x=seeds.out)) +
  geom_histogram(bins = 100)

## take the log of seeds out
ggplot(binter, aes(x=log(seeds.out))) +
  geom_histogram(bins = 100)

## change brho bkgrd data to use as intraspecific brho data
bintra = bbkgrd %>%
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0) %>% ## get rid of 0 brho backgrounds
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g, num.focal.indiv, ACAM) %>%
  mutate(seeds.out = total.biomass.g*alloB)

names(bintra)
## change col names
names(bintra) = c("unique.ID", "block", "water", "microbe", "rep", "num.focal.indiv", "total.biomass.g", "num.bg.indiv", "ACAM", "seeds.out")

## join brho focal and brho bkgrd data
brho.model = rbind(binter, bintra) %>%
  select(-ACAM) ## this col is only needed for analyses with RII not for pop models; although potentially we want to use seeds in??

## acam ####
names(abkgrd)
names(acam)
## already have num.bg.indiv; don't need to join bbkgrd with acam because of this

## Necessary columns
## unique.ID
## block, water, microbe, rep, 
## num.focal.indiv
## total.biomass.g
## num.bg.indiv

## select necessary columns; translate biomass to seeds out
alloAf = allo[allo$Species == "ACAM",]$slope
alloAsC = allo[allo$Species == "ACAM",]$seeds_C
alloAsD = allo[allo$Species == "ACAM",]$seeds_D

ainter = acam %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, total.biomass.g, num.bg.indiv) %>%
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  filter(!is.na(total.biomass.g))  %>% ## there is one NA value, remove & figure out why it is missing later!
  mutate(flowers.out = total.biomass.g*alloAf, 
         seeds.out = ifelse(water %in% c(0.75, 1), flowers.out*alloAsC, flowers.out*alloAsD)) %>%
  select(-flowers.out)

ggplot(ainter, aes(x=seeds.out, fill = as.factor(water))) +
  geom_histogram(bins = 50) +
  facet_wrap(~water)

## change acam bkgrd data to use as intraspecific acam data
names(abkgrd)

aintra = abkgrd %>%
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0, num.bg.indiv != 1) %>%
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g) %>%
  mutate(total.biomass.g = ifelse(unique.ID == 250, 3.662, total.biomass.g), ## fix a value missing a decimal place
         flowers.out = total.biomass.g*alloAf, 
         seeds.out = ifelse(water %in% c(0.75, 1), flowers.out*alloAsC, flowers.out*alloAsD)) %>%
  #filter(seeds.out < 550) %>% ## remove the one crazy big observation; look into later 
  select(-flowers.out) %>%
  ## join with binter to get number of focal individuals
  left_join(binter[ , -c(7,9,10)], by = c("unique.ID", "block", "water", "microbe", "rep", "num.bg.indiv")) 

## there are a number of these with missing # focal individuals

names(aintra) = c("unique.ID", "block", "water", "microbe", "rep", "num.focal.indiv", "total.biomass.g", "seeds.out", "num.bg.indiv")

names(ainter)

acam.model = rbind(ainter, aintra)

# clean env ####
rm(abkgrd, abkgrd.join, acam, aintra, ainter, allo, bbkgrd, bintra, brho, brho_clean, alloB, alloAf, alloAsC, alloAsD)
