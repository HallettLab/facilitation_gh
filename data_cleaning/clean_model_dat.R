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

## for BRHO models
brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

## for ACAM models
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))
abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))


# Clean Data ####
## brho ####
## join in acam background data 
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, num.resprouted.BRHO.focals)

brho_clean = brho %>%
  select(-X0) %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))

## Necessary columns
## unique.ID
## block, water, microbe, rep, 
## num.focal.indiv
## total.biomass.g
## num.bg.indiv

## select necessary columns; translate biomass to seeds out
binter = brho_clean %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, total.biomass.g, num.bg.indiv) %>%
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  filter(!is.na(total.biomass.g)) %>% ## there is one NA value, remove & figure out why it is missing later!
  mutate(seeds.out = total.biomass.g*388.25)
## 388.25 slope of BRHO allo b/w total bio and seeds out

ggplot(binter, aes(x=seeds.out)) +
  geom_histogram(bins = 100)

## change brho bkgrd data to use as intraspecific brho data
bintra = bbkgrd %>%
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0) %>% ## get rid of 0 brho backgrounds
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g, num.focal.indiv) %>%
  mutate(seeds.out = total.biomass.g*388.25)

## change col names
names(bintra) = c("unique.ID", "block", "water", "microbe", "rep", "num.focal.indiv", "total.biomass.g", "num.bg.indiv", "seeds.out")

## join brho focal and brho bkgrd data
brho.model = rbind(binter, bintra)

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

acam.model = acam %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, total.biomass.g, num.bg.indiv) %>%
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  ## filter(water == 1, microbe == 1) %>%
  filter(!is.na(total.biomass.g))  %>% ## there is one NA value, remove & figure out why it is missing later!
  mutate(flowers.out = total.biomass.g*34.9341448598145, 
         seeds.out = flowers.out * 1.66666666666667) 

ggplot(acam.model, aes(x=seeds.out)) +
  geom_histogram(bins = 100)

names(abkgrd)

aintra = abkgrd %>%
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0) %>%
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g) %>%
  mutate(flowers.out = total.biomass.g*34.9341448598145, 
         seeds.out = flowers.out * 1.66666666666667) %>%
  filter(seeds.out > 550) %>% ## remove the one crazy big observation; look into later 
  select(-flowers.out)

names(aintra) = c("unique.ID", "block", "water", "microbe", "rep", "num.focal.indiv", "total.biomass.g", "seeds.out")

names(brho.model)

brho.all = rbind(brho.model, bintra)



# clean env ####
rm()
