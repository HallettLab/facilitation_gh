## Header ##
## 
## Clean Model Data
##
## Purpose: clean biomass data, calculate seed output from allometric relationship, put in correct format for population models; also create a df for use in calculating RII later on
## 
## Author: Carmen Watkins

## NOTES: 
## will remove m0 data from model df
## will make per-cap seeds out column to use in modeling

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
brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20250214.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

## for ACAM models
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))
abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))

## allometry data from mega-comp
allo = read.csv("data/allometry_for_GH_facilMS.csv")

# Clean Data ####
## brho ####
### join with acam bg dat ####
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, num.resprouted.BRHO.focals, notes)

brho_clean = brho %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))

### check brho focal nums ####
ggplot(brho_clean, aes(x=num.focal.indiv.in.bag, y = num.addl.focals)) +
  geom_point()

ggplot(brho_clean, aes(x=num.focal.indiv.in.bag, y = num.resprouted.BRHO.focals)) +
  geom_point()

## to account for indiv that grew but weren't collected in pots: 
## create a per-capita seed output 
## multiply the per-cap amt by the total num of focal indiv

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
rm.contaminated = c(15, 25, 84, 85, 114, 103)
## some contamination info in the nodule counts data sheet
## some in the experimental design spreadsheet still located in the google drive
## this will eventually be updated when nodule counts are finished
## 84 is taken from AMF colonization data sheet

### binter for model ####
### create RII df ####
binter = brho_clean %>%
  
  ## select needed cols
  select(unique.ID, block, water, microbe, rep, num.focal.indiv.in.bag,num.addl.focals, num.resprouted.BRHO.focals, total.biomass.g, num.bg.indiv, ACAM) %>%
  
  ## fill bg.indiv with 0's where there are NA's
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  
  filter(!is.na(total.biomass.g), !unique.ID %in% rm.contaminated) %>% 
  ## there is one NA value, remove & figure out why it is missing later!
  ## also remove contaminated samples
  
  ## create per-cap total biomass col
  mutate(total.bio.percap = total.biomass.g/num.focal.indiv.in.bag,
         seeds.out.percap = total.bio.percap*alloB,
         
         ## determine correct num focals
         ## replace NA's with 0 in focal columns
         num.addl.focals = ifelse(is.na(num.addl.focals), 0, num.addl.focals),
         num.resprouted.BRHO.focals = ifelse(is.na(num.resprouted.BRHO.focals), 0, num.resprouted.BRHO.focals),
         
         ## sum all focal columns to get total #
         total.focal.indiv = num.focal.indiv.in.bag + num.addl.focals + num.resprouted.BRHO.focals,
         
         ## calc seeds out of all focals
         seeds.out.ALL.focals = seeds.out.percap*total.focal.indiv) %>%
  mutate(num.focal.indiv = total.focal.indiv,
         seeds.out = seeds.out.ALL.focals) %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, num.bg.indiv, ACAM, seeds.out, seeds.out.percap)


## plot distribution of seeds out
ggplot(binter, aes(x=seeds.out)) +
  geom_histogram(bins = 100)

## take the log of seeds out
ggplot(binter, aes(x=log(seeds.out))) +
  geom_histogram(bins = 100)

### bintra for model ####
## change brho bkgrd data to use as intraspecific brho data
bintra = bbkgrd %>%
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0) %>% ## get rid of 0 brho backgrounds
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g, num.focal.indiv, ACAM) %>%
  mutate(seeds.out = total.biomass.g*alloB) %>%
  select(-total.biomass.g) %>%
  
  ## swap focal and bg columns to make it fit in brho models
  mutate(num.focal.indiv2 = num.bg.indiv,
         num.bg.indiv2 = num.focal.indiv,
         seeds.out.percap = seeds.out / num.focal.indiv2) %>%
  select(-num.bg.indiv, -num.focal.indiv)

names(bintra)
## change col names
names(bintra) = c("unique.ID", "block", "water", "microbe", "rep", "ACAM", "seeds.out", "num.focal.indiv", "num.bg.indiv", "seeds.out.percap")

## join brho focal and brho bkgrd data
brho.model = rbind(binter, bintra) %>%
  select(-ACAM) %>% ## this col is only needed for analyses with RII not for pop models; although potentially we want to use seeds in??
  mutate(soil = ifelse(microbe == 1, 0, 1)) %>% ## flip the microbe treatment indicators for modeling; want default (0) to be live soil condition while 1 should indicate sterilized so it calcs the deviation parameter for sterilized soil.
  filter(!unique.ID %in% rm.contaminated)
## triple check that these are not in ehre



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
alloAs = allo[allo$Species == "ACAM",]$seeds_C
#alloAsD = allo[allo$Species == "ACAM",]$seeds_D

### ainter for model ####
ainter = acam %>%
  
  ## select needed columns
  select(unique.ID, block, water, microbe, BRHO, rep, num.focal.indiv, total.biomass.g, num.bg.indiv) %>%
  
  ## change NA's to 0 in bg indiv column
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  
  ## remove missing samples
  filter(!is.na(total.biomass.g))  %>% 
  
  ## calc seed output with allo relationships
  mutate(flowers.out = total.biomass.g*alloAf, 
         seeds.out = flowers.out*alloAs,
         
         ## calc seeds out per-capita
         seeds.out.percap = seeds.out / num.focal.indiv) %>%
  
  ## remove unnecessary columns
  select(-flowers.out, -total.biomass.g)

ggplot(ainter, aes(x=seeds.out, fill = as.factor(water))) +
  geom_histogram(bins = 50) +
  facet_wrap(~water)

### aintra for model ####
## change acam bkgrd data to use as intraspecific acam data
names(abkgrd)

aintra = abkgrd %>%
  
  ## get rid of 0 background pots
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0, num.bg.indiv != 1) %>%
  
  ## select only needed columns
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, total.biomass.g) %>%
  
  ## fix a value missing a decimal place
  mutate(total.biomass.g = ifelse(unique.ID == 250, 3.662, total.biomass.g), 
         
         ## use allo to calc total num flowers
         flowers.out = total.biomass.g*alloAf,
         
         ## use allo to calc total num seeds
         seeds.out = flowers.out*alloAs) %>%
  
  ## get rid of flowers out column
  select(-flowers.out) %>%
  
  ## join with binter to get number of BRHO indiv in each pot
  ## join but exclude binter$seeds.out and binter$seeds.out.percap
  ## these cols refer to BRHO seeds which we do NOT want here
  left_join(binter[ , -c(8:10)], by = c("unique.ID", "block", "water", "microbe", "rep", "num.bg.indiv")) %>%
  # filter(microbe == 1) %>%  ## have to filter here, do NOT want microbe 0 pots in models
  
  ## swap bg column for focal column & vice-versa
  mutate(num.focal.indiv2 = num.bg.indiv,
         num.bg.indiv2 = num.focal.indiv,
         seeds.out.percap = seeds.out / num.focal.indiv2) %>%
  select(-num.bg.indiv, -num.focal.indiv, -total.biomass.g) %>%
  
  filter(unique.ID != 172) ## BRHO was not sampled in this pot; need to fill in # of BRHO that survived, then could potentially use as an ACAM background plot! 
names(aintra)
names(aintra) = c("unique.ID", "block", "water", "microbe", "rep", "seeds.out", "num.focal.indiv", "num.bg.indiv", "seeds.out.percap")

names(ainter)

rm.contaminated = c(15, 25, 84, 85, 114, 103)

ainter2 = ainter %>%
  select(-BRHO)
#aintra2 = aintra %>% ## get rid of this part as we no longer want to filter by microbe treatment
  #filter(microbe == 1)
acam.model = rbind(ainter2, aintra) %>%
  mutate(soil = ifelse(microbe == 1, 0, 1)) %>% ## flip the microbe treatment indicators for modeling; want default (0) to be live soil condition while 1 should indicate sterilized so it calcs the deviation parameter for sterilized soil.
  filter(!unique.ID %in% rm.contaminated)

# clean env ####
rm(abkgrd, abkgrd.join, acam, ainter2, allo, bbkgrd, brho, brho_clean, alloB, alloAf, alloAs, ainter, aintra, binter, bintra)
