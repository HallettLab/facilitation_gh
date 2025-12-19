## Header ##
## 
## Clean Model Data
##
## Purpose: clean biomass data, calculate seed output from allometric 
## relationship, put in correct format for population models; also create 
## a dataframe for use in calculating AII 
## 
## Author: Carmen Watkins

## Outputs: 
## 
## will make per-cap seeds out column to use in modeling

# Set Up ####
library(tidyverse)

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## Read in data ####
file_loc = "~/Dropbox-UniversityofOregon/Carmen Watkins/Facilitation_GH/data/biomass/Adult/"
#file_loc = "~/University of Oregon Dropbox/Lauren Hallett/Facilitation_GH/data/biomass/Adult/"

## for BRHO models
brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20250214.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

## for ACAM models
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))
abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))

## allometry data from mega-comp
allo = read.csv("data/allometry_for_GH_facilMS.csv")

## contaminated sample IDs
source("1_data_cleaning/clean_nodule_dat.R") ## output = rm.contaminated vector

# Clean Data ####
## BRHO ####

### join brho focal data with acam bg data
  ### first, select only the needed columns from the acam bg data.
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, 
         num.resprouted.BRHO.focals, notes)

  ### then join
brho_clean = brho %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))

### check the number of brho focal individuals
    ## num.focal.indiv.in.bag = number collected at final harvest and number weighed
    ## additional focals could be ones that weren't collected due to not being ready yet
    ## resprouted focals were also not collected but should be counted
ggplot(brho_clean, aes(x=num.focal.indiv.in.bag, y = num.addl.focals)) +
  geom_point()

ggplot(brho_clean, aes(x=num.focal.indiv.in.bag, y = num.resprouted.BRHO.focals)) +
  geom_point()

## Create Data Frames ## 
## Necessary modifications: 
    ## translate biomass to seeds out
        ## using allometric relationship
    ## remove contaminated individuals
    ## account for indiv that grew but weren't collected in pots by: 
        ## creating per-capita seed output using ONLY collected focals
        ## calculating total focals in pot
        ## multiplying the per-cap amount by the total num of focal indiv
    ## select necessary columns
        ## unique.ID
        ## block, water, microbe, rep, 
        ## ACAM - i.e. the planted density of A. americans
        ## num.focal.indiv
        ## total.biomass.g
        ## num.bg.indiv

## get slope of allo relationship
alloB = allo[allo$Species == "BRHO",]$slope 

### Make AII DF ####
binter_for_AII = brho_clean %>%
  
  ## select needed cols
  select(unique.ID, block, water, microbe, rep, num.focal.indiv.in.bag,
         num.addl.focals, num.resprouted.BRHO.focals, total.biomass.g, 
         num.bg.indiv, ACAM) %>%
  
  ## fill bg.indiv with 0's where there are NA's
  mutate(num.bg.indiv = ifelse(is.na(num.bg.indiv), 0, num.bg.indiv)) %>%
  
  ## remove NA values and contaminated individuals
  filter(!is.na(total.biomass.g), !unique.ID %in% rm.contaminated) %>% 
  ## there is one NA value, remove & figure out why it is missing later!

  ## create per-cap total biomass col
  mutate(total.bio.percap = total.biomass.g/num.focal.indiv.in.bag,
         
         ## translate to seeds 
         seeds.out.percap = total.bio.percap*alloB,
         
         ## replace NA's with 0 in focal columns
         num.addl.focals = ifelse(is.na(num.addl.focals), 0, num.addl.focals),
         num.resprouted.BRHO.focals = ifelse(is.na(num.resprouted.BRHO.focals), 
                                             0, num.resprouted.BRHO.focals),
         
         ## sum all focal columns to get total number
         total.focal.indiv = (num.focal.indiv.in.bag + num.addl.focals + 
                                num.resprouted.BRHO.focals),
        
        ## multiply per cap seeds out by total num focals to get total seeds out
         seeds.out.ALL.focals = seeds.out.percap*total.focal.indiv)

### Make Model DF ####
#### brho intersp ####

## prep brho interspecific focals for modelling by removing m0, 
## and tweaking column names for consistency
binter = binter_for_AII %>%
  #filter(microbe == 1) %>%
  mutate(num.focal.indiv = total.focal.indiv,
         seeds.out = seeds.out.ALL.focals) %>%
  select(unique.ID, block, water, microbe, rep, num.focal.indiv, 
         num.bg.indiv, ACAM, seeds.out, seeds.out.percap) 

#### brho intrasp ####
## use brho bkgrd data as intraspecific brho focal data
bintra = bbkgrd %>%
  ## get rid of 0 brho backgrounds
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0) %>% 
  ## select needed columns
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, 
         total.biomass.g, num.focal.indiv, ACAM) %>%
  ## translate from biomass to seeds
  mutate(seeds.out = total.biomass.g*alloB) %>%
  select(-total.biomass.g) %>%
  
  ## swap focal and bg columns to make it fit in brho models
  mutate(num.focal.indiv2 = num.bg.indiv, ## bg brho individuals become 'focals'
         num.bg.indiv2 = num.focal.indiv, ## acam focals become 'background'
         
         ## calculate percap seeds out
         seeds.out.percap = seeds.out / num.focal.indiv2) %>% 
  select(-num.bg.indiv, -num.focal.indiv) ## remove old column names

names(bintra)
## change col names
names(bintra) = c("unique.ID", "block", "water", "microbe", "rep", "ACAM", 
                  "seeds.out", "num.focal.indiv", "num.bg.indiv", 
                  "seeds.out.percap")

#### join ####
## join brho focal and brho bkgrd data
brho.model = rbind(binter, bintra) %>%
  select(-ACAM) %>% ## this col is only needed for RII analyses not for models
  mutate(soil = ifelse(microbe == 1, 0, 1))
  ## flip the microbe treatment indicators for modeling; 
    ## want default (0) to be live soil condition while 1 should indicate 
    ## sterilized so it calcs the deviation parameter for sterilized soil.

## ACAM ####
names(abkgrd)
names(acam)
## already have num.bg.indiv; don't need to join bbkgrd with acam because of this

## select necessary columns; translate biomass to seeds out
alloAf = allo[allo$Species == "ACAM",]$slope
alloAs = allo[allo$Species == "ACAM",]$seeds_C
#alloAsD = allo[allo$Species == "ACAM",]$seeds_D

### acam intersp ####
ainter = acam %>%
  
  ## select needed columns
  select(unique.ID, block, water, microbe, BRHO, rep, num.focal.indiv,
         total.biomass.g, num.bg.indiv) %>%
  
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

### acam intrasp ####
## change acam bkgrd data to use as intraspecific acam data
aintra = abkgrd %>%
  
  ## get rid of 0 background pots
  filter(!is.na(num.bg.indiv), num.bg.indiv != 0, num.bg.indiv != 1) %>%
  
  ## select only needed columns
  select(unique.ID, block, water, microbe, rep, num.bg.indiv, 
         total.biomass.g) %>%
  
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
  left_join(binter[ , -c(8:10)], by = c("unique.ID", "block", "water", 
                                        "microbe", "rep", "num.bg.indiv")) %>%

  ## swap bg column for focal column & vice-versa
  mutate(num.focal.indiv2 = num.bg.indiv,
         num.bg.indiv2 = num.focal.indiv,
         seeds.out.percap = seeds.out / num.focal.indiv2) %>%
  select(-num.bg.indiv, -num.focal.indiv, -total.biomass.g) %>%
  
  filter(unique.ID != 172) ## BRHO was not sampled in this pot; 

names(aintra)
## rename columns
names(aintra) = c("unique.ID", "block", "water", "microbe", "rep", "seeds.out", 
                  "num.focal.indiv", "num.bg.indiv", "seeds.out.percap")

names(ainter)

### join ####
## join into one df for model
ainter2 = ainter %>%
  select(-BRHO)
aintra2 = aintra %>%
  filter(microbe == 1) ## need to filter here, because aintra have microbe 0, 
                      ## while ainter do not
acam.model = rbind(ainter2, aintra2) %>%
  mutate(soil = ifelse(microbe == 1, 0, 1))
## flip the microbe treatment indicators for modeling; 
## want default (0) to be live soil condition while 1 should indicate 
## sterilized so it calcs the deviation parameter for sterilized soil.

# clean env ####
rm(abkgrd, abkgrd.join, acam, aintra2, ainter2, allo, bbkgrd, brho, brho_clean, 
   alloB, alloAf, alloAs)
