## Header ##
## 
## Additive Intensity Index
##
## Purpose: Calculate the additive intensity index from seed output data
##
## Author: Carmen Watkins

# SET UP ####
## read in cleaned data 
source("1_data_cleaning/clean_model_dat.R")

## Additive Intensity Index (Diaz-Sierra et al. 2017)
## 2 * (deltaP / P^-N + abs(deltaP))
    ## P^-N = performance of the target species w/o neighbors
    ## deltaP = P+N - P-N = total impact of neighbors

## This script also calculates the Relative Interaction Intensity (Armas 2005)

# Calc AII ####
## BRHO ####
## start with Bromus hordeaceus focal individuals in Acmispon americanus backgrounds
## calculate mean control seeds out for use in calculations
controlsB = binter_for_AII %>%
  filter(ACAM == 0) %>% ## only want planted 0's
  group_by(water, microbe) %>%
  summarise(mean.control = mean(seeds.out.percap))

## calculate AII and RII comparing 0 background to all other densities
brho_RII = left_join(binter_for_RII_seed_analyses, controlsB, by = c("water", "microbe")) %>%
  ## join average control biomass values in with main df
  
  ## calculate the additive interaction intensity (NIntA)
  mutate(NIntA = 2 * ((seeds.out.percap - mean.control)/ (mean.control + abs((seeds.out.percap - mean.control)))), 
         
         ## also calculate the relative interaction intensity (RII) 
         RII = (seeds.out.percap - mean.control) / (mean.control + seeds.out.percap),
         
         ## rename values in columns for better visualization
         microbe = ifelse(microbe == 0, "Sterilized", "Live"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low"))) %>%
  
  ## remove contaminated samples
  filter(!unique.ID %in% rm.contaminated)

## ACAM ####
## then Acmispon americanus focal individuals in Bromus hordeaceus backgrounds
## calculate mean control seeds out for use in calculations
controlsA = ainter %>%
  filter(num.bg.indiv == 0) %>% ## only want planted 0's
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  group_by(water) %>%
  summarise(mean.control = mean(seeds.percap))

## calculate RII comparing 0 background to all other densities
acam_RII = left_join(ainter, controlsA, by = c("water")) %>%
 
  ## first calculate per-capita seed output
  mutate(seeds.percap = seeds.out/num.focal.indiv,

         ## calculate the additive interaction intensity (NIntA)
         NIntA = 2 * ((seeds.out.percap - mean.control)/ (mean.control + abs((seeds.out.percap - mean.control)))),
         
         ## also calculate the relative interaction intensity (RII) 
         RII = (seeds.percap - mean.control) / (mean.control + seeds.percap),
         
         ## rename values in columns for better visualization
         microbe = ifelse(microbe == 0, "Sterilized", "Live"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")),
       
         ## fix a mis-labeled BRHO density treatment
         BRHO = ifelse(BRHO == 48, 60, BRHO)) 

## clean up env 
rm(controlsB, controlsA)