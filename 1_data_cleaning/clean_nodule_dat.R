## Header ##
## 
## Clean Nodule Data
##
## Purpose: clean nodule data and determine which samples are contaminated.
## create a vector of contaminated sample ID's to use in cleaning seed data.
##
## Author: Carmen Watkins
## 
## NOTES: 

# Set Up ####
## load packages
library(tidyverse)

theme_set(theme_classic())

## Read in data ####
nod_loc = "~/Dropbox-UniversityofOregon/Carmen Watkins/Facilitation_GH/data/"

## most contaminant data in the nodule counts file
nods = read.csv(paste0(nod_loc, "nodules/nodule_counts_20251219.csv"))

## some contaminated pots were noted at another time (possibly during sampling or root washing and added to experimental design sheet at the time)
expdes = read.csv(paste0(nod_loc, "experimental_design_20250104.csv"))

## find contaminated samples
unique(nods$nodules.present)
## only want "Y" answers; 
## when blanks, assume wasn't sampled

contaminated = nods %>%
  filter(microbe == 0 & nodules.present == "Y")

names(expdes)

contaminated2 = expdes %>%
  filter(microbe == 0 & nodules_present == "Y")

## combine into one vector of unique sample ID numbers
rm.contaminated = c(unique(contaminated$unique.ID), 
                    unique(contaminated2$unique.ID))
  

## clean env 
rm(nods, expdes, contaminated, contaminated2, nod_loc)
