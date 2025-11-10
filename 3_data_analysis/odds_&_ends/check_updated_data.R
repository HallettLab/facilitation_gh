
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

brho_old = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))

brho_comb = left_join(brho, brho_old, by = c("unique.ID", "block", "water", "microbe", "rep", "bkgrd", "focal", "ACAM", "BRHO"))


ggplot(brho_comb, aes(x=num.focal.indiv, y=num.focal.indiv.in.bag)) +
  geom_point()

## check 
test = brho_comb %>%
  filter((num.focal.indiv == 2 & num.focal.indiv.in.bag == 1) | (num.focal.indiv == 3 & num.focal.indiv.in.bag == 2 ) )






