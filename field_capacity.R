## Script purpose: Calculate the Field capacity of SFREC soil for the official experiment
## will calculate field capacity as total weight (soil + water) - dry soil weight = water weight at field capacity

## set up environment
library(tidyverse)
library(lubridate)

theme_set(theme_classic())

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/set_up/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/set_up/"
  
} else {

    lead <- ""
} 

## read in data
## soil dry weights
dry <- read.csv(paste0(lead, "soil_dry_weight.csv"))

## field capacity trials
trials <- read.csv(paste0(lead, "field_capacity.csv")) %>%
  filter(rep != 2) ## remove a rep that was not done correctly

# Calculate #### 
# Use the weight of the soil at field capacity minus the dry weight of the soil to calculate the amount of water it holds at field capacity.

## calculate fraction of soil that is dry weight
dry2 <- dry %>%
  group_by(Material) %>%
  summarise(mean.prop.weight = mean(Prop_dry_weight))

## calculate field capacity & prep for graphing
trials2 <- trials %>%
  
  mutate(dry.weight.mix = STERILE_soil_weight_g*dry2[dry2$Material == "soil",]$mean.prop.weight + sand_weight_g*dry2[dry2$Material == "sand",]$mean.prop.weight, 
         
         ## calculate the soil/sand dry weights using prop of dry soil from trials
         
         field.cap = pot_weight_24_hours - dry.weight.mix - empty_pot_weight_g,
  
        ## water amount in g in the soil at field capacity
         
         water.to.soil.prop.FC = field.cap/dry.weight.mix)


ggplot(trials2, aes(x=rep, y=field.cap)) + 
  geom_bar(stat = 'identity')

