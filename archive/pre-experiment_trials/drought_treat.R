## watering plan

# Set up environment ####
library(tidyverse)
library(lubridate)

theme_set(theme_classic())

## read in data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/trials/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/trials/"
  
} else {
  # Marina
  lead <- ""
} 

## field capacity trials
trials <- read.csv(paste0(lead, "soil_water_trials.csv")) %>%
  filter(water.saturation == 100.00) %>%
  mutate(water.saturation = 1) ## change to water proportion

## soil dry weights
dry <- read.csv(paste0(lead, "soil_dry_weight.csv"))
wt_trials <- read.csv(paste0(lead, "soil_water_texture_trial_setup.csv"))

dm <- read.csv(paste0(lead, "drought_treatment_maintenance.csv"))

## to calculate the amount of soil to add

## Sd = dry soil weight (g)
## Sd' = dry soil weight (g) measured from dry weight trials
## Sa = air dry soil weight (g)
## Sa' air dry soil weight (g) measured from dry weight trials
## Wfc = water (g) at field capacity
## Tfc = total weight at field capacity measured from field capacity trials
## Ep = empty pot weight

## Wn = water needed
## Tw = target weight of pot

# Clean Data ####
colnames(dry)

dry2 <- dry %>%
  mutate(Sa.div.Sd.p = dry_weight_g/as_is_weight_g) %>%
  group_by(material) %>%
  summarise(mean.Sa.div.Sd.p = mean(Sa.div.Sd.p))

trials2 <- trials %>%
  mutate(Sd.p.2 = soil_as_is_weight_g*dry2[dry2$material == "field soil",]$mean.Sa.div.Sd.p + sand_as_is_weight_g*dry2[dry2$material == "sand",]$mean.Sa.div.Sd.p,
         Wfc.p = pot_weight___hours_post_saturation.2 - empty_pot_weight_g - Sd.p.2,
         Wfc.p.div.Sd.p.2 = Wfc.p/Sd.p.2) %>%
  filter(Wfc.p > 200) ## get rid of the two very low 60:40 trials

sum.trials <- trials2 %>% 
  group_by(soil_sand_ratio) %>%
  summarise(mean.Wfc.p.div.Sd.p.2 = mean(Wfc.p.div.Sd.p.2))

wt_trials2 <- left_join(wt_trials, sum.trials, by = "soil_sand_ratio") %>%
  mutate(water2 = ifelse(water == 0.25, 0.5, water)) %>% ## change the 0.25 trials to 0.5 because nothing grew
  mutate(added_field_soil_weight = ifelse(is.na(added_field_soil_weight), 0, added_field_soil_weight),
         added_sand_weight = ifelse(is.na(added_sand_weight), 0, added_sand_weight)) %>%
  mutate(Sd = (soil_actual_weight_g + added_field_soil_weight)*dry2[dry2$material == "field soil",]$mean.Sa.div.Sd.p + (sand_actual_weight_g+added_sand_weight)*dry2[dry2$material == "sand",]$mean.Sa.div.Sd.p,
         Sa = (soil_actual_weight_g + added_field_soil_weight) + (sand_actual_weight_g+added_sand_weight),
         Wfc = mean.Wfc.p.div.Sd.p.2*Sd,
         Water_amt_treatment = Wfc*water2,
         Target_weight = empty_pot_weight_g + Water_amt_treatment + Sd,
         Water_needed = Target_weight - Sa - empty_pot_weight_g)

#dm2 <- dm %>%
  #select(1:5, 14:16)


final <- left_join(wt_trials2, dm, by = c("bkgrd", "focal", "water", "soil_sand_ratio", "rep")) %>%
  mutate(soil_air_dry_weight_g = soil_actual_weight_g + added_field_soil_weight,
         sand_air_dry_weight_g = sand_actual_weight_g + added_sand_weight) %>%
  select(1:6, 9, 25, 26, 20, 22:24)

plot(wt_trials2[wt_trials2$water != 1,]$Target_weight, dm[dm$water != 1,]$Target_weight)


#to_save <- wt_trials2 %>%
 # select(1:9, 13:16)

write.csv(final, paste0(lead, "drought_treatment_maintenance.csv"))

write.csv(wt_trials2, paste0(lead, "drought_treatment_maintenance_update.csv"))



## join weight trials with info from field capacity trials
wt_trials2 <- left_join(wt_trials, trials_sum, by = "soil_sand_ratio")

ggplot(wt_trials2, aes(x=soil_sand_ratio, y=mean.water.to.soil.prop.FC)) +
  geom_boxplot()

wt_trials3 <- wt_trials2 %>%
  mutate(dry.weight.mix = soil_actual_weight_g*dry2[dry2$material == "field soil",]$mean.prop.weight + sand_actual_weight_g*dry2[dry2$material == "sand",]$mean.prop.weight,
         water.at.FC = dry.weight.mix * mean.water.to.soil.prop.FC)


test <- wt_trials3 %>%
  select(1:5, 11,13:14)

ggplot(wt_trials3, aes(x=soil_sand_ratio, y=water.at.FC)) +
  geom_boxplot()

ggplot(wt_trials3, aes(x=soil_sand_ratio, y=dry.weight.mix)) +
  geom_boxplot()
         
         water.at.FC = dry.weight.mix * mean.water.to.soil.prop.FC)
         
    water_to_add = dry.weight.mix*mean.field.cap.prop)
