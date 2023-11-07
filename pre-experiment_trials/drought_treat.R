## watering plan

## set up environment
library(tidyverse)
library(lubridate)

theme_set(theme_classic())

## read in data
## field capacity trials
trials <- read.csv("soil_water_trials.csv") %>%
  filter(water.saturation == 100.00) %>%
  mutate(water.saturation = 1)

## soil dry weights
dry <- read.csv("soil_dry_weight.csv")
wt_trials <- read.csv("soil_water_texture_trials.csv")

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

colnames(dry)

dry2 <- dry %>%
  mutate(Sa.prime = as_is_weight_g,
         Sd.prime = dry_weight_g,
         Sa.div.Sd.prime = Sd.prime/Sa.prime) %>%
  group_by(material) %>%
  summarise(mean.Sa.div.Sd.prime = mean(Sa.div.Sd.prime))

trials2 <- trials %>%
  mutate(Sd.prime.2 = soil_as_is_weight_g*dry2[dry2$material == "field soil",]$mean.Sa.div.Sd.prime + sand_as_is_weight_g*dry2[dry2$material == "sand",]$mean.Sa.div.Sd.prime,
         Wfc.prime = pot_weight___hours_post_saturation.2 - empty_pot_weight_g - Sd.prime.2,
         Wfc.prime.div.Sd.prime.2 = Wfc.prime/Sd.prime.2)

sum.trials <- trials2 %>%
  group_by(soil_sand_ratio) %>%
  summarise(mean.Wfc.prime.div.Sd.prime.2 = mean(Wfc.prime.div.Sd.prime.2))

wt_trials2 <- left_join(wt_trials, sum.trials, by = "soil_sand_ratio") %>%
  mutate(Sd = soil_actual_weight_g*dry2[dry2$material == "field soil",]$mean.Sa.div.Sd.prime + sand_actual_weight_g*dry2[dry2$material == "sand",]$mean.Sa.div.Sd.prime,
         Sa = soil_actual_weight_g + sand_actual_weight_g,
         Wfc = mean.Wfc.prime.div.Sd.prime.2*Sd,
         Wt = Wfc*water,
         Tw = empty_pot_weight_g + Wt + Sd,
         Wn = Tw - Sa - empty_pot_weight_g)

to_save <- wt_trials2 %>%
  select(1:9, 14:17)

write.csv(to_save, "drought_treatment_maintenance.csv")




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


#%>%
  #group_by(soil_sand_ratio) %>%
  #mutate()

ggplot(wt_trials3, aes(x=soil_sand_ratio, y=dry.weight.mix)) +
  geom_boxplot()
         
         water.at.FC = dry.weight.mix * mean.water.to.soil.prop.FC)
         
    water_to_add = dry.weight.mix*mean.field.cap.prop)
