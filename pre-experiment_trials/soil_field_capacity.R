## Script purpose: Calculate the Field capacity of SFREC soil at various ratios of field soil: sand
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
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/Trials/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/Trials/"
  
} else {
  # Marina
  lead <- ""
} 


## read in data
## soil dry weights
dry <- read.csv(paste0(lead, "soil_dry_weight.csv"))

## perlite dry weight
perdry <- read.csv(paste0(lead, "perlite_dry_weights.csv"))

## field capacity trials
trials <- read.csv(paste0(lead, "soil_water_trials.csv")) %>%
  filter(water.saturation == 100.00) %>%
  mutate(water.saturation = 1)

pertrials <- read.csv(paste0(lead, "perlite_setup.csv"))


# Calculate #### 
# Use the weight of the soil at field capacity minus the dry weight of the soil to calculate the amount of water it holds at field capacity.

## calculate fraction of soil that is dry weight
dry2 <- dry %>%
  mutate(prop.weight = dry_weight_g/as_is_weight_g) %>%
  group_by(material) %>%
  summarise(mean.prop.weight = mean(prop.weight))

perdry2 <- perdry %>%
  mutate(prop.weight = perlite_dry_weight/air_dry_weight_g) %>%
  group_by(material) %>%
  summarise(mean.prop.weight = mean(prop.weight))

## calculate field capacity & prep for graphing
trials2$soil_sand_ratio <- as.factor(trials2$soil_sand_ratio) ## change soil:sand ratio to factor

trials2 <- trials %>%
  mutate(dry.weight.mix = soil_as_is_weight_g*dry2[dry2$material == "field soil",]$mean.prop.weight + sand_as_is_weight_g*dry2[dry2$material == "sand",]$mean.prop.weight, ## calculate the soil dry weight using prop of dry soil from trials
         
         field.cap = pot_weight___hours_post_saturation.2 - dry.weight.mix - empty_pot_weight_g) %>% ## water amount in g in the soil at field capacity
  
  mutate(soil_sand_ratio = fct_relevel(soil_sand_ratio, "100_0", "75_25", "60_40", "50_50"),
         
         #field.cap.prop = field.cap/(pot_weight___hours_post_saturation.2 - empty_pot_weight_g),  
         water.to.soil.prop.FC = field.cap/dry.weight.mix)

trials_sum <- trials2 %>%
  group_by(soil_sand_ratio) %>%
  summarise(
    
    #mean.field.cap = mean(field.cap), se.field.cap = calcSE(field.cap), 
            #mean.VWC = mean(VWC___hours_post_saturation.2), se.VWC = calcSE(VWC___hours_post_saturation.2),
            #mean.field.cap.prop = mean(field.cap.prop), se.field.cap.prop = calcSE(field.cap.prop),
            mean.water.to.soil.prop.FC = mean(water.to.soil.prop.FC), se.water.to.soil.prop.FC = calcSE(water.to.soil.prop.FC))


pertrials2 <- pertrials  %>%
  mutate(dry.weight.mix = soil_amount_g*dry2[dry2$material == "field soil",]$mean.prop.weight + sand_amount_g*dry2[dry2$material == "sand",]$mean.prop.weight + perlite_amount_g*perdry2[perdry2$material == "perlite",]$mean.prop.weight, ## calculate the soil dry weight using prop of dry soil from trials
         
         field.cap = total_pot_weight.1 - dry.weight.mix - empty_pot_weight_g,## water amount in g in the soil at field capacity
         
         water.to.soil.prop.FC = field.cap/dry.weight.mix)
  
# Visualize ####
ggplot(trials_sum, aes(x=soil_sand_ratio, y=mean.field.cap)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.field.cap - se.field.cap, ymax = mean.field.cap + se.field.cap), width = 0.25) +
  ylab("Field Capacity (g H2O)") +
  xlab("Soil to Sand Ratio")

ggsave("mean_field_cap.png", width = 4, height = 3)

ggplot(trials_sum, aes(x=soil_sand_ratio, y=mean.VWC)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.VWC - se.VWC, ymax = mean.VWC + se.VWC), width = 0.25) +
  ylab("Mean VWC (%)") +
  xlab("Soil to Sand Ratio")

ggsave("mean_VWC_field_cap.png", width = 4, height = 3)


ggplot(trials2, aes(x=soil_sand_ratio, y=field.cap)) +
  geom_boxplot() +
  theme_classic() +
  geom_jitter() +
  ylab("Field Capacity (g H2O)") +
  xlab("Soil to Sand Ratio")

ggsave("field_cap.png", width = 5, height = 4)

ggplot(trials2, aes(x=soil_sand_ratio, y=VWC___hours_post_saturation.2)) +
  geom_boxplot() +
  theme_classic() +
  geom_jitter() +
  ylab("VWC (%) at Field Capacity") +
  xlab("Soil to Sand Ratio")

ggsave("VWC_field_cap.png", height = 4, width = 5)


## Soil weights
soil_weights <- trials2 %>%
  group_by(soil_sand_ratio) %>%
  summarise(mean.soil.weight = mean(soil_as_is_weight_g), se.soil.weight = calcSE(soil_as_is_weight_g),
            max.soil.weight = max(soil_as_is_weight_g),
            median.soil.weight = median(soil_as_is_weight_g)) %>%
  mutate(soil_to_add = max.soil.weight/0.9) 


ggplot(trials2, aes(x=soil_sand_ratio, y=soil_as_is_weight_g)) +
  geom_boxplot() +
  geom_jitter()


