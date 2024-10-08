
## read in data
source("field_capacity.R")

## soil dry weights
water_lev <- read.csv(paste0(lead, "water_level_pots.csv"))

## get one average value
sum_FC <- trials2 %>%
  summarise(mean.water.to.soil.prop.FC = mean(water.to.soil.prop.FC))


water_lev2 <- water_lev %>%
  mutate(dry.weight.mix = STERILE_soil_amount_g*dry2[dry2$Material == "soil",]$mean.prop.weight + sand_amount_g*dry2[dry2$Material == "sand",]$mean.prop.weight,
         
         air.dry.weight.mix = STERILE_soil_amount_g + sand_amount_g,
         Wfc = sum_FC$mean.water.to.soil.prop.FC * dry.weight.mix,
         Water_amt_treatment = Wfc*water_level,
         Target_weight = empty_pot_weight_g + Water_amt_treatment + dry.weight.mix)


write.csv(water_lev2, paste0(lead, "watering_sheet.csv"))
