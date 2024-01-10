## set up environment
library(tidyverse)
library(lubridate)

theme_set(theme_classic())

## read in data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/Trials/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/Trials/"
  
} else {
  # 
  lead <- ""
} 

germ <- read.csv(paste0(lead, "seeding.csv")) 

germ$soil_sand_ratio <- as.factor(germ$soil_sand_ratio)

germ <- germ %>%
  select(1:7, 11:37) %>%
  filter(!is.na(water)) %>%
  mutate(germ.prop.BRHO = BRHO_12_13/BRHO.seeds,
         germ.prop.ACAM = ACAM_12_13/ACAM.seeds) %>%
  mutate(soil_sand_ratio = fct_relevel(soil_sand_ratio, "100_0", "75_25", "60_40", "50_50"))

ggplot(germ, aes(x=as.factor(water), y=germ.prop.BRHO, color = soil_sand_ratio)) +
  geom_boxplot() +
  xlab("Water Saturation") +
  ylab("BRHO germination (2 wks)") +
  geom_hline(yintercept = 1, linetype = "dashed")

#ggsave("pre-experiment_trials/BRHO_germ_v_water_soil.png", width = 6, height = 3)

ggplot(germ[germ$ACAM.seeds != 0,], aes(x=as.factor(water), y=germ.prop.ACAM, color = soil_sand_ratio)) +
  geom_boxplot() +
  xlab("Water Saturation") +
  ylab("ACAM germination (2 wks)")

#ggsave("pre-experiment_trials/ACAM_germ_v_water_soil.png", width = 6, height = 3)

germ.sum <- germ %>%
  group_by(soil_sand_ratio, water) %>%
  summarise(meang = mean(germ.prop.ACAM, na.rm = TRUE))


colnames(germ)

ACAM <- germ %>%
  select(1:5, 7, 9, 12, 15, 18, 21, 24, 27, 30, 33) %>%
  pivot_longer(cols = c(7:15), names_to = "date", values_to = "stem_count") %>%
  mutate(month = substr(date, 6, 7),
         day = substr(date, 9, 10), 
         year = 2023,
         date2 = ymd(paste0(year, month, day)))

ggplot(ACAM, aes(x=date2, y=stem_count, color = as.factor(water))) +
  geom_point() +
  #geom_line() +
  facet_wrap(~soil_sand_ratio)


