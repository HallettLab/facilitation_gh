## Header ##
## 
## Clean Focal species data ##
## 
## Author: Carmen Watkins

# Set Up ####
library(tidyverse)

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## Read in data
file_loc = "/Users/carme/University of Oregon Dropbox/Carmen Watkins/Facilitation_GH/data/biomass/Adult/"

brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))

abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

# Clean Data ####
## brho focal ####
names(brho)

hist(brho$total.biomass.g)
hist(brho$inflor.g)
unique(brho$processing.notes)

unique(brho$water)
unique(brho$microbe)
unique(brho$bkgrd)
unique(brho$ACAM)

## calculate per capita biomass
names(abkgrd)
abkgrd.join = abkgrd %>%
  select(unique.ID, water, microbe, rep, num.bg.indiv, num.dead.bg.indiv, num.resprouted.BRHO.focals)

brho_clean = brho %>%
  mutate(tot.bio.percap = total.biomass.g/num.focal.indiv, 
         unique.ID = X0)  %>%
  select(-X0) %>%
  left_join(abkgrd.join, by = c("unique.ID", "water", "microbe", "rep"))
  
## calculate mean control biomass for use in RII calcs
controls = brho_clean %>%
  filter(ACAM == 0) %>%
  group_by(water, microbe) %>%
  summarise(mean.control = mean(tot.bio.percap))

## calculate RII comparing 0 background to all other densities
brho_RII = left_join(brho_clean, controls, by = c("water", "microbe")) %>%
  mutate(RII = (tot.bio.percap - mean.control) / (mean.control + tot.bio.percap),
         microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")))


## acam focal ####
## calculate per capita biomass
acam_clean = acam %>%
  mutate(tot.bio.percap = total.biomass.g/num.focal.indiv) 

## calculate mean control biomass for use in RII calcs
acontrols = acam_clean %>%
  filter(BRHO == 0) %>%
  group_by(water, microbe) %>%
  summarise(mean.control = mean(tot.bio.percap))

## calculate RII comparing 0 background to all other densities
acam_RII = left_join(acam_clean, acontrols, by = c("water", "microbe")) %>%
  mutate(RII = (tot.bio.percap - mean.control) / (mean.control + tot.bio.percap),
         microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")))




# Explore ####
ggplot(brho_RII, aes(x=as.factor(ACAM), y=RII)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_grid(water ~ microbe) +
  ylab("Relative Interaction Intensity") +
  xlab("Planted Legume Density") +
  theme(text = element_text(size = 15))

#ggsave("figures/Nov2024_committee_mtg/brho_RII_waterXmicrobe.png", width = 10, height = 7)

brho_RII %>%
  filter(ACAM != 0) %>%
ggplot(aes(x=num.bg.indiv, y=RII, color = microbe)) +
  geom_point() +
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~water) +
  ylab("Relative Interaction Intensity") +
  xlab("Actual Legume Density") +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("#6699CC", "#661100"))

#ggsave("figures/Nov2024_committee_mtg/brho_RII_waterXmicrobe_actual_dens.png", width = 9, height = 3.5)

#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#6699CC,#888888

ggplot(acam_RII, aes(x=as.factor(BRHO), y=RII)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_grid(water ~ microbe) +
  ylab("Relative Interaction Intensity") +
  xlab("Planted Grass Density") +
  theme(text = element_text(size = 15))

ggsave("figures/Nov2024_committee_mtg/acam_RII_waterXmicrobe.png", width = 4, height = 4)



ggplot(brho_clean, aes(x=ACAM, y=tot.bio.percap)) +
  geom_point() +
  geom_smooth()

ggplot(brho_clean, aes(x=as.factor(ACAM), y=tot.bio.percap)) +
  geom_boxplot() +
  facet_grid(microbe~water)

ggplot(brho_clean, aes(x=ACAM, y=tot.bio.percap)) +
  geom_point() + 
  geom_smooth() +
  facet_grid(microbe~water)


brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.RII = mean(RII, na.rm = T),
            se.RII = calcSE(RII)) %>%
  
  ggplot(aes(x=ACAM, y=mean.RII, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.RII - se.RII, ymax = mean.RII + se.RII)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a")) +
  xlab("Planted Legume Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

ggsave("figures/MS_version1/meanRII_planted_dens.png", width = 8, height = 4.5)
  
  
#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c
















