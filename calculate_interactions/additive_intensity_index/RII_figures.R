
# Set up ####
## read in data
source("data_cleaning/clean_model_dat.R")

## binter_for_RII_seed_analyses is the correct data frame to use in these analyses; it contains only BRHO focal data
## brho.model also contains BRHO bkgrd data reformatted to be intraspecific focal data, which is not appropriate in calculating the RII here

# BRHO ####
## Calc RII ####
## calculate mean control biomass for use in RII calcs
controls = binter_for_RII_seed_analyses %>%
  filter(ACAM == 0) %>% ## only want planted 0's
  group_by(water, microbe) %>%
  summarise(mean.control = mean(seeds.out.percap))

## calculate RII comparing 0 background to all other densities
brho_RII = left_join(binter_for_RII_seed_analyses, controls, by = c("water", "microbe")) %>%
  mutate(RII = (seeds.out.percap - mean.control) / (mean.control + seeds.out.percap),
         
         microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low"))) %>%
  filter(!unique.ID %in% rm.contaminated)

## Plot ####
## RII by planted density
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
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")
# ggsave("figures/MS_draft3/Fig2_meanRII_planted_dens_20250210.png", width = 7, height = 4)

brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.RII = mean(RII, na.rm = T),
            se.RII = calcSE(RII)) %>%
  
  ggplot(aes(x=ACAM, y=mean.RII, fill = microbe)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.RII - se.RII, ymax = mean.RII + se.RII)) +
  geom_line() +
  geom_point(aes(fill = microbe), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("black", "white")) +
#  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")




brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.seeds = mean(seeds.out.percap, na.rm = T),
            se.seeds = calcSE(seeds.out.percap)) %>%
  
  ggplot(aes(x=ACAM, y=mean.seeds, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Per-Capita Seeds Out") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")
# ggsave("figures/MS_draft2/seeds_out_planted_dens.png", width = 7, height = 4)

## Seed output by final density
ggplot(brho_RII, aes(x=num.bg.indiv, y=seeds.out.percap, color = water)) +
 # geom_point(aes(fill = water), colour = "black", pch = 21, size = 3) +
  geom_point(size = 2) +
  facet_wrap(~microbe) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_smooth(method = "lm", alpha = 0.25) +
  xlab("Legume Density") +
  ylab("Grass Per-capita Seeds Out") +
  labs(color = "Water")
## ggsave("figures/MS_draft2/seeds_out_v_final_dens.png", width = 8, height = 3.5)



## Supp Figs ####
## planted vs. final density
ggplot(brho_RII, aes(x=ACAM, y=num.bg.indiv)) +
  geom_point() +
  facet_grid(water~microbe) +
  xlab("Planted Legume Density") +
  ylab("Final Legume Density")
## ggsave("figures/MS_version1/FigS3_planted_v_final_dens.png", width = 8, height = 7)

## RII by final density
brho_RII %>%
  filter(num.bg.indiv != 0) %>%
  ggplot(aes(x=num.bg.indiv, y=RII, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 2.5) +
  #geom_smooth()+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~microbe) +
  ylab("Relative Interaction Intensity") +
  xlab("Final Legume Density") +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values = c("#70a494", "#f6edbd", "#de8a5a")) +
  labs(fill = "Water")
# ggsave("figures/MS_version1/FigS2_RII_num_bg.png", width = 8, height = 3.5)













## this would be good as a supplementary figure I think!
ggplot(brho_RII, aes(x=num.bg.indiv, y=seeds.percap, fill = microbe)) +
  geom_point(aes(fill = microbe), colour = "black", pch = 21, size = 3) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("#6699CC", "#661100")) +
  ylab("Seeds per Capita") +
  xlab("Final Legume Density") +
  labs(fill = NULL)
#ggsave("figures/MS_version1/FigS1_seedsout_num_bg.png", width = 9, height = 3.5)


## Seed output by planted density


# ACAM ####
controls = ainter %>%
  filter(num.bg.indiv == 0) %>% ## only want planted 0's
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  group_by(water) %>%
  summarise(mean.control = mean(seeds.percap))

## calculate RII comparing 0 background to all other densities
acam_RII = left_join(ainter, controls, by = c("water")) %>%
  
 # filter(num.bg.indiv > 9) %>% ## go back to be more careful with this filtering later
  
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  
  mutate(RII = (seeds.percap - mean.control) / (mean.control + seeds.percap),
         
         microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")), 
         BRHO = ifelse(BRHO == 48, 60, BRHO)) #%>%
  #filter(!unique.ID %in% rm.contaminated)


ggplot(acam_RII, aes(x=num.bg.indiv, y=RII, fill = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3) +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a")) +
  xlab("Final Grass Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water")

#ggsave("figures/MS_version1/FigS4_acamRII.png", width = 5, height = 3.5)

acam_RII %>%
  group_by(BRHO, water) %>%
  summarise(mean.RII = mean(RII), 
            se.RII = calcSE(RII)) %>%
  
ggplot(aes(x=BRHO, y=mean.RII, fill = water)) +
  
  geom_errorbar(aes(ymin = mean.RII - se.RII, ymax = mean.RII + se.RII), width = 2) +
  
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3) +
  geom_line() +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a")) +
  xlab("Final Grass Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water")


# Joint figures ####

brho_RII_j = brho_RII %>%
  select(unique.ID, block, water, microbe, ACAM, rep, RII) %>%
  mutate(planted.bg = ACAM, 
         species = "BRHO") %>%
  select(-ACAM)

acam_RII_j = acam_RII %>%
  select(unique.ID, block, water, microbe, BRHO, rep, RII) %>%
  mutate(planted.bg = BRHO,
         species = "ACAM") %>%
  select(-BRHO)

joint_RII = rbind(brho_RII_j, acam_RII_j)


joint_RII %>%
  group_by(planted.bg, water, microbe, species) %>%
  summarise(mean.RII = mean(RII), 
            se.RII = calcSE(RII)) %>%
  
  ggplot(aes(x=planted.bg, y=mean.RII, fill = microbe, shape = species)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_line() +
  
  geom_errorbar(aes(ymin = mean.RII - se.RII, ymax = mean.RII + se.RII), width = 2) +
  
  geom_point(aes(fill = microbe),size = 3) +
  
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21,22))+
  xlab("Final Grass Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Microbe", shape = "Species") +
  facet_wrap(~water)





