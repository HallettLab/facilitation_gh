
## SET UP ####
## read in data 
source("data_cleaning/clean_model_dat.R")



## 2 * (deltaP / P^-N + abs(deltaP))


## P^-N = performance of the target species w/o neighbors

## deltaP = P+N - P-N = total impact of neighbors


## Calc RII ####
## calculate mean control biomass for use in RII calcs
controls = binter_for_RII_seed_analyses %>%
  filter(ACAM == 0) %>% ## only want planted 0's
  group_by(water, microbe) %>%
  summarise(mean.control = mean(seeds.out.percap))

## calculate RII comparing 0 background to all other densities
brho_RII = left_join(binter_for_RII_seed_analyses, controls, by = c("water", "microbe")) %>%
  mutate(RII = (seeds.out.percap - mean.control) / (mean.control + seeds.out.percap),
         
         microbe = ifelse(microbe == 0, "Sterilized", "Live"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")),
         
         NIntA = 2 * ((seeds.out.percap - mean.control)/ (mean.control + abs((seeds.out.percap - mean.control)))) ) %>%
  filter(!unique.ID %in% rm.contaminated)

# ACAM ####
controlsA = ainter %>%
  filter(num.bg.indiv == 0) %>% ## only want planted 0's
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  group_by(water) %>%
  summarise(mean.control = mean(seeds.percap))

## calculate RII comparing 0 background to all other densities
acam_RII = left_join(ainter, controlsA, by = c("water")) %>%
  # filter(num.bg.indiv > 9) %>% ## go back to be more careful with this filtering later
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  
  mutate(RII = (seeds.percap - mean.control) / (mean.control + seeds.percap),
         
         microbe = ifelse(microbe == 0, "Sterilized", "Live"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low")),
         NIntA = 2 * ((seeds.out.percap - mean.control)/ (mean.control + abs((seeds.out.percap - mean.control)))),
         BRHO = ifelse(BRHO == 48, 60, BRHO)) #%>%
#filter(!unique.ID %in% rm.contaminated)








ggplot(brho_RII, aes(x=num.bg.indiv, y=NIntA)) +
  geom_point()

ggplot(brho_RII, aes(x=RII, y=NIntA)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")


brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

brho_RII %>%
#  group_by(ACAM, water, microbe) %>%
#  summarise(mean.NIntA = mean(NIntA, na.rm = T),
#            se.NIntA = calcSE(NIntA)) %>%
  
  filter(ACAM !=0) %>% 
  
  ggplot(aes(x=ACAM, y=NIntA, color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
#  geom_line() +
 # geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_jitter(size = 2.5)

brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

# Fig 1 ####

brj = brho_RII %>%
  select(unique.ID, water, microbe, rep, ACAM, num.bg.indiv, RII, NIntA) %>%
  mutate(planted.bg = ACAM,
         focal = "BRHO") %>%
  select(-ACAM)

acj = acam_RII %>% 
  select(unique.ID, water, microbe, rep, BRHO, num.bg.indiv, RII, NIntA) %>%
  mutate(planted.bg = BRHO,
         focal = "ACAM") %>%
  select(-BRHO)

RII_sp = rbind(brj, acj)

## plot both together
RII_sp %>%
  
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  
  ggplot(aes(x=planted.bg, y=NIntA, color = focal, shape = microbe)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~water) +
  scale_color_manual(values = c("#a3ad62", "#f0c6c3", "#de8a5a")) +
  xlab("Planted Neighbor Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom") +
  #geom_smooth(method = "lm", alpha = 0.25) +
  geom_jitter(size = 2) +
  scale_shape_manual(values = c(16, 1)) +
  labs(color = "Focal Species", shape = "Soil")

ggsave("data_analysis/RII/figures/NIntA_index.png", width = 7, height = 3.5)


#798234,#a3ad62,#d0d3a2,#fdfbe4,#f0c6c3,#df91a3,#d46780


