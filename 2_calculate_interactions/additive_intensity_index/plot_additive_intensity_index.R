## Header ##
## 
## Plot Additive Intensity Index
##
## Purpose: Plot the additive intensity index in different conditions
##
## Author: Carmen Watkins

# Set Up ####
## read in data 
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")


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

## Join ####
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

# Plot ####
brho_RII %>%
  filter(ACAM != 0) %>%
  ggplot(aes(x=num.bg.indiv, y=NIntA, color = water, linetype = microbe, shape = microbe)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #  geom_line() +
  geom_point() +
  #geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~water) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_smooth(method = "lm", alpha = 0.15) +
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(19,1))


# Fig 2 ####
RII_sp %>%
  filter(focal == "BRHO") %>%
  group_by(planted.bg, water, microbe, focal) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  ggplot(aes(x=planted.bg, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(size = 3.5, pch = 21) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water", shape = "Species") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom") #+
#scale_shape_manual(values = c(22, 21))

#ggsave("figures/Apr2025/Fig2_NIntA_index.png", width = 7, height = 4)

## Talk Version ####
RII_sp %>%
  filter(focal == "BRHO", microbe == "Live") %>%
  group_by(planted.bg, water, microbe, focal) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  ggplot(aes(x=planted.bg, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(size = 3.5, pch = 21) +
  #facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  xlab("Planted Legume Density") +
  ylab("Interaction") +
  labs(fill = "Water", shape = "Species") +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "bottom") 

ggsave("figures/dissertation_talk/AII_v_density.png", width = 5, height = 4)


# Fig 5 ####
## plot both together
RII_sp %>%
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  #filter(planted.bg != 0) %>%
  filter(focal == "BRHO") %>%
  
  ggplot(aes(x=planted.bg, y=NIntA, color = microbe)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(~water) +
  scale_color_manual(values = c("#595959", "#bababa")) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), alpha = 0.15) +
  xlab("Planted Neighbor Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom") +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c(16, 1)) +
  labs(color = "Soil Treatment", shape = "Soil")
