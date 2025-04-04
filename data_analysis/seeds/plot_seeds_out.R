## Plot seed output

## read in data
source("data_cleaning/clean_model_dat.R")

library(ggpubr)

# Figure 2 ####
## BRHO, HW ####
b_ij_h = binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  filter(water.text == "High") %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, shape = as.factor(microbe), color = as.factor(water.text), linetype = as.factor(microbe))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
 # geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#70a494")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("Grass Seed Output") +
  labs(linetype = "Microbe", shape = "Microbe", color = "Water") +
  scale_shape_manual(values = c(1, 16)) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 1500)) +
  theme(legend.position="none")


b_ij_i = binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  filter(water.text == "Intermediate") %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, shape = as.factor(microbe), color = as.factor(water.text), linetype = as.factor(microbe))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  #geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#f3d0ae")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("") +
  labs(linetype = "Microbe", shape = "Microbe", color = "Water") +
  scale_shape_manual(values = c(1, 16)) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 1500)) +
  theme(legend.position="none")

b_ij_l = binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  filter(water.text == "Low") %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, shape = as.factor(microbe), color = as.factor(water.text), linetype = as.factor(microbe))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
 # geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("") +
  labs(linetype = "Microbe", shape = "Microbe", color = "Water") +
  scale_shape_manual(values = c(1, 16)) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 1500)) +
  theme(legend.position="none")

b_ii = bintra %>% 
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=num.focal.indiv, y=seeds.out.percap, color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +

  #geom_smooth(method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  
  geom_jitter(size = 1.75) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Grass Density") +
  ylab("Grass Seed Output") +
  labs(linetype = "Microbe", shape = "Microbe", color = "Water") +
  scale_shape_manual(values = c(1, 16)) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 1500))

a_ji = ainter %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  
 
   geom_point(size = 1.75) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Grass Density") +
  ylab("") +
  labs(color = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 315), xlim = c(0, 70))
#  coord_cartesian(ylim = c(0,110), )

a_jj = aintra %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=num.focal.indiv, y=seeds.out.percap, color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_point(size = 1) +
  geom_smooth(method= "gam", alpha = 0.15, linewidth = 1) +
  
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  #scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("Legume Seed Output") +
  labs(color = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,315), xlim = c(0, 70)) 

ggarrange(b_ij_h, b_ij_i, b_ij_l, b_ii, a_jj, a_ji, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("data_analysis/seeds/figures/seeds_by_dens.png", width = 10, height = 7.5)



## Make legend ####
binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         m.text = ifelse(microbe == "1", "Live", "Sterilized")) %>%
  #filter(water.text == "High") %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, shape = as.factor(m.text), linetype = as.factor(m.text),color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_smooth(method = "lm", alpha = 0.15, linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("Grass Per Capita Seed Output") +
  labs(shape = "Soil", color = "Water", linetype = "Soil") +
  scale_shape_manual(values = c(16, 1)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 1500))

ggsave("data_analysis/seeds/figures/legend_for_seeds_fig.png", width = 9, height = 5)

# ACAM ####


## ggsave("figures/MS_draft3/acam_seeds_by_dens.png", width = 4, height = 4)
