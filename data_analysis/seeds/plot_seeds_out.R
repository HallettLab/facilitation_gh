## Plot seed output

## read in data
source("data_cleaning/clean_model_dat.R")

# BRHO ####
## mean/SE by planted density
binter_for_RII_seed_analyses %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.spc = mean(seeds.out.percap, na.rm = T),
            se.spc = calcSE(seeds.out.percap)) %>%
  
  ggplot(aes(x=ACAM, y=mean.spc, fill = as.factor(water))) +
  geom_errorbar(aes(ymin = mean.spc - se.spc, ymax = mean.spc + se.spc), width = 1) +
  geom_line() +
  geom_point(aes(fill = as.factor(water)), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Planted Legume Density") +
  ylab("Seed Production (per cap.)") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")
# ggsave("figures/MS_draft2/Fig2pt2_seedsout_planted_dens.png", width = 5, height = 4)

## raw data seeds per cap by num bg indiv
binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, shape = as.factor(microbe), color = as.factor(water.text), linetype = as.factor(microbe))) +
  facet_wrap(~water.text) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", alpha = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Legume Density") +
  ylab("Per Capita Seed Production") +
  labs(linetype = "Microbe", shape = "Microbe", color = "Water") +
  scale_shape_manual(values = c(1, 16)) +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom")

#ggsave("figures/MS_draft3/seeds_by_dens.png", width = 7, height = 3.5)

# ACAM ####
ainter %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, color = as.factor(water.text))) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", alpha = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_linetype_manual(values = c(4, 1)) +
  xlab("Grass Density") +
  ylab("Per Capita Seed Production") +
  labs(color = "Water") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom")

ggsave("figures/MS_draft3/acam_seeds_by_dens.png", width = 4, height = 4)
