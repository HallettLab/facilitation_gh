## Header ##
## 
## Script Name: Fig 2: Density Dependent Interactions
##
## Purpose: Plot the density dependent sigmoidal interaction coefficient and 
## the additive intensity index by density. Also plot leaf nutrients and isotopes
## to get at mechanism.
##
## This script creates data figure 2 of the paper 
## 
## Author: Carmen Watkins

# Set Up ####
## load packages
library(tidyverse)
library(ggpubr)
library(cowplot)

## read in sig alphas
source("3_data_analysis/create_sigmoidal_alphas_df.R")

## read in additive intensity index
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")

## read in leaf nutrient data
source("3_data_analysis/analyse_leaf_nutrients.R")

# Figure 2 ####
## panel a: sigmoidal interaction coefficient
## brho ####
b_sig = alpha_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y=alpha_inter, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  #ylab("Raw Interspecific Interaction") +
  ylab("α_ji") +
  xlab("Legume Density") +
  labs(color = "Water Level") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed")

## panel b: additive intensity index
b_AII = brho_AII %>%
  filter(microbe == "Live") %>%
  group_by(ACAM, water) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  xlab("Planted Legume Density") +
  ylab("AII") +
  labs(fill = "Water") +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "bottom")

## acam ####
a_sig = alpha_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y=alpha_inter, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  #ylab("Raw Interspecific Interaction") +
  ylab("α_ij") +
  xlab("Legume Density") +
  labs(color = "Water Level") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed")

## panel b: additive intensity index
a_AII = acam_AII %>%
  filter(microbe == "Live") %>%
  group_by(BRHO, water) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=BRHO, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA), width = 2) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  xlab("Planted Grass Density") +
  ylab("AII") +
  labs(fill = "Water") +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "bottom")

## join ####
ggarrange(b_sig, b_AII, a_sig, a_AII, common.legend = T, legend = "bottom", labels = "auto", align = "hv")

ggsave("../outputs/figures/Autumn_2026/Fig2_dens_env_interactions.png", width = 10, height = 6, bg = "white")
#2.5/3.5

# Figure 5 ####
## panels c/d: leaf nutrient 'explanatory variables'
bC = ggplot(CN_AII, aes(x=delta13C, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("AII") +
  xlab("Delta 13C") +
  labs(color = "Water") +
  theme(text = element_text(size=12))

bN = ggplot(CN_AII, aes(x=WtN, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("AII") +
  xlab("Leaf % N") +
  labs(color = "Water") +
  theme(text = element_text(size=12))

