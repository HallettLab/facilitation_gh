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

# Figure 2 ####
alpha_mean = alpha_sig %>%
  group_by(focal, water, dens) %>%
  summarise(mean_alpha_inter = mean(alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")))

## panel a: brho ####
b_sig = alpha_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(
    water == 1, "High",
    ifelse(water == 0.75, "Intermediate",
           "Low"))) %>%
  ggplot(aes(
    x=dens, 
    y=alpha_inter, 
    color = as.factor(water))
    ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(
    alpha = 0.15, 
    aes(
      color = water.text, 
      group = interaction(water.text, post_num)
      )) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(
    data = alpha_mean[alpha_mean$focal == "BRHO",],
    aes(
      x=dens, 
      y=mean_alpha_inter, 
      group = water.text, 
      color = water.text),
    linewidth = 1.25, 
    color = "black"
    ) +
  geom_line(
    data = alpha_mean[alpha_mean$focal == "BRHO",], 
    aes(
      x=dens, y=mean_alpha_inter, 
      group = water.text, 
      color = water.text),
    linewidth = 0.75
    ) +
  ylab("α_ji") +
  xlab("Legume Density") +
  labs(color = "Water Level") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))

## panel b: acam ####
a_sig = alpha_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(
    aes(
      x=dens, 
      y=alpha_inter, 
      color = as.factor(water))
    ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(
    alpha = 0.15,
    aes(
      color = water.text, 
      group = interaction(water.text, post_num))
    ) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(
    data = alpha_mean[alpha_mean$focal == "ACAM",], 
    aes(
      x=dens,
      y=mean_alpha_inter, 
      group = water.text, 
      color = water.text),
    linewidth = 1.25, color = "black"
    ) +
  geom_line(
    data = alpha_mean[alpha_mean$focal == "ACAM",], 
    aes(
      x=dens,
      y=mean_alpha_inter,
      group = water.text,
      color = water.text),
    linewidth = 0.75
    ) +
  ylab("α_ij") +
  xlab("Grass Density") +
  labs(color = "Water Level") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed")

## join ####
ggarrange(b_sig, a_sig, common.legend = T, legend = "bottom", labels = "auto", align = "hv", ncol = 1)

ggsave(
  "../outputs/figures/Autumn_2026/Fig3_dens_env_interactions_sigonly.png",
  width = 4.5, 
  height = 6, 
  bg = "white"
  )
#2.5/3.5