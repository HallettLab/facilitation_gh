## Header ##
## 
## Script Name: Plot IGR mboth
##
## Purpose: Plot the invasion growth rates and intrinsic growth rates of both 
## species, comparing the live vs. sterilised soil and different water
## treatments. Also plot posterior distributions of all intra and interspecific
## interactions.
##
## This script creates data figure 1 of the paper 
## 
## Author: Carmen Watkins

# Set up ####
library(tidyverse)
library(ggpubr)
library(cowplot)

theme_set(theme_classic())

## posteriors
acam = read.csv("../outputs/posteriors/acam_stat_posts_B_20260108.csv")
brho = read.csv("../outputs/posteriors/brho_stat_posts_B_20260107.csv")

## invasion growth rates
igr_microbe = read.csv("../outputs/analysis_outputs/igr_mboth_20260109.csv") 

## prep igr df
gr_df = igr_microbe %>%
  ## put lambda on log scale to match that of IGR
  mutate(log_lambda = log(lambda)) %>%
  select(focal, water, microbe, igr, log_lambda) %>%
  pivot_longer(cols = c("igr", "log_lambda"), names_to = "type", 
               values_to = "growth_rate") %>%
  mutate(type = ifelse(type == "igr", "Invasion", "Intrinsic"),
         soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"))

# Data Fig 1 ####
## Growth rates ####
### acam ####
mAGR = gr_df %>%
  filter(focal == "ACAM") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate)) %>%
  mutate(type2 = ifelse(type == "Intrinsic", "λ", "r"))

aigr = gr_df %>%
  filter(focal == "ACAM") %>%
  mutate(type2 = ifelse(type == "Intrinsic", "λ", "r")) %>%
  mutate(int = interaction(soil, type2)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type2), 
             shape = type2, linetype = type2, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAGR, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type2)) +
  scale_shape_manual(values = c(21, 22)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(fill = "Soil", color = "Soil", shape = "Growth Rate", 
       linetype = "Growth Rate") +
  ylab("ln(Nt+1/Nt)") +
  xlab(NULL) +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Growth Rate", override.aes = list(size = 4))) +
  theme(text = element_text(size=13.5)) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  ggtitle("A. americanus") +
  theme(plot.title = element_text(face = "italic"))

### brho ####
mBGR = gr_df %>%
  filter(focal == "BRHO") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate)) %>%
  mutate(type2 = ifelse(type == "Intrinsic", "λ", "r"))

bigr = gr_df %>%
  filter(focal == "BRHO") %>%
  mutate(type2 = ifelse(type == "Intrinsic", "λ", "r")) %>%
  mutate(int = interaction(soil, type2)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type2), 
             shape = type2, linetype = type2, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBGR, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type2)) +
  scale_shape_manual(values = c(21, 22)) +
  labs(fill = " ", color = " ", shape = "Growth Rate", 
       linetype = "Growth Rate") +
  ylab(" ") +
  xlab(NULL) +
  guides(fill = guide_legend(" ", override.aes = list(shape = 21))) +
  theme(text = element_text(size=13.5)) +
  guides(shape = "none") +
  scale_linetype_manual(values = c("dotted", "solid")) +
  ggtitle("B. hordeaceus") +
  theme(plot.title = element_text(face = "italic"))

### put together ####
grs = ggarrange(aigr, bigr, ncol = 2, nrow = 1, legend = "right", align = "h", 
          labels = "AUTO", common.legend = T)


## Alphas ####
### acam intra ####
## calculate mean acam INTRAspecific interaction value
mAA = acam %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")))

## plot
aa = acam %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")),
         water.text = fct_relevel(water.text, "Low", "Int", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAA, aes(x=water.text, y=mean_alpha, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 2.5,
    shape = 21, aes(fill = soil)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("α_ii") +
  xlab(NULL)  +
  labs(fill = "Soil", color = "Soil") +
  theme(text = element_text(size=13.5)) +
  coord_cartesian(ylim = c(-0.085, 0.021))


### brho inter ####
## calculate mean brho INTERspecific interaction value
mBA = brho %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")))

## plot
ba = brho %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")),
         water.text = fct_relevel(water.text, "Low", "Int", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBA, aes(x=water.text, y=mean_alpha, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 2.5,
    shape = 21, aes(fill = soil)) +
  ylab("α_ji") +
  xlab(NULL)  +
  labs(fill = "Soil", color = "Soil") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=13.5)) +
      #  axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0.021))

### acam inter ####
## calculate mean acam INTERspecific interaction value
mAB = acam %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")))

## plot
ab = acam %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")),
         water.text = fct_relevel(water.text, "Low", "Int", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha_brho )) +
  geom_point(alpha = 0.15, color = "#565656") +
  geom_line(data = mAB, aes(x=water.text, y=mean_alpha, group = 1)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 2.5,
    shape = 21, fill = "white") +
  ylab("α_ij") +
  xlab(NULL)  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=13.5)) +
  #      axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0.021)) 

### brho intra ####
## calculate mean brho INTRAspecific interaction value
mBB = brho %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")))

## plot
bb = brho %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Int", "Low")),
         water.text = fct_relevel(water.text, "Low", "Int", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha_brho )) +
  geom_point(alpha = 0.15, color = "#565656") +
  geom_line(data = mBB, aes(x=water.text, y=mean_alpha, group = 1)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 2.5,
    shape = 21, fill = "white") +
  ylab("α_jj") +
  xlab(NULL)  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=13.5)) +
  coord_cartesian(ylim = c(-0.085, 0.021))

### put together ####
alph = ggarrange(aa, ab, bb, ba, ncol = 4, nrow = 1, common.legend = TRUE, 
          legend = "none", align = "h", labels = c("C", "D", "E", "F"))


plot_grid(grs, alph, nrow = 2, align = "v", rel_heights = c(1, 0.5))

