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
  
  labs(fill = "Soil", color = "Soil", shape = "Growth Rate", linetype = "Growth Rate") +
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
  
  labs(fill = " ", color = " ", shape = "Growth Rate", linetype = "Growth Rate") +
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


# OLD FIG 2 #### 
## acam lambda ####
## calculate mean acam lambda value
mAL = acam %>% 
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", 
               values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_lam = mean(lam)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
aL = acam %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=lam, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAL, aes(x=water.text, y=mean_lam)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab("Intrinsic Growth Rate") +
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil", linetype = "Soil") +
  #guides(color = guide_legend("Soil", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  ggtitle("A. americanus")  +
  theme(plot.title = element_text(face = "italic"))

## brho lambda ####
## calculate mean brho lambda value
mBL = brho %>% 
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", 
               values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_lam = mean(lam)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
bL = brho %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", 
               values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", 
                                  "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=lam, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBL, aes(x=water.text, y=mean_lam)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab(" ") +
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil", linetype = "Soil") +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  ggtitle("B. hordeaceus")  +
  theme(plot.title = element_text(face = "italic"))


## acam intra ####
## calculate mean acam INTRAspecific interaction value
mAA = acam %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
aa = acam %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAA, aes(x=water.text, y=mean_alpha, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("INTRAspecific alpha") +
  xlab("Water Level")  +
  labs(fill = "Soil", color = "Soil") +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-0.085, 0.021))

## brho inter ####
## calculate mean brho INTERspecific interaction value
mBA = brho %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
ba = brho %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", 
               values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBA, aes(x=water.text, y=mean_alpha, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab(" ") +
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0.021))
  
  ## code to add max value of sigmoidal in
 # coord_cartesian(ylim = c(-0.085, 0.21)) +
  #geom_point(data = max_int, aes(x=water.text, y=max_a), alpha =  0.15) +
  #geom_line(data = m_MI, aes(x=water.text, y=mma)) +
  #stat_summary(data = max_int, aes(x=water.text, y=max_a),
   # fun = "mean",        
#    geom = "point",
 #   col = "black",
  #  size = 4,
  # shape = 21, fill = "white") 
  
## acam inter ####
## calculate mean acam INTERspecific interaction value
mAB = acam %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
ab = acam %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
   
  ggplot(aes(x = water.text, y=alpha_brho )) +
  geom_point(alpha = 0.15, color = "#565656") +
  geom_line(data = mAB, aes(x=water.text, y=mean_alpha, group = 1)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, fill = "white") +
  ylab("INTERspecific alpha") +
  xlab(" ")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
 # guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0.021)) 
  #coord_cartesian(ylim = c(-0.085, 0.02))

## brho intra ####
## calculate mean brho INTRAspecific interaction value
mBB = brho %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

## plot
bb = brho %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha_brho )) +
  geom_point(alpha = 0.15, color = "#565656") +
  geom_line(data = mBB, aes(x=water.text, y=mean_alpha, group = 1)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, fill = "white") +
  ylab(" ") +
  xlab("Water Level")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
 # guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-0.085, 0.021))

## put together ####
ggarrange(aL, bL, ab, ba, aa,bb, ncol = 2, nrow = 3, common.legend = TRUE, 
          legend = "bottom", align = "v", labels = "AUTO")

ggsave("figures/final_diss/Fig2_mutualism_params_with_sigmax.png", width = 7, height = 9)


# Fig 2 Talk ####
## lambda ####
acam %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=lam, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAL, aes(x=water.text, y=mean_lam)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab("Intrinsic Growth Rate") +
  xlab("Water Level")  +
  labs(fill = "Soil", color = "Soil", linetype = "Soil") +
  #guides(color = guide_legend("Soil", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=16)) +
#  ggtitle("A. americanus")  +
  theme(plot.title = element_text(face = "italic"))

ggsave("figures/dissertation_talk/acam_lambda.png", width = 6, height = 4)

## alpha ####
mBA = brho %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

brho %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = water.text, y=alpha, group = soil, color = soil)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBA, aes(x=water.text, y=mean_alpha, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab("Interaction Coefficient") +
  xlab("Water Level")  +
  labs(fill = "Soil", color = "Soil") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=16))

ggsave("figures/dissertation_talk/brho_inter_alpha.png", width = 6, height = 4)



# Fig 3 ####
## acam igr ####
mAGR = gr_df %>%
  filter(focal == "ACAM") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

aigr = gr_df %>%
  filter(focal == "ACAM") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAGR, aes(x=water.text, y=mean_gr)) +
  #scale_color_manual(values = c("black", "white")) +
 # scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  labs(fill = "Soil", color = "Soil", shape = "Type", linetype = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Type", override.aes = list(size = 4))) +
  theme(text = element_text(size=14)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  ggtitle("A. americanus") +
  theme(plot.title = element_text(face = "italic"))

## brho igr ####
mBGR = gr_df %>%
  filter(focal == "BRHO") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

bigr = gr_df %>%
  filter(focal == "BRHO") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBGR, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  
  labs(fill = " ", color = " ", shape = "Type", linetype = "Type") +
  ylab(" ") +
  xlab("Water Level") +
  guides(fill = guide_legend(" ", override.aes = list(shape = 21))) +
  theme(text = element_text(size=14)) +
  guides(shape = "none") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  ggtitle("B. hordeaceus") +
  theme(plot.title = element_text(face = "italic"))

## put together ####
ggarrange(aigr, bigr, ncol = 2, nrow = 1, legend = "bottom", align = "h", 
          labels = "AUTO", common.legend = T)

ggsave("figures/final_diss/diss_done/Fig3_mutualism_igrs.png", width = 8, height = 4)

# Fig 3 Talk ####
## acam igr ####
mAGR = gr_df %>%
  filter(focal == "ACAM") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

gr_df %>%
  filter(focal == "ACAM") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAGR, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  labs(fill = "Soil", color = "Soil", shape = "Type", linetype = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Type", override.aes = list(size = 4))) +
  theme(text = element_text(size=16)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_cartesian(ylim = c(-2.5, 4.25))

ggsave("figures/dissertation_talk/acam_igr.png", width = 6, height = 4)

### no invasion ####
mAGR2 = gr_df %>%
  filter(focal == "ACAM", type == "Intrinsic") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate)) 

gr_df %>%
  filter(focal == "ACAM", 
         type == "Intrinsic") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAGR2, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(fill = "Soil", color = "Soil", shape = "Type", linetype = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Type", override.aes = list(size = 4))) +
  theme(text = element_text(size=16)) +
  scale_linetype_manual(values = c("dashed", "solid"))  +
  coord_cartesian(ylim = c(-2.5, 4.25))
ggsave("figures/dissertation_talk/acam_igr_intrin_only.png", width = 6, height = 4)


## brho igr ####
mBGR = gr_df %>%
  filter(focal == "BRHO") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

gr_df %>%
  filter(focal == "BRHO") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBGR, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  
  labs(fill = " ", color = " ", shape = "Type", linetype = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend(" ", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Type", override.aes = list(size = 4))) +
  
  theme(text = element_text(size=16)) +
  #guides(shape = "none") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_cartesian(ylim = c(3.75, 7))

ggsave("figures/dissertation_talk/brho_igr.png", width = 6, height = 4)

### no invasion ####
mBGR2 = gr_df %>%
  filter(focal == "BRHO", type == "Intrinsic") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate)) 

gr_df %>%
  filter(focal == "BRHO", 
         type == "Intrinsic") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), shape = type, linetype = type, color = soil)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBGR2, aes(x=water.text, y=mean_gr)) +
  scale_color_manual(values = c("#020202", "#969696")) +
  scale_fill_manual(values = c("#020202", "#dbdbdb")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  #geom_hline(yintercept = 0, linetype = "dotted") +
  labs(fill = "Soil", color = "Soil", shape = "Type", linetype = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  guides(shape = guide_legend("Type", override.aes = list(size = 4))) +
  theme(text = element_text(size=16)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  coord_cartesian(ylim = c(3.75, 7))

ggsave("figures/dissertation_talk/brho_igr_intrin_only.png", width = 6, height = 4)
