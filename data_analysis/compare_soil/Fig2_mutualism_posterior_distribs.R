# Set up ####
library(tidyverse)
library(ggpubr)
library(cowplot)

theme_set(theme_classic())

## posteriors
acam = read.csv("data/model_posteriors/acam_soil_comp_posts_final_20250424.csv")
brho = read.csv("data/model_posteriors/brho_soil_comp_posts_final_20250424.csv")

## invasion growth rates
igr_microbe = read.csv("data_analysis/compare_soil/igr_20250424_final_models.csv") 

## sigmoidal model igr
igr_sig = read.csv("data_analysis/MCT/output/igr_sigmoidal_20250428.csv")

max_int = igr_sig %>%
  filter(dens == 1) %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(water, focal, post_num) %>%
  summarise(max_a = max(alpha_inter)) %>%
  mutate(water.text = as.factor(ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  filter(focal == "BRHO") %>%
  mutate(soil = "Live")

m_MI = max_int %>%
  ungroup() %>%
  select(focal, water.text, max_a, soil) %>%
  group_by(focal, water.text, soil) %>%
  summarise(mma = mean(max_a)) %>%
  filter(focal == "BRHO")

## prep igr df
gr_df = igr_microbe %>%
  mutate(log_lambda = log(lambda)) %>%
  select(focal, water, microbe, igr, log_lambda) %>%
  pivot_longer(cols = c("igr", "log_lambda"), names_to = "type", values_to = "growth_rate") %>%
  mutate(type = ifelse(type == "igr", "Invasion", "Intrinsic"),
         soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"))

# Fig 2 Update ####
## acam lambda ####
mAL = acam %>% 
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_lam = mean(lam)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

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
mBL = brho %>% 
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_lam = mean(lam)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

bL = brho %>%
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
mAA = acam %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

aa = acam %>%
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
  coord_cartesian(ylim = c(-0.085, 0))

## brho inter ####
mBA = brho %>% 
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  group_by(water, soil) %>%
  summarise(mean_alpha = mean(alpha)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))

ba = brho %>%
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
  ylab(" ") +
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
 # coord_cartesian(ylim = c(-0.085, 0.02))
  
  ## code to add max value of sigmoidal in
  coord_cartesian(ylim = c(-0.085, 0.21)) +
  geom_point(data = max_int, aes(x=water.text, y=max_a), alpha =  0.15) +
  geom_line(data = m_MI, aes(x=water.text, y=mma)) +
  stat_summary(data = max_int, aes(x=water.text, y=max_a),
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
   shape = 21, fill = "white") 
  
  


## acam inter ####
mAB = acam %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))


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
  coord_cartesian(ylim = c(-0.085, 0.21)) 
  #coord_cartesian(ylim = c(-0.085, 0.02))

## brho intra ####
mBB = brho %>% 
  select(water, alpha_brho) %>%
  group_by(water) %>%
  summarise(mean_alpha = mean(alpha_brho)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")))


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
  coord_cartesian(ylim = c(-0.085, 0))


## put together ####
ggarrange(aL, bL, ab, ba, aa,bb, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom", align = "v", labels = "AUTO")

ggsave("figures/final_diss/Fig2_mutualism_params_with_sigmax.png", width = 7, height = 9)



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
ggarrange(aigr, bigr, ncol = 2, nrow = 1, legend = "bottom", align = "h", labels = "AUTO", common.legend = T)

ggsave("figures/final_diss/diss_done/Fig3_mutualism_igrs.png", width = 8, height = 4)
