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
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  scale_fill_manual(values = c("#400227", "#d2a2a7")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAL, aes(x=water.text, y=mean_lam, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab("Intrinsic Growth Rate") +
  xlab(NULL)  +
  labs(fill = "Soil", color = "Soil") +
  #guides(color = guide_legend("Soil", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank())

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
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  scale_fill_manual(values = c("#02401B", "#81A88D")) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBL, aes(x=water.text, y=mean_lam, color = soil)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, aes(fill = soil)) +
  ylab(" ") +
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil") +
 # guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank())

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
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  scale_fill_manual(values = c("#400227", "#d2a2a7")) +
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
  xlab(" ")  +
  labs(fill = "Soil", color = "Soil") +
  #guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
    theme(text = element_text(size=15),
          axis.text.x=element_blank()) +
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
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  scale_fill_manual(values = c("#02401B", "#81A88D")) +
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
  #guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0.02))


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
  geom_point(alpha = 0.15) +
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
  coord_cartesian(ylim = c(-0.085, 0.02))

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
  geom_point(alpha = 0.15) +
  geom_line(data = mBB, aes(x=water.text, y=mean_alpha, group = 1)) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    shape = 21, fill = "white") +
  ylab(" ") +
  xlab(" ")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
 # guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(-0.085, 0))


## put together ####
a = ggarrange(aL, ab, aa, aigr, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom", align = "v")
b = ggarrange(bL, ba, bb, bigr, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom", align = "v")

ggarrange(a, b, ncol = 2)

ggsave("figures/final_diss/Fig2_mutualism_params.png", width = 8, height = 12)



# Fig 3 ####
## acam igr ####
mAGR = gr_df %>%
  filter(focal == "ACAM") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

aigr = gr_df %>%
  filter(focal == "ACAM") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), color = soil, shape = type)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mAGR, aes(x=water.text, y=mean_gr, color = soil)) +
  
  
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  scale_fill_manual(values = c("#400227", "#d2a2a7")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(fill = "Soil", color = "Soil", shape = "Type") +
  ylab("Growth Rate") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  theme(text = element_text(size=15))

## brho igr ####
mBGR = gr_df %>%
  filter(focal == "BRHO") %>%
  group_by(water.text, soil, type) %>%
  summarise(mean_gr = mean(growth_rate))

bigr = gr_df %>%
  filter(focal == "BRHO") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=water.text, y=growth_rate, group = interaction(soil, type), color = soil, shape = type)) +
  geom_point(alpha = 0.15) +
  geom_line(data = mBGR, aes(x=water.text, y=mean_gr, color = soil)) +
  
  
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  scale_fill_manual(values = c("#02401B", "#81A88D")) +
  stat_summary(
    fun = "mean",        
    geom = "point",
    col = "black",
    size = 4,
    aes(fill = soil, shape = type)) +
  scale_shape_manual(values = c(21, 22)) +
  
  labs(fill = "Soil", color = "Soil", shape = "Type") +
  ylab(" ") +
  xlab("Water Level") +
  guides(fill = guide_legend("Soil", override.aes = list(shape = 21))) +
  theme(text = element_text(size=15))

## put together ####






# Fig 2 old ####
## acam lambda ####
aL = acam %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = soil, y=lam, color = soil)) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  ylab("Intrinsic Growth Rate") +
  xlab(NULL)  +
  facet_wrap(~water.text) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(fill = "Soil", color = "Soil") +
  guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## brho lambda ####
bL = brho %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lam") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = soil, y=lam, color = soil)) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  ylab("Intrinsic Growth Rate") +
  xlab(NULL)  +
  facet_wrap(~water.text) +
  labs(fill = "Soil", color = "Soil") +
  guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## acam intra ####
aa = acam %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x = soil, y=alpha, color = soil)) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  ylab("INTRA specific alpha") +
  xlab(NULL)  +
  facet_wrap(~water.text) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(fill = "Soil", color = "Soil") +
  guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15)) +
  scale_y_continuous(breaks = seq(-0.08, 0, by = 0.04))

## acam inter
ab = acam %>%
 # select(water, alpha_acam, alpha_acam_m0) %>%
 # pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
#  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"),
         soil = "Both") %>%
  
  ggplot(aes(x=soil, y=alpha_brho)) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  ylab("INTER specific alpha") +
  xlab(NULL)  +
  facet_wrap(~water.text) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #labs(fill = "Soil", color = "Soil") +
  guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15)) +
  theme(#text = element_text(size=13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))


## brho inter ####
ba = brho %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%

  ggplot(aes(x = soil, y=alpha, color = soil)) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  ylab("INTER specific alpha") +
  xlab(NULL)  +
  facet_wrap(~water.text) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(fill = "Soil", color = "Soil") +
  guides(color = guide_legend("Soil Treatment", override.aes = list(linewidth = 1))) +
  theme(text = element_text(size=15))

a = ggarrange(aL, aa, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")
b = ggarrange(bL, ba, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", align = "v")

ggarrange(a, b, ncol = 2)

ggsave("figures/final_diss/Fig2_mutualism_params.png", width = 12, height = 7)



# OLD ####

aL = acam %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lambda") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=lambda, color = soil, fill = soil)) +
  geom_density(aes(x=lambda), alpha = 0.5, linewidth = 1) +
 # geom_density(aes(x=lambda_m0), color = "#d2a2a7", fill = "#d2a2a7", alpha = 0.5, linewidth = 1) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  scale_fill_manual(values = c("#400227", "#d2a2a7")) +
  facet_wrap(~water.text) +
  xlab("Lambda") +
  ylab(" ") +
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(fill = "Soil", color = "Soil")

ba = brho %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=alpha, color = soil, fill = soil)) +
  geom_density(alpha = 0.5, linewidth = 1) +
 # geom_density(aes(x=alpha_acam_m0), color = "#81A88D", fill = "#81A88D", alpha = 0.5, linewidth = 1) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  scale_fill_manual(values = c("#02401B", "#81A88D")) +
  facet_wrap(~water.text) +
  xlab("INTER specific alpha") +
  ylab("Density")  +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-0.08, 0, by = 0.02)) +
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(fill = "Soil", color = "Soil") +
  theme(legend.position="none")

bL = brho %>%
  select(water, lambda, lambda_m0) %>%
  pivot_longer(cols = c("lambda", "lambda_m0"), names_to = "soil", values_to = "lambda") %>%
  mutate(soil = ifelse(soil == "lambda", "Live", "Sterilized")) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=lambda, color = soil, fill = soil)) +
  geom_density(alpha = 0.5, linewidth = 1) +
  #geom_density(aes(x=lambda_m0), color = "#81A88D", fill = "#81A88D", alpha = 0.15, linewidth = 1) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  scale_fill_manual(values = c("#02401B", "#81A88D")) +
  facet_wrap(~water.text) +
  xlab("Lambda") +
  ylab(" ")  +
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(fill = "Soil", color = "Soil")

alphas = ggarrange(aa, ba, ncol = 1, nrow = 2, labels = "AUTO", align = "h")
lambdas = ggarrange(aL, bL, ncol = 1, nrow = 2, labels = "AUTO", align = "h")

plot_grid(aa, aL, ba, bL, rel_widths = c(1, 1.25), labels = c('A', 'B', 'C', 'D'), align = "h")

#ggarrange(aa, aL, ba, bL, ncol = 2, nrow = 2, labels = "AUTO", align = "h")
ggsave("figures/Apr2025/final/mutualism_posteriors.png", width = 9, height = 4.5)

