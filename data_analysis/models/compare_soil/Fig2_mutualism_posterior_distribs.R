
library(tidyverse)
library(ggpubr)
library(cowplot)

theme_set(theme_classic())

acam = read.csv("data/model_posteriors/acam_soil_comp_posts_20250420.csv")
brho = read.csv("data/model_posteriors/brho_soil_comp_posts_20250420.csv")

aa = acam %>%
  select(water, alpha_acam, alpha_acam_m0) %>%
  pivot_longer(cols = c("alpha_acam", "alpha_acam_m0"), names_to = "soil", values_to = "alpha") %>%
  mutate(soil = ifelse(soil == "alpha_acam", "Live", "Sterilized")) %>%
  
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
ggplot(aes(x=alpha, color = soil, fill = soil)) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  scale_fill_manual(values = c("#400227", "#d2a2a7")) +
  geom_density(alpha = 0.25, linewidth = 1) +
  facet_wrap(~water.text) +
  xlab("INTRA specific alpha") +
  ylab("Density")  +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-0.08, 0, by = 0.04)) +
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(fill = "Soil", color = "Soil") +
  theme(legend.position="none")

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

