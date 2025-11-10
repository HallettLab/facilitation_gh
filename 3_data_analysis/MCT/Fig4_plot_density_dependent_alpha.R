igr_sig = read.csv("data_analysis/MCT/output/igr_sigmoidal_20250428.csv")

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

library(tidyverse)

theme_set(theme_classic())

## create mean df ####
bigr_mean = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  # filter(focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         focal = as.factor(focal), 
         focal = fct_relevel(focal, "BRHO", "ACAM"))

# Talk Figure ####
## Plot density dependent interactions & IGRs from sigmoidal models
igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  # geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Interspecific Alpha") +
  xlab("Legume Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15))

ggsave("figures/dissertation_talk/dens_dep_alpha.png", width = 7, height = 3.5)

## with restricted x-axis
igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  # geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Interspecific Alpha") +
  xlab("Legume Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(xlim = c(0,75))

ggsave("figures/dissertation_talk/dens_dep_alpha_restrict_x.png", width = 6, height = 3.5)



# OLD ####
# Fig 3 ####
## brho alpha ####
brho_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  filter(focal == "BRHO") %>%
  ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water.text), color = as.factor(water.text))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  xlab(" ") +
  ylab("Interspecific Alpha") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))

## acam alpha ####
acam_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  filter(focal == "ACAM") %>%
  ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))
#coord_cartesian(xlim = c(0,50)) #+
#coord_cartesian(xlim = c(0,30), ylim = c(-0.06, 0.15))

## brho igr ####
brho_igr = igr_sig %>%
  filter(focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ggplot(aes(x=dens, y=mean.igr, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = mean.igr - (2*se.igr), ymax = mean.igr + (2*se.igr)), alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Neighbor Density") +
  ylab("Invasion Growth Rate") +
  labs(fill = "Water", color = "Water") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-2,8))

## acam igr ####
acam_igr = igr_sig %>%
  filter(focal == "ACAM") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ggplot(aes(x=dens, y=mean.igr, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = mean.igr - (2*se.igr), ymax = mean.igr + (2*se.igr)), alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Neighbor Density") +
  ylab(" ") +
  labs(fill = "Water", color = "Water") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-2,8))

ggarrange(brho_alpha, acam_alpha, brho_igr, acam_igr, labels = "AUTO", common.legend = TRUE, ncol = 2, nrow = 2, legend = "bottom", align = c("v"))
#

#ggsave("figures/Apr2025/Fig3_alphas_igrs_dens_with0.png", width = 5.8, height = 6)



# Fig 5 ####
## create mean df ####
bigr_mean = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  # filter(focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         focal = as.factor(focal), 
         focal = fct_relevel(focal, "BRHO", "ACAM"))

## brho alpha 
## alpha
ba = igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  # geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Interspecific Alpha") +
  xlab(" ") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15)) #+
# coord_cartesian(ylim = c(-0.08, 0.2))

## acam alpha
aa = igr_sig %>%
  filter(focal == "ACAM") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab(" ") +
  xlab(" ") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15))# +
#coord_cartesian(ylim = c(-0.08, 0.2))

## brho igr
bigr = igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  
  ggplot(aes(x=dens, y = igr,  fill = as.factor(water.text))) +
  
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  ylab("Invasion Growth Rate") +
  xlab("Neighbor Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-2.5, 8))

#ggsave("test.png", width = 5, height = 3)
aigr = igr_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  
  ggplot(aes(x=dens, y = igr,  fill = as.factor(water.text))) +
  
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  ylab(" ") +
  xlab("Neighbor Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 0.75) +
  guides(color = guide_legend("Water", override.aes = list(size = 3, alpha = 1, linewidth = 1))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-2.5, 8))


ggarrange(ba, aa, bigr, aigr, labels = "AUTO", common.legend = T, ncol = 2, nrow = 2, legend = "bottom", align = c("v"))

ggsave("figures/Apr2025/Fig3_alpha_igr_dens.png", width = 7, height = 7)















#ggsave("figures/Apr2025/Fig5_igr_dens4.png", width = 7, height = 3.5)



























# OLD ####
brho_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  filter(focal == "BRHO") %>%
  ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water.text), color = as.factor(water.text))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  xlab(" ") +
  ylab("Interspecific Alpha") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))