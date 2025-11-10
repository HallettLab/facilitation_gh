
# Set up ####
## Scripts to manually run to get needed dfs
## 1) additive_intensity_index.R
## 2) MCT/calc_IGR.R
## 3) leaf_nutrient_figs.R

## create a figure that shows context dependent interactions + mechanisms

## igr_sig is created in the plot_alpha_functs_IGR_w_uncertainty.R script
## not clean enough to source the script yet, so will need to run manually to create df

# Plot ####
## raw AII ####
a = brho_RII %>%
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
  xlab("Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

## Modeled int ####
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

b = igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Interspecific Alpha") +
  xlab("Legume Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95, linewidth = 2))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(xlim = c(0, 42))

b
## the df for this figure comes from leaf_nutrient_figs.R
c = CN_RII %>%
  filter(microbe.x == 1) %>%
  mutate(water.text = ifelse(water.x == 1, "High",
                             ifelse(water.x == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=delta13C, y=NIntA, color = water.text)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.15) +
  theme(text = element_text(size = 15)) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
 # facet_wrap(~microbe.x) +
  ylab("Additive Intensity Index") +
  xlab("Delta 13C")

d = CN_RII %>%
  filter(microbe.x == 1) %>%
  mutate(water.text = ifelse(water.x == 1, "High",
                             ifelse(water.x == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=WtN, y=NIntA, color = water.text)) +
  geom_point() +
  theme(text = element_text(size = 15)) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
  ylab("Additive Intensity Index") +
  #facet_wrap(~microbe.x) +
  xlab("Leaf % N")

ggarrange(a, b, c, d, ncol = 2, nrow = 2, common.legend = T, legend = "bottom", align = "hv", labels = "AUTO")

ggsave("figures/final_diss/Fig4_density_dep_interactions_mech.png", width = 8, height = 7)

# Fig SXX ####
sa = CN_bio %>%
  filter(microbe == 1) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = delta13C, color = as.factor(water.text))) +
  geom_point() +
  geom_smooth(method = "gam", linewidth = 1.5, alpha = 0.25) +
  
  scale_color_manual(values = c("#de8a5a", "#f3d0ae",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("Delta 13C") +
  labs(color = "Water", shape = "Soil Treatment") +
  theme(text = element_text(size = 15))

sb = CN_bio %>%
  filter(microbe == 1) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = WtN, color = as.factor(water.text))) +
  geom_point() +
  geom_smooth(method = "gam", linewidth = 1.5, alpha = 0.25) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("Leaf % N") +
  labs(color = "Water", shape = "Soil Treatment") +
  theme(text = element_text(size = 15))

ggarrange(sa, sb, ncol = 2, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/final_diss/supp/leafnut_dens.png", width = 8, height = 4)
