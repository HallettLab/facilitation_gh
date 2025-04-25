
## create a figure that shows context dependent interactions + mechanisms

## igr_sig is created in the plot_alpha_functs_IGR_w_uncertainty.R script
## not clean enough to source the script yet, so will need to run manually to create df
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
  # facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

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
  xlab("Modeled Legume Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95, linewidth = 2))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(xlim = c(0, 42))

ggarrange(a, b, ncol = 2, common.legend = T, legend = "bottom")

ggsave("figures/final_diss/Fig4_density_dep_interactions.png", width = 8, height = 3.5)










## the df for this figure comes from leaf_nutrient_figs.R
b = CN_bio %>%
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

c = CN_bio %>%
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
  xlab(" ") +
  ylab("Leaf % N") +
  labs(color = "Water", shape = "Soil Treatment") +
  theme(text = element_text(size = 15))

ggarrange(a, b, c, ncol = 3, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Apr2025/final/Fig2_nL_interactions_mech.png", width = 8.5, height = 3.5)
