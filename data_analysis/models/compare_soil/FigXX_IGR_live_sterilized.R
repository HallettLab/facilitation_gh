
## Plot IGR diffs with m0 and m1

igr_microbe = rbind(igr_m1, igr_m0)
## write.csv(igr_microbe, "data_analysis/models/compare_soil/igr_20250420_models.csv", row.names = F)

b = igr_microbe %>%
  filter(focal == "BRHO") %>%
  
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x=as.factor(soil), y=igr, color = as.factor(soil))) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water.text) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  labs(color = "Soil") +
  ylab("Grass Invasion Growth Rate") +
  xlab("Soil Treatment")

a = igr_microbe %>%
  filter(focal == "ACAM") %>%
  
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  ggplot(aes(x=as.factor(soil), y=igr, color = as.factor(soil))) +
  geom_jitter() +
  facet_wrap(~water.text) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  labs(color = "Soil") +
  ylab("Legume Invasion Growth Rate") +
  xlab("")

ggarrange(a, b, ncol = 1, labels = "AUTO")

ggsave("figures/Apr2025/final/Fig6_igr_microbe_comparison.png", width = 8, height = 6)

ggplot(igr_microbe, aes(x=alpha_inter, y=igr, color = as.factor(water))) +
  geom_point() +
  facet_grid(microbe~focal, scales = "free")
