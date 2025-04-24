
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




#group = interaction(as.factor(soil), 


igr_microbe %>%
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  
  mutate(int = interaction(focal, as.factor(soil)),
         int = fct_relevel(int, "BRHO.Live", "BRHO.Sterilized", "ACAM.Live", "ACAM.Sterilized")) %>%
  
  
  ggplot(aes(x=int, y=igr, color = int)) +
  geom_jitter(size = 0.75) +
  facet_wrap(~water.text) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  scale_color_manual(values = c("#02401B", "#81A88D", "#400227", "#d2a2a7")) +
  labs(color = "Focal x Soil") +
  ylab("Invasion Growth Rate") +
  xlab("") +
  guides(color = guide_legend("Focal x Soil", override.aes = list(linewidth = 1))) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(text = element_text(size=15)) 

ggsave("figures/Apr2025/final/Fig3_IGR_soil_comparison.png", width = 10, height =4.5)

#position = position_jitterdodge(dodge.width = 1)

