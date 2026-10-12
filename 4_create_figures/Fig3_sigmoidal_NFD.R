source("3_data_analysis/3_NFD_sigmoidal.R")

### Plot ####
# Fig 3 ####
NDF_sig_means = NDF_sig %>% 
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  group_by(water, sp) %>%
  summarise(mean_niche = mean(Niche), 
            mean_fitness = mean(Fitness)) %>%
  mutate(water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate")))


NDF_sig %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  ggplot(aes(x=Niche, y= Fitness, shape = sp, color = as.factor(water))) +
  geom_point(alpha = 0.25, aes(fill = as.factor(water))) +
  geom_point(data = NDF_sig_means, aes(x=mean_niche, y=mean_fitness, fill = as.factor(water)), size = 4, color = "black") +
  theme_classic() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_shape_manual(values = c(21, 22)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  labs(color = "Water", shape = "Species") +
  xlab("Niche Differences") +
  ylab("Fitness Differences") +
  theme(text = element_text(size=14))

ggsave("../outputs/figures/Autumn_2026/Fig3_NFD_sig.png", width = 7, height = 5)
