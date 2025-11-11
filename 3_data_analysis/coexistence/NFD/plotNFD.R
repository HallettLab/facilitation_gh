
library(ggpubr)


NFD_plot_stat = NDF_static %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  mutate(model = "static")







## Prep for plotting

NFD_plot = NDF_sig %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  mutate(model = "sigmoidal")



fd = NFD_plot %>%
  ggplot(aes(x = sp, y = Fitness, color = water)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water)  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ylab("Fitness Inequality") +
  xlab("Species") +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  labs(color = "Water")
ggsave("figures/Apr2025/Fig2alt_fd_boxplots.png", width = 7, height = 3)


nd = NFD_plot %>%
  ggplot(aes(x = sp, y = Niche, color = water)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water, ncol = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ylab("Niche Differences") +
  xlab("Species") +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a"))

ggarrange(nd, fd, ncol = 2, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Apr2025/Fig2alt_nfd_boxplots.png", width = 4, height = 5)



## BRHO persistence with or without BRHO interactions 
NFDtog = rbind(NFD_plot, NFD_plot_stat)



a = NFDtog %>%
  filter(sp == "BRHO") %>%
  ggplot(aes(x=model, y = Niche, color = water)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  xlab(" ") +
  ylab("Niche Differences")

b = NFDtog %>%
  filter(sp == "BRHO", Fitness > -20) %>%
  ggplot(aes(x=model, y = Fitness, color = water)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  ylab("Fitness Differences") +
  xlab("Model")

ggarrange(a, b, labels = "AUTO", common.legend = T, ncol = 1, legend = "bottom")

ggsave("figures/Apr2025/statvsig_NFD.png", width = 7, height = 5)
## why would there be greater fitness diffs in sigmoidal model??
##is this biologically meaningful??







