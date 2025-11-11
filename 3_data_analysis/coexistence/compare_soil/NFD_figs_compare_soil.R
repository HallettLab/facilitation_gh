
# Compare ACAM ND, FD ####
## Prep data ####
m1brho = NDF_m1 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "BRHO")  %>%
  mutate(microbe = 1)

m0brho = NDF_m0 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "BRHO") %>%
  mutate(microbe = 0)

FD_brho = rbind(m1brho, m0brho)

## Plot ####
NDb = FD_brho %>%
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live")) %>%
  ggplot(aes(x=as.factor(soil), y=Niche, color = as.factor(soil))) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab(" ") +
  ylab("Niche Differences")

FDb = FD_brho %>%
  #filter(water != "Intermediate") %>%
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live")) %>%
  ggplot(aes(x=as.factor(soil), y=Fitness, color = as.factor(soil))) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  xlab("Soil Treatment") +
  ylab("Fitness Differences")

ggarrange(NDb, FDb, ncol = 1,labels = "AUTO")
ggsave("figures/Apr2025/final/FigXX_NFD_microbe_comparison_BRHO.png", width = 8, height = 6)

## something is off in the intermediate treatment... 


# Compare ACAM ND, FD ####
## Prep data ####
m1acam = NDF_m1 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "ACAM")  %>%
  mutate(microbe = 1)

m0acam = NDF_m0 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "ACAM") %>%
  mutate(microbe = 0)

FD_acam = rbind(m1acam, m0acam)

## Plot ####
NDa = FD_acam %>%
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live")) %>%
  ggplot(aes(x=as.factor(soil), y=Niche, color = as.factor(soil))) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab(" ") +
  ylab("Niche Differences")

FDa = FD_acam %>%
  mutate(soil = ifelse(microbe == 0, "Sterilized", "Live")) %>%
  ggplot(aes(x=as.factor(soil), y=Fitness, color = as.factor(soil))) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~water) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  xlab("Soil Treatment") +
  ylab("Fitness Differences")

ggarrange(NDa, FDa, ncol = 1,labels = "AUTO")
ggsave("figures/Apr2025/final/FigXX_NFD_microbe_comparison_ACAM.png", width = 8, height = 6)

