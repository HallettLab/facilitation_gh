## Plot seed output

## read in data
source("data_cleaning/clean_model_dat.R")


binter %>%
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.spc = mean(seeds.percap, na.rm = T),
            se.spc = calcSE(seeds.percap)) %>%
  filter(microbe == 1) %>%
  #mutate(microbe == "Live Soil") %>%
  
  ggplot(aes(x=ACAM, y=mean.spc, fill = as.factor(water))) +
 # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.spc - se.spc, ymax = mean.spc + se.spc), width = 1) +
  geom_line() +
  geom_point(aes(fill = as.factor(water)), colour = "black", pch = 21, size = 3.5) +
  #facet_wrap(~microbe) +
  scale_fill_manual(values = c("#de8a5a", "#f6edbd", "#008080")) +
  xlab("Planted Legume Density") +
  ylab("Seed Production (per cap.)") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

# ggsave("figures/MS_draft2/Fig2pt2_seedsout_planted_dens.png", width = 5, height = 4)

