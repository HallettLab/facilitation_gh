

## calculate mean control biomass for use in RII calcs
controls = brho.model %>%
  filter(num.bg.indiv == 0) %>%
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  group_by(water, microbe) %>%
  summarise(mean.control = mean(seeds.percap))

## calculate RII comparing 0 background to all other densities
brho_RII = left_join(brho.model, controls, by = c("water", "microbe")) %>%
  filter(num.focal.indiv < 5) %>%
  mutate(seeds.percap = seeds.out/num.focal.indiv) %>%
  
  mutate(RII = (seeds.percap - mean.control) / (mean.control + seeds.percap),
         microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil"), 
         water = ifelse(water == 1, "High",
                        ifelse(water == 0.75, "Intermediate",
                               "Low"))) %>%
  filter(!unique.ID %in% rm.contaminated)

## RII by final density
brho_RII %>%
  filter(num.bg.indiv != 0) %>%
  ggplot(aes(x=num.bg.indiv, y=RII, color = water)) +
  geom_point() +
  #geom_smooth()+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~microbe) +
  ylab("Relative Interaction Intensity") +
  xlab("Final Legume Density") +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("#008080", "#f6edbd", "#de8a5a"))

## RII by planted density
brho_RII %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.RII = mean(RII, na.rm = T),
            se.RII = calcSE(RII)) %>%
  
  ggplot(aes(x=ACAM, y=mean.RII, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.RII - se.RII, ymax = mean.RII + se.RII)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a")) +
  xlab("Planted Legume Density") +
  ylab("Relative Interaction Intensity") +
  labs(fill = "Water") +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

## Seed output by final density
ggplot(brho_RII, aes(x=num.bg.indiv, y=seeds.percap, fill = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3) +
  facet_wrap(~microbe) +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a"))

## this would be good as a supplementary figure I think!
ggplot(brho_RII, aes(x=num.bg.indiv, y=seeds.percap, fill = microbe)) +
  geom_point(aes(fill = microbe), colour = "black", pch = 21, size = 3) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("#6699CC", "#661100")) +
  ylab("Seeds per Capita") +
  xlab("Final Legume Density") +
  labs(fill = NULL)
ggsave("figures/MS_version1/FigS1_seedsout_num_bg.png", width = 9, height = 3.5)


## Seed output by planted density





