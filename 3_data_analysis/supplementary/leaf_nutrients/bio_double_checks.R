


test = binter_for_RII_seed_analyses %>%
  filter(ACAM == "0")



ggplot(test, aes(x = as.factor(water), y=total.bio.percap)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~microbe) +
  xlab("Water Treatment") + 
  ylab("Total Biomass per-capita (g)")

ggsave("figures/Apr2025/leaf_nutrient_story/dens0_biomass_percap.png", width = 6, height = 3)

binter_for_RII_seed_analyses %>%
group_by(ACAM, water, microbe) %>%
  summarise(mean_bio = mean(total.bio.percap),
            se_bio = calcSE(total.bio.percap)) %>%
  mutate(ACAM = as.numeric(ACAM),
         water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized")) %>%
  ggplot(aes(x=ACAM, y=mean_bio, shape = as.factor(microbe.text), fill = water.text, group = interaction(water, microbe))) +
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio), width = 2) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Legume Density") +
  ylab("Bio Per-Cap (g)") +
  theme(text = element_text(size = 15)) +
  # facet_wrap(~water.text) +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a"))+
  scale_shape_manual(values = c(21, 22)) +
  labs(shape = "Soil Treatment")

ggsave("figures/Apr2025/leaf_nutrient_story/alldens_bio_percap.png", width = 8, height = 3.5)
