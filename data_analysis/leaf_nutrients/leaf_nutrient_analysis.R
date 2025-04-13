
# Set up ####
source("data_cleaning/clean_CN_dat.R")

theme_set(theme_classic())


ggplot(CN_final, aes(x=delta15N, y=delta13C)) +
  geom_point() +
  geom_smooth(method = "lm")
## there is a positive relationship between delta 15N and delta 13C - just good to be aware of

# Figs ####
## d13 C ####
## dens x water patterns in live soil only
CN_final %>%
  filter(microbe == 1) %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean_d13 = mean(delta13C),
            se_d13 = calcSE(delta13C)) %>%
  mutate(ACAM = as.numeric(ACAM),
         water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized")) %>%
  ggplot(aes(x=ACAM, y=mean_d13, shape = as.factor(microbe.text), fill = water.text, group = interaction(water, microbe))) +
  geom_errorbar(aes(ymin = mean_d13 - se_d13, ymax = mean_d13 + se_d13), width = 2) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Legume Density") +
  ylab("delta 13C") +
  theme(text = element_text(size = 15)) +
 # facet_wrap(~water.text) +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a"))+
  scale_shape_manual(values = c(21, 22)) +
  labs(shape = "Soil Treatment")

# ggsave("figures/Apr2025/leaf_nutrient_story/alldens_livesoil_d13C.png", width = 8, height = 3.5)


## d15 N ####
CN_final %>%
  group_by(ACAM, water, microbe) %>%
  filter(microbe == 1) %>%
  summarise(mean_d15 = mean(delta15N),
            se_d15 = calcSE(delta15N)) %>%
  mutate(ACAM = as.numeric(ACAM),
         water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized")) %>%
  ggplot(aes(x=ACAM, y=mean_d15, shape = as.factor(microbe.text), fill = water.text, group = interaction(water, microbe))) +
  geom_errorbar(aes(ymin = mean_d15 - se_d15, ymax = mean_d15 + se_d15), width = 2) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Legume Density") +
  ylab("delta 15N") +
  theme(text = element_text(size = 15)) +
  #facet_wrap(~water.text) +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a"))+
  scale_shape_manual(values = c(21, 22)) +
  labs(shape = "Soil Treatment")
# ggsave("figures/Apr2025/leaf_nutrient_story/alldens_livesoil_d15N.png", width = 8, height = 3.5)

CN_final %>%
  group_by(ACAM, water, microbe) %>%
  filter(water != 0.75) %>%
  summarise(mean_d15 = mean(delta15N),
            se_d15 = calcSE(delta15N)) %>%
  mutate(ACAM = as.numeric(ACAM),
         water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized")) %>%
  ggplot(aes(x=ACAM, y=mean_d15, shape = as.factor(microbe.text), fill = water.text, group = interaction(water, microbe))) +
  geom_errorbar(aes(ymin = mean_d15 - se_d15, ymax = mean_d15 + se_d15), width = 2) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Legume Density") +
  ylab("delta 15N") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~water.text) +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  scale_fill_manual(values = c("#70a494", "#de8a5a"))+
  scale_shape_manual(values = c(21, 22)) +
  labs(shape = "Soil Treatment")
ggsave("figures/Apr2025/leaf_nutrient_story/water_lo_hi_d15N.png", width = 8, height = 3.5)

## %N ####
N = CN_final %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean_N = mean(WtN),
            se_N= calcSE(WtN)) %>%
  mutate(ACAM = as.numeric(ACAM),
         water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized")) %>%
  ggplot(aes(x=ACAM, y=mean_N, shape = as.factor(microbe.text), fill = water.text, group = interaction(water, microbe))) +
  geom_errorbar(aes(ymin = mean_N - se_N, ymax = mean_N + se_N), width = 2) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Legume Density") +
  ylab("%N") +
  theme(text = element_text(size = 15)) +
#  facet_wrap(~water.text) +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a"))+
  scale_shape_manual(values = c(21, 22)) +
  labs(shape = "Soil Treatment")

