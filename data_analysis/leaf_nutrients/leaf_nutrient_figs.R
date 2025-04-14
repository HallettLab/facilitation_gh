
# Set up ####
source("data_cleaning/clean_model_dat.R")
source("data_cleaning/clean_CN_dat.R")

theme_set(theme_classic())

# Join Data ####
names(binter_for_RII_seed_analyses)
names(CN_final)

## join
CN_bio = left_join(CN_final, binter_for_RII_seed_analyses, by = c("unique.ID", "water", 
                                                         "microbe", "rep"))
## 114 and 84 did not join correctly, look into this later!
## ah, these were contaminated samples; removed in cleaning script now

##interesting that there are not final densities greater than 40; this indicates that this approach might be more realistic than doing the planted densities? 

## probably would be good to do a metric of self thinning....
## tried this and self thinning does not really correlate with any of the leaf nutrient or final biomass measures
## good to check, but no clear pattern there 

# Fig 4 ####
d13 = CN_bio %>%
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

wtN = CN_bio %>%
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
  xlab("Legume Density") +
  ylab("Leaf % N") +
  labs(color = "Water", shape = "Soil Treatment") +
  theme(text = element_text(size = 15))

ggarrange(d13, wtN, common.legend = T, legend = "bottom", labels = "AUTO")

ggsave("figures/Apr2025/Fig4_leaf_nutrients.png", width = 7, height = 3.5)


# Fig SXX ####
CN_bio %>%
  filter(water != 0.75) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = delta15N, color = as.factor(water.text), shape = microbe.text, linetype = microbe.text)) +
  
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#de8a5a",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("Delta 15N") +
  labs(color = "Water", shape = "Soil Treatment", linetype = "Soil Treatment") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~water.text)

CN_bio %>%
  filter(water != 0.75) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = CN, color = as.factor(water.text), shape = microbe.text, linetype = microbe.text)) +
  
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#de8a5a",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("CN Ratio") +
  labs(color = "Water", shape = "Soil Treatment", linetype = "Soil Treatment") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~water.text)

CN_bio %>%
  filter(water != 0.75) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = WtN, color = as.factor(water.text), shape = microbe.text, linetype = microbe.text)) +
  
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#de8a5a",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("% N") +
  labs(color = "Water", shape = "Soil Treatment", linetype = "Soil Treatment") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~water.text)

CN_bio %>%
  filter(water != 0.75) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = delta13C, color = as.factor(water.text), shape = microbe.text, linetype = microbe.text)) +
  
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#de8a5a",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("Delta 13C") +
  labs(color = "Water", shape = "Soil Treatment", linetype = "Soil Treatment") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~water.text)


# Other Exploration ####
CN_bio %>%
  filter(microbe == 1) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = CN, color = as.factor(water.text))) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("CN Ratio") +
  labs(color = "Water", shape = "Soil Treatment") +
  theme(text = element_text(size = 15))

CN_bio %>%
  filter(microbe == 1) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate", "Low")),
         microbe.text = ifelse(microbe == 1, "Live", "Sterilized"),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High")) %>%
  ggplot(aes(x=num.bg.indiv, y = delta15N, color = as.factor(water.text))) +
  geom_point() +
  #facet_wrap(~water.text) +
  geom_smooth(method = "lm", linewidth = 2, alpha = 0.25) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae",  "#70a494")) +
  scale_shape_manual(values = c(19, 21)) +
  xlab("Legume Density") +
  ylab("Delta 15N") +
  labs(color = "Water", shape = "Soil Treatment")

