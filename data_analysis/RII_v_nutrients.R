
# Set up ####

library(car)
library(ggpubr)

## Read in data 
source("data_analysis/RII/additive_intensity_index.R")

source("data_cleaning/clean_CN_dat.R")


CN_RII = left_join(CN_final, brho_RII, by = c("unique.ID", "block", "rep")) %>%
  filter(num.bg.indiv != 0)


# Plot ####
aii_m1 = brho_RII %>%
  filter(microbe == "Live") %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  #facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab("Additive Intensity Index") +
  labs(fill = "Water") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom")

aii_m0 = brho_RII %>%
  filter(microbe == "Sterilized") %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean.NIntA = mean(NIntA, na.rm = T),
            se.NIntA = calcSE(NIntA)) %>%
  
  ggplot(aes(x=ACAM, y=mean.NIntA, fill = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.NIntA - se.NIntA, ymax = mean.NIntA + se.NIntA)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  #facet_wrap(~microbe) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  xlab("Planted Legume Density") +
  ylab(" ") +
  labs(fill = "Water") +
  theme(text = element_text(size = 14)) +
  theme(legend.position = "bottom")

d13C = CN_RII %>%
  filter(microbe.x == 1) %>%
  ggplot(aes(x=delta13C, y=NIntA, color = water.y)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.15) +
  theme(text = element_text(size = 14)) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
  ylab("Additive Intensity Index") +
  xlab("Water Use Efficiency")

d13C_m0 = CN_RII %>%
  filter(microbe.x == 0) %>%
  ggplot(aes(x=delta13C, y=NIntA, color = water.y)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.15) +
  theme(text = element_text(size = 14)) +
  
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
  ylab("Additive Intensity Index") +
  xlab("Delta 13C")

leafN = CN_RII %>%
  filter(water.x != 0.75) %>%
  ggplot(aes(x=WtN, y=NIntA, color = water.y, linetype = as.factor(microbe.y), shape = microbe.y)) +
  geom_point() +
  theme(text = element_text(size = 14)) +
  
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  geom_smooth(method = "lm", alpha = 0.05) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
  ylab(" ") +
  xlab("Leaf % N") +
  labs(linetype = "Soil") +
  scale_shape_manual(values = c(19,1))
  

leafN_m0 = CN_RII %>%
  filter(microbe.x == 0) %>%
  ggplot(aes(x=WtN, y=NIntA, color = water.y)) +
  geom_point() +
  theme(text = element_text(size = 14)) +
  
  scale_color_manual(values = c("#
                                ", "#de8a5a")) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(color = "Water") +
  ylab(" ") +
  xlab("Leaf % N")


ggarrange(aii_m1, aii_m0, d13C, leafN, labels = "AUTO", common.legend = T, legend =  "bottom")

ggsave("figures/final_diss/diss_done/Fig2_RII_Nutrients.png", width = 7, height = 7)

# Model ####
mod_dat = CN_RII %>%
  filter(num.bg.indiv != 0, microbe.x == 1)

## delta 13C ####
m1 = lm(NIntA ~ delta13C * as.factor(water.x) + num.bg.indiv, data = mod_dat)
summary(m1)

m1_coeff = as.data.frame(summary(m1)$coeff) %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(m1_coeff, "tables/brho_AII_v_delta13C_tab_20250429.csv", row.names = F)

Anova(m1, type = 3, test.statistic = "F")

m1df = as.data.frame(Anova(m1, type = 3, test.statistic = "F")) %>%
  rownames_to_column(var = "coeff") %>%
  mutate_if(is.numeric, round, digits = 3)
  
write.csv(m1df, "tables/RII_v_delta13c.csv", row.names = F)

## %N ####
m2 = lm(NIntA ~ WtN + as.factor(water.x) + num.bg.indiv, data = mod_dat)
summary(m2)

Anova(m2, type = 2, test.statistic = "F")

m2_coeff = as.data.frame(summary(m2)$coeff) %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(m2_coeff, "tables/brho_AII_v_percN_tab_20250429.csv", row.names = F)

m2df = as.data.frame(Anova(m2, type = 2, test.statistic = "F")) %>%
  rownames_to_column(var = "coeff") %>%
  mutate_if(is.numeric, round, digits = 3)

write.csv(m2df, "tables/RII_v_leafN.csv", row.names = F)


#m3 = lm(NIntA ~ WtN + as.factor(water.x) + num.bg.indiv + microbe.y, data = CN_RII)
#summary(m3)
