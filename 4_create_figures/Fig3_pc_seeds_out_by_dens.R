
## compare ACAM biomass between sterilized and live soil 

# Set up ####
## read in data
source("data_cleaning/clean_model_dat.R")

library(wesanderson)
library(ggpubr)
library(car)

head(binter_for_RII_seed_analyses)

wes_palette("Moonrise2")
wes_palette("Royal2")

## num.bg.indiv - need bg indiv biomass
## aintra is probably the correct df for this

head(aintra)
unique(aintra$microbe)
## this does not have hte micrboe data; will need to find that


## need to compare per-cap bio of ACAM in sterilized vs. not
## will have to include water x density as well

# Plot ####
legume = aintra %>%
  filter(!unique.ID %in% rm.contaminated) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                           ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"),
         m.text = ifelse(microbe == "1", "Live", "Sterilized")) %>%
ggplot(aes(x=num.focal.indiv, y=seeds.out.percap, color = as.factor(m.text))) +
  geom_point() +
  facet_wrap(~water.text) +
  geom_smooth(method = "gam", alpha = 0.25) +
  xlab(" ") +
  ylab("Legume per-cap seeds out") +
  labs(color = "Soil") +
  scale_color_manual(values = c("#400227","#d2a2a7")) +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 14))

## "#a88185", "#a8819c", "#d2a2a7"

grass = binter_for_RII_seed_analyses %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"),
         m.text = ifelse(microbe == "1", "Live", "Sterilized")) %>%
  ggplot(aes(x=num.bg.indiv, y=seeds.out.percap, color = as.factor(m.text))) +
  geom_point() +
  facet_wrap(~water.text) +
  geom_smooth(method = "gam", alpha = 0.25) +
  xlab("Legume Density") +
  ylab("Grass per-cap seeds out") +
  labs(color = "Soil") +
  scale_color_manual(values = c(wes_palette("Cavalcanti1")[2], wes_palette("Cavalcanti1")[4])) +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 14))
#a88185

ggarrange(legume, grass, ncol = 1, common.legend = F, legend = "right", labels = "AUTO")

# ggsave("figures/Apr2025/final/Fig3_pc_seedsout_soil_trts.png", width = 8.5, height = 6)

# Model ####
amod = aintra %>%
  filter(!unique.ID %in% rm.contaminated)

ma = lm(seeds.out.percap ~ num.focal.indiv * microbe * water, data = amod)
summary(ma)

logma = lm(log(seeds.out.percap) ~ num.focal.indiv * microbe * water, data = amod)
summary(logma)

AIC(ma)
AIC(logma)
anova(ma, logma)

logma_coeff = as.data.frame(summary(logma)$coeff) %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(logma_coeff, "tables/acam_seeds_dens_water_microbe_lm_coeff_tab_20250429.csv", row.names = F)

#check_model(m2)

ma_tab = as.data.frame(Anova(logma, type = 3, test.statistic = "F")) %>%
  mutate(species = "A. americanus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
  #  select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(ma_tab, "tables/acam_intra_seeds_ANOVA_tab_20250429.csv", row.names = F)

bmod = binter_for_RII_seed_analyses %>%
  filter(!unique.ID %in% rm.contaminated)


mb = lm(seeds.out.percap ~ num.bg.indiv + microbe + water, data = bmod)
summary(mb)
logmb = lm(log(seeds.out.percap) ~ num.bg.indiv + microbe + water, data = bmod)
summary(logmb)
AIC(mb)
AIC(logmb)

logmb_coeff = as.data.frame(summary(logmb)$coeff) %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, round, digits = 3) 

write.csv(logmb_coeff, "tables/brho_seeds_dens_water_microbe_lm_coeff_tab_20250429.csv", row.names = F)

#check_model(m2)

Anova(logmb, type = 2, test.statistic = "F")

logmb_tab = as.data.frame(Anova(logmb, type = 3, test.statistic = "F")) %>%
  mutate(species = "B. hordeaceus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
  #  select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(logmb_tab, "tables/brho_inter_seeds_ANOVA_tab_20250429.csv", row.names = F)

