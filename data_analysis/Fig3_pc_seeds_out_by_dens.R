
## compare ACAM biomass between sterilized and live soil 

## read in data
source("data_cleaning/clean_model_dat.R")

library(wesanderson)

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

ggsave("figures/Apr2025/final/Fig3_pc_seedsout_soil_trts.png", width = 8.5, height = 6)
