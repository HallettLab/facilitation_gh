## analyse leaf nutrient data

# Set up ----------------------
library(car)

## read in data
source("1_data_cleaning/clean_CN_dat.R")
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")

## join nutrient data with interactions
CN_final2 = CN_final %>%
  mutate(
    water = ifelse(
      water == 1, 
      "High",
      ifelse(
        water == 0.75, 
        "Intermediate",
        "Low")),
    microbe = ifelse(
      microbe == 1, 
      "Live", 
      "Sterilized")
    )

CN_AII = left_join(
  brho_AII, 
  CN_final2, 
  by = c(
    "unique.ID",
    "block",
    "water",
    "microbe",
    "rep")
  )

# models ---------------
##' Under water stress: interaction should be more competitive when 
##' there is greater water stress evident in leaves 
##' 
##' 
##' – this could be 
##' an overall relationship or an interaction between water treatment 
##' and water stress? – i.e. we may only see this relationship in low
##' water environment
##' 
##' DON'T use AIC for hypothesis testing
##' could instead use anova() to test hypotheses between models?

## check assumptions ---------


## run models ----------------
m1 = lm(
  NIntA ~ delta13C + water + num.bg.indiv + microbe, 
  data = CN_AII
)
summary(m1)
Anova(m1, type = 2)

## what to plot? 
## relationship between delta13C and interaction coeff
## relationship between water treatment and interaction coeff
## relationship between density and interaction coeff

m2 = lm(
  NIntA ~ WtN*water + microbe + num.bg.indiv, 
  data = CN_AII
)
summary(m2)
Anova(m2, type = 3)


m3 = lm(
  WtN ~ water*microbe + num.bg.indiv,
  data = CN_AII
)

summary(m3)
Anova(m3, type = 2)

# plots -----------------------
## panel a ------
a = ggplot(CN_AII, aes(x=ACAM.x, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction Index") +
  xlab("Legume Density") +
  labs(color = "Water") +
  theme(text = element_text(size=13))

## panel b ------
b = ggplot(CN_AII, aes(x=delta13C, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction Index") +
  xlab("Delta 13C") +
  labs(color = "Water") +
  theme(text = element_text(size=13))

## panel c ------
c = ggplot(CN_AII, aes(x=WtN, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction Index") +
  xlab("Leaf % N") +
  labs(color = "Water") +
  theme(text = element_text(size=13))

## panel d ------
d = ggplot(CN_AII, aes(x=water, y=WtN)) +
  geom_boxplot(aes(color = water)) +
  geom_jitter(aes(color = water), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Leaf % N") +
  xlab("Water Treatment") +
  labs(color = "Water") +
  theme(text = element_text(size=13))
## Don't separate by microbe because this is not significant.

ggarrange(
  a, b, c, d, 
  common.legend = T,
  labels = "auto",
  legend = "bottom"
  )

ggsave("../outputs/figures/Autumn_2026/Fig6_leaf_nutrient_AII.png",
       width = 7, 
       height = 6, 
       bg = "white"
       )

# Clean Env ####
rm(m1, m2, CN_clean, CN_final, CN_final2, test)
