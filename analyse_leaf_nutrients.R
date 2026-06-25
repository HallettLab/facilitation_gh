## Header ##


## analyse leaf nutrient data

# Set up ####
library(car)

## read in data
source("1_data_cleaning/clean_CN_dat.R")
source("1_data_cleaning/calc_additive_intensity_index.R")

## join nutrient data with interactions
CN_final2 = CN_final %>%
  mutate(water = ifelse(water == 1, "High", 
                        ifelse(water == 0.75, "Intermediate", 
                               "Low")),
         microbe = ifelse(microbe == 1, "Live", "Sterilized"))

test = left_join(brho_AII, CN_final2, by = c("unique.ID", "block", "water", 
                                            "microbe", "rep"))


## water stress predict interacitons?
ggplot(test, aes(x=delta13C, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction") +
  xlab("Delta 13C") +
  labs(color = "Water") +
  theme(text = element_text(size=15))
ggsave("../outputs/d13c_ints.png", width = 5, height = 3)  

m1 = lm(NIntA ~ delta13C + num.bg.indiv + water + microbe, data = test)
summary(m1)
Anova(m1)


## treatments predict water stress?
ggplot(test, aes(x=num.bg.indiv, y = delta13C)) +
  geom_point(aes(color = water)) +
  #  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  xlab("Density") +
  ylab("Delta 13C") +
  labs(color = "Water") +
  theme(text = element_text(size=15)) +
  coord_cartesian(xlim = c(0,45))


m2 = lm(delta13C ~ num.bg.indiv + water + microbe, data = test)




summary(m2)
Anova(m2)


ggplot(test, aes(x=delta13C, y=NIntA, color = WtN)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
 # scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction") +
  xlab("Delta 13C") +
  #labs(color = "Water") +
  theme(text = element_text(size=15))


  


ggplot(test, aes(x=WtN, y=NIntA)) +
  geom_point(aes(color = water)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Interaction") +
  xlab("Leaf % N") +
  labs(color = "Water") +
  theme(text = element_text(size=15))
ggsave("../outputs/percN_ints.png", width = 5, height = 3)  

ggplot(test, aes(x=num.bg.indiv, y=WtN)) +
  geom_point(aes(color = water)) +
  stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("#70a494",  "#f3d0ae", "#de8a5a")) +
  ylab("Leaf % N") +
  xlab("Density") +
  labs(color = "Water") +
  theme(text = element_text(size=15)) +
  coord_cartesian(xlim = c(0,45))


ggplot(test, aes(x=WtN, y=delta13C)) +
  geom_point() +
  stat_smooth(method = "lm")


