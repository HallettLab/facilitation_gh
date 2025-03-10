
library(tidyverse)
library(lubridate)
library(stringr)

## Carmen's code to clean & separate relevant CN data!
#CN_data_raw <- read.csv("data/leaf_cn_raw_data.csv", skip = 5)

#sampling_data_raw <- read.csv("data/adult_sampling.csv")

#key = sampling_data_raw %>%
 # select(unique.ID, block, water, microbe, rep, bkgrd, ACAM, num.bg.indiv)

#names(CN_data_raw) <- c("SIRFER.num", "unique.ID", "weight", "delta_15N", "delta_13C", "WtN", "WtC", "CN")

#CN_clean = CN_data_raw %>%
 # filter(SIRFER.num == "", !unique.ID %in% c("", "Sample ID")) %>%
  #filter(!str_detect(unique.ID, "-")) %>%
#  mutate(unique.ID = as.numeric(unique.ID))%>%
 # mutate(delta_15N = as.numeric(delta_15N))%>%
  #mutate(delta_13C = as.numeric(delta_13C))%>%
#  mutate(WtN = as.numeric(WtN))%>%
 # mutate(WtC = as.numeric(WtC))%>%
  #mutate(CN = as.numeric(CN))

#CN_final = left_join(CN_clean, key, by = c("unique.ID")) %>%
 # filter(microbe == 1, water %in% c(0.6, 1), ACAM %in% c(0, 12, 24))

#write.csv(CN_final, "data/leaf_CN_data_for_Tetianna.csv", row.names = F)


## READ THE DATA FILE IN HERE! 
CN_final = read.csv("data/leaf_CN_data_for_Tetianna.csv")

##plot CN by water
ggplot(CN_final, aes(x=as.factor(water), y=CN)) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Water") +
  ylab("C:N Ratio")

## plot CN by density
ggplot(CN_final, aes(x=as.factor(ACAM), y=CN)) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Legume Density") +
  ylab("C:N Ratio")

## plot CN by interaction of water x dens
CN_final %>%
  mutate(w_d = paste0(water, "_", ACAM),
         w_d = as.factor(w_d),
         w_d = fct_relevel(w_d, "0.6_0", "1_0", "0.6_12", "1_12", "0.6_24", "1_24")) %>%
  ggplot(aes(x=w_d, y=CN, color = as.factor(water))) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Water x Density") +
  ylab("C:N Ratio") +
  scale_color_manual(values = c("#de8a5a", "#008080")) +
  labs(color = "Water")

#ggsave("CN_ratio.png", width = 5, height = 3) ## code to save the figure

AMF_CN = left_join(AMF_results, CN_final, by = c("unique.ID", "block", "water", "microbe", "rep", "bkgrd", "num.bg.indiv"))

ggplot(AMF_CN, aes(x=delta_15N, y=percent_colonization)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(AMF_CN, aes(x=CN, y=percent_colonization)) +
  geom_point() +
  geom_smooth(method = "lm")
## AMF col higher with less N

## C:N 

AMF_CN_bio = left_join(AMF_CN, dat, by = c("unique.ID", "block", "water", "microbe", "rep", "num.bg.indiv", "ACAM"))

ggplot(AMF_CN_bio, aes(x = percent_colonization, y = seeds.out.percap)) +
  geom_point() +
  geom_smooth(method = "lm")
## More AMF col leads to higher biomass/seed output because plants have access to more nutrients

ggplot(AMF_CN_bio, aes(x = num.bg.indiv, y = percent_colonization)) +
  geom_jitter() +
  geom_smooth(method = "lm")

na.check = AMF_CN_bio %>%
  filter(is.na(num.bg.indiv))
## track down why these didn't join in correctly - 3 dens 12 samples


## run a quick statistical test 
a1 = aov(CN ~ as.factor(ACAM) * as.factor(water), data = CN_final)
summary(a1)
## marginally significant difference between water treatments

## plot delta 15 N by density
ggplot(CN_final, aes(x=as.factor(ACAM), y=delta_15N)) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Legume Density") +
  ylab("delta 15 N")

## plot delta 15 N by water
ggplot(CN_final, aes(x=as.factor(water), y=delta_15N)) +
  geom_boxplot() +
  geom_jitter() +
  xlab("Water") +
  ylab("delta 15 N")

## delta 15 N by interaction of water x dens
CN_final %>%
  mutate(w_d = paste0(water, "_", ACAM),
         w_d = as.factor(w_d),
         w_d = fct_relevel(w_d, "0.6_0", "1_0", "0.6_12", "1_12", "0.6_24", "1_24")) %>%
ggplot(aes(x=w_d, y=delta_15N, color = as.factor(water))) +
  geom_boxplot() +
  geom_jitter() +
 # geom_point() +
  xlab("Water x Density") +
  ylab("delta 15 N") +
  scale_color_manual(values = c("#de8a5a", "#008080")) +
  labs(color = "Water")

a2 = aov(delta_15N ~ as.factor(ACAM) * as.factor(water), data = CN_final)
summary(a2)
## no significant differences
