
# Set up ####
## load packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(ggpubr)

## function to calculate SE
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in data 
## CN data
CN_data_raw = read.csv("data/leaf_cn_raw_data.csv", skip = 5)

## experimental design data
exp_design = read.csv("data/experimental_design_20250104.csv")

## rename CN data
names(CN_data_raw) <- c("sirfer_info", "unique.ID", "weight.mg", "delta15N", "delta13C", "WtN", "WtC", "CN")

theme_set(theme_classic())

# Clean Data ####
## remove info from SIRFER processing
CN_dat = CN_data_raw %>% 
  filter(sirfer_info == "",
         !unique.ID %in% c("", "Sample ID")) %>%
  select(-sirfer_info)

## create a key to match treatment info to CN data
sample_key = exp_design %>%
  select(unique.ID, block, water, microbe, rep, bkgrd, focal, ACAM, BRHO) %>%
  mutate(unique.ID = as.numeric(unique.ID))

## separate data to fix Unique ID's in Julia's data
## fix unique ID's in intermediate water level data
## these were from Julia's project, where we sampled at 2-weeks and 2-months post germination
CN_dat_Julia = CN_dat %>% 
  filter(str_detect(unique.ID, "-")) %>%
  mutate(unique.ID2 = str_split(unique.ID, "-") %>%
           sapply(head, 1), 
         days_post_germ = str_split(unique.ID, "-") %>%
           sapply(tail, 1), 
         unique.ID = unique.ID2) %>%
  select(-unique.ID2) %>%
  mutate_all(as.numeric) ## change all columns to numeric

CN_dat_Other <- CN_dat %>% 
  filter(!str_detect(unique.ID, "-")) %>%
  mutate(days_post_germ = 56) %>%
  mutate_all(as.numeric) ## change all columns to numeric

## re-join CN data
CN_all = rbind(CN_dat_Other, CN_dat_Julia)

## join CN data with experimental info
CN_clean = left_join(CN_all, sample_key, by = c("unique.ID"))

# Visualize ####
## Exploratory ####
CN_clean %>%
  filter(days_post_germ == 56, 
         BRHO == "focal") %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(ACAM = as.numeric(ACAM)) %>%
ggplot(aes(x=ACAM, y=CN, color = as.factor(water))) +
  geom_point() +
  facet_wrap(~microbe)

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal") %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=CN, color = as.factor(water))) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~water)

CN_clean %>%
  filter(days_post_germ == 56, 
         microbe != 0,
         BRHO == "focal") %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(water), y=CN, color = as.factor(water))) +
  geom_boxplot() +
  geom_jitter()

## what about N per mg??
CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal",
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
    ## I thought we only did BRHO...??
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=CN)) +
  geom_boxplot() +
  geom_jitter() + 
  facet_wrap(~ACAM, ncol = 4)
  

CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  
  mutate(N_per_mg = WtN/weight.mg) %>%
  
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=N_per_mg)) +
  geom_boxplot() +
  geom_point()

## Fig SXXX ####
leafCN = CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=CN)) +
  geom_boxplot() +
  geom_jitter() + 
  facet_wrap(~ACAM, ncol = 4) +
  xlab(" ") +
  ylab("Leaf C:N Ratio")

mg_N = CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% 
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=N_per_mg)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~ACAM, ncol = 4) +
  xlab(" ") +
  ylab("mg N / dried leaf tissue")

delta15N = CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=delta15N)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~ACAM, ncol = 4) +
  xlab("Soil Treatment") +
  ylab("delta 15 N")

ggarrange(leafCN, mg_N, delta15N, ncol = 1, labels = "AUTO")

ggsave("figures/Apr2025/Supp/leafN_soiltrt_v_dens.png", width = 8, height = 8)
## could show: leaf CN, N/mg tissue, delta 15N
## then would want colonization of these samples....

## More Explore ####
CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=delta15N)) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(water~ACAM) +
  xlab("Soil Treatment") +
  ylab("delta 15 N") 

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(microbe), y=delta13C)) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(water~ACAM) +
  xlab("Soil Treatment") +
  ylab("delta 13 C")

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(water), y=delta13C)) +
  geom_boxplot() +
  geom_jitter() +
 # facet_grid(water~ACAM) +
  xlab("Soil Treatment") +
  ylab("delta 13 C")


CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>% ## somehow there seem to be ACAM focals in here?? 
  ## I thought we only did BRHO...??
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(water), y=delta13C)) +
  geom_boxplot() +
  geom_jitter() +
  # facet_grid(water~ACAM) +
  xlab("Soil Treatment") +
  ylab("delta 13 C")


# Fig 6 ####
d13C = CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>% 
  mutate(N_per_mg = WtN/weight.mg) %>%
  group_by(ACAM) %>%
  summarise(mean_d13 = mean(delta13C),
            se_d13 = calcSE(delta13C)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=mean_d13, group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_d13 - se_d13, ymax = mean_d13 + se_d13), width = 0.25) +
  geom_line() +
  xlab(" ") +
  ylab("delta 13C") +
  theme(text = element_text(size = 15))

lCN = CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  group_by(ACAM) %>%
  summarise(meanCN = mean(CN),
            seCN = calcSE(CN)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=meanCN, group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanCN - seCN, ymax = meanCN + seCN), width = 0.25) +
  geom_line() +
  xlab(" ") +
  ylab("Leaf C:N Ratio") +
  theme(text = element_text(size = 15))

lNmg = CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  group_by(ACAM) %>%
  summarise(meanN = mean(N_per_mg),
            seN = calcSE(N_per_mg)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=meanN, group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanN - seN, ymax = meanN + seN), width = 0.25) +
  geom_line()+
  xlab(" ") +
  ylab("Leaf N / mg") +
  theme(text = element_text(size = 15))

d15N = CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>% 
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  group_by(ACAM) %>%
  summarise(mean15N = mean(delta15N),
            se15N = calcSE(delta15N)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=mean15N, group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean15N - se15N, ymax = mean15N + se15N), width = 0.25) +
  geom_line()+
  xlab("Legume Density") +
  ylab("Delta 15N") +
  theme(text = element_text(size = 15))

ggarrange(d13C, lNmg, d15N, ncol = 1, labels = "AUTO")

ggsave("figures/Apr2025/Fig4_isotopes_leafN_dens.png", width = 6, height = 6)


## Fig 6 take 2 ####
CN_clean %>%
  filter(days_post_germ == 56, 
         #water == 0.75,
         BRHO == "focal"#, microbe == 1
         ) %>% 
  mutate(N_per_mg = WtN/weight.mg) %>%
  group_by(ACAM, water, microbe) %>%
  summarise(mean_d13 = mean(delta13C),
            se_d13 = calcSE(delta13C)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=mean_d13, color = as.factor(water), shape = as.factor(microbe), group = interaction(water, microbe))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_d13 - se_d13, ymax = mean_d13 + se_d13), width = 0.25) +
  geom_line() +
  xlab(" ") +
  ylab("delta 13C") +
  theme(text = element_text(size = 15))

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal",
         microbe == 1) %>% 
  mutate(N_per_mg = WtN/weight.mg) %>%
  group_by(ACAM, water) %>%
  summarise(mean_d13 = mean(delta13C),
            se_d13 = calcSE(delta13C)) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=mean_d13, color = as.factor(water), group = water)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_d13 - se_d13, ymax = mean_d13 + se_d13), width = 0.25) +
  geom_line() +
  xlab(" ") +
  ylab("delta 13C") +
  theme(text = element_text(size = 15))












## Yet more exploring ####
### Tetianna's samples ####
CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=N_per_mg)) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Leaf N/mg")

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(water), y=delta15N)) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Delta 15N") +
  facet_wrap(~ACAM, ncol = 4)

### other ####
CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(water), y=delta15N, color = as.factor(microbe))) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Leaf N/mg") +
  facet_wrap(~ACAM, ncol = 4)

CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=delta15N, color = as.factor(water))) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Leaf N/mg") +
  facet_wrap(~as.factor(microbe), ncol = 4)



CN_clean %>%
  filter(days_post_germ == 56, 
         water == 0.75,
         BRHO == "focal") %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=N_per_mg)) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Leaf N/mg")

CN_clean %>%
  filter(days_post_germ == 14, 
         water == 0.75,
         BRHO == "focal") %>%
  mutate(N_per_mg = WtN/weight.mg) %>%
  mutate(ACAM = as.numeric(ACAM)) %>%
  ggplot(aes(x=as.factor(ACAM), y=CN)) +
  geom_boxplot() +
  geom_jitter() +
  xlab(" ") +
  ylab("Leaf C:N Ratio") 

# Run Models ####
## Water 1, 0.6 ####
CN_model_dat = CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg)

m1 = aov(delta15N ~ as.factor(microbe) + ACAM + as.factor(water) + as.factor(microbe):as.factor(water), data = CN_model_dat)
summary(m1)

TukeyHSD(m1)

m1$coefficients %>% as.data.frame() %>% write.csv(file = "test.csv")
m1$effects %>% as.data.frame() %>% write.csv(file = "test.csv")


TukeyHSD(m1)

ggplot(CN_model_dat, aes(x=as.factor(microbe), y=delta15N)) +
  geom_boxplot() +
  geom_jitter()

m2 = aov(N_per_mg ~ as.factor(microbe)  + ACAM +  as.factor(microbe):ACAM + as.factor(water), data = CN_model_dat)
summary(m2)

TukeyHSD(m2)


ggplot(CN_model_dat, aes(x=as.factor(water), y=N_per_mg)) +
  geom_boxplot() +
  geom_jitter()

m3 = aov(CN ~ as.factor(microbe)*ACAM + as.factor(water), data = CN_model_dat)
summary(m3)

TukeyHSD(m3)

ggplot(CN_model_dat, aes(x=as.factor(water), y=CN)) +
  geom_boxplot() +
  geom_jitter()

ggplot(CN_model_dat, aes(x=as.factor(microbe), y=CN, color = as.factor(ACAM))) +
  geom_boxplot() +
  geom_jitter()

m4 = aov(delta13C ~ as.factor(microbe) * ACAM * as.factor(water), data = CN_model_dat)
summary(m4)


## Water 0.75 ####
CN_model_dat = CN_clean %>%
  filter(days_post_germ == 56, 
         water != 0.75,
         BRHO == "focal", 
         ACAM != 60) %>%
  mutate(N_per_mg = WtN/weight.mg)
