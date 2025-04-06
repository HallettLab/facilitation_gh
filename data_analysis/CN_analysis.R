
# Set up ####
## load packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(ggpubr)

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

# Fig SXXX ####
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

ggarrange(leafCN, mg_N, delta15N, ncol = 1)

ggsave("figures/Apr2025/Supp/leafN_soiltrt_v_dens.png", width = 8, height = 8)
## could show: leaf CN, N/mg tissue, delta 15N
## then would want colonization of these samples....












  CN_clean %>%
    filter(days_post_germ == 14, 
           water == 0.75,
           BRHO == "focal") %>% ## somehow there seem to be ACAM focals in here?? 
    ## I thought we only did BRHO...??
    mutate(ACAM = as.numeric(ACAM)) %>%
    ggplot(aes(x=as.factor(ACAM), y=CN)) +
    geom_boxplot() +
    geom_point()

CN_data_J56 <- CN_data_clean %>%
  filter(str_detect(id, "-56")) %>%
  mutate(id = gsub("-.*","",id)) %>%
  mutate(id = as.numeric(id)) %>%
  mutate(NAir = as.numeric(NAir))%>%
  mutate(CVPDB = as.numeric(CVPDB))%>%
  mutate(WtN = as.numeric(WtN))%>%
  mutate(WtC = as.numeric(WtC))%>%
  mutate(CN = as.numeric(CN))%>%
  arrange(id)








ggplot(CN_dat_Julia, aes(x=CN)) +
  geom_histogram() +
  facet_wrap(~days_post_germ)

ggplot(CN_dat_Julia, aes(x= ACAM, y=CN)) +
  #geom_histogram() +
  geom_point() +
  facet_wrap(~days_post_germ)



extra_data <- sampling_data_raw %>% 
  select(unique.ID, microbe, water) %>%
  rename_at("unique.ID", ~"id")

joined_data_Other <- CN_data_Other %>% left_join(extra_data, by="id")
joined_data_J56 <- CN_data_J56 %>% left_join(extra_data, by = "id")
joined_data_full <- joined_data_Other %>% full_join(joined_data_J56) %>% arrange(id)

avg_data_Other <- joined_data_Other %>%
  group_by(microbe, water) %>%
  summarize(avg_CN = mean(as.numeric(CN)))

avg_data_J56 <- joined_data_J56 %>%
  group_by(microbe, water) %>%
  summarize(avg_CN = mean(as.numeric(CN)))

avg_data_full <- joined_data_full %>%
  group_by(microbe, water) %>%
  summarize(avg_CN = mean(as.numeric(CN)))

ggplot(joined_data_Other, aes(x=as.factor(microbe), y=CN))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggplot(joined_data_J56, aes(x=as.factor(microbe), y=CN))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggplot(joined_data_full, aes(x=as.factor(microbe), y=CN))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggsave("CNGraph.jpg", width = 6, height = 4)

ggplot(joined_data_full, aes(x=as.factor(microbe), y=NAir))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggsave("deltaNGraph.jpg", width = 6, height = 4)

ggplot(joined_data_full, aes(x=as.factor(microbe), y=CVPDB))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggsave("deltaCGraph.jpg", width = 6, height = 4)

ggplot(joined_data_full, aes(x=as.factor(microbe), y=WtN))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggsave("PercentNGraph.jpg", width = 6, height = 4)

ggplot(joined_data_full, aes(x=as.factor(microbe), y=WtC))+
  geom_jitter()+
  geom_boxplot()+
  facet_wrap(vars(water))

ggsave("PercentCGraph.jpg", width = 6, height = 4)

str(joined_data_full)

ggplot(avg_data_Other, aes(x=as.factor(microbe), y = avg_CN, fill = as.factor(microbe)))+
  geom_bar(stat="identity")

ggplot(avg_data_J56, aes(x=as.factor(microbe), y = avg_CN, fill = as.factor(microbe)))+
  geom_bar(stat="identity")

ggplot(avg_data_full, aes(x=as.factor(microbe), y = avg_CN, fill = as.factor(microbe)))+
  geom_bar(stat="identity")