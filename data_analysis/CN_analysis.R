library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)

CN_data_raw <- read.csv("leaf_cn_raw_data.csv")
sampling_data_raw <- read.csv("adult_sampling.csv")

names(CN_data_raw) <- c("info", "id", "weight", "NAir", "CVPDB", "WtN", "WtC", "CN")

CN_data_clean <- CN_data_raw %>% 
  filter(info == "") %>%
  filter(id != "" & id != "Sample ID") %>%
  select(-info)

CN_data_Julia <- CN_data_clean %>% filter(str_detect(id, "-"))

CN_data_Other <- CN_data_clean %>% 
  filter(!str_detect(id, "-")) %>%
  mutate(id = as.numeric(id))%>%
  mutate(NAir = as.numeric(NAir))%>%
  mutate(CVPDB = as.numeric(CVPDB))%>%
  mutate(WtN = as.numeric(WtN))%>%
  mutate(WtC = as.numeric(WtC))%>%
  mutate(CN = as.numeric(CN))%>%
  arrange(id)

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