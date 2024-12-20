library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)

nodule_data_raw <- read.csv("nodule_raw_data.csv")

nodule_data_clean <- nodule_data_raw %>%
  filter(!is.na(num.nodules)) %>%
  filter(!unique.ID == 115) %>%
  select(unique.ID, microbe, num.nodules)

nodule_data_alt <- nodule_data_raw %>%
  select(ACAM, num.nodules) %>%
  filter(is.na(num.nodules))

ggplot(nodule_data_clean, aes(x = as.factor(unique.ID), y = num.nodules))+
  geom_bar(stat = "identity") +
  facet_wrap(vars(microbe), scales = "free_x") +
  labs(x = "Unique ID", y = "Nodule Count", title = "Nodule Count Comparison")

ggsave("Nodule_Counts_Graph.jpg", width = 6, height = 4)

