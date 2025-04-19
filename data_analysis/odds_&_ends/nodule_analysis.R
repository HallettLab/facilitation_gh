
# Set up ####
library(tidyverse)

calcSE<-function(x){
  x <- x[is.na(x)==F]
  sd(x)/sqrt(length(x))
}

nodule_data_raw <- read.csv("data/nodule_raw_data.csv")

## prep data
nodule_data_clean <- nodule_data_raw %>%
  filter(!is.na(num.nodules)) %>%
  filter(!unique.ID == 115) %>%
  select(unique.ID, microbe, num.nodules)

## plot data
ggplot(nodule_data_clean, aes(x=as.factor(microbe), y=num.nodules)) +
  geom_boxplot() +
  geom_jitter(pch = 21, size = 2) +
  xlab("Microbes Present") +
  ylab("Nodule Count")

ggsave("figures/Apr2025/Supp/nodule_check.png", width = 4, height = 3)

