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

# Format Data ####
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

## fix 66, 99 mistake ####
CN_all[CN_all$unique.ID == 99,]
CN_all[CN_all$unique.ID == 99,]$unique.ID = 66
## believe sample 66 was IDed as 99 at the processing lab
## see below in explore section for more details 

## join CN data with experimental info
CN_clean = left_join(CN_all, sample_key, by = c("unique.ID"))

# Explore ####
CN_clean %>%
  ggplot(aes(x=ACAM, y=CN)) +
  geom_boxplot() +
  geom_jitter()
## there are focal ACAM present - need to confirm with Julia if she sampled ACAM leaves

## number of reps per treatment
CN_clean %>%
  filter(days_post_germ == 56) %>%
  group_by(ACAM, water, microbe) %>%
  summarise(reps = n()) %>%
  ggplot(aes(x=ACAM, y=reps)) +
  geom_bar(stat = "identity") +
  facet_grid(water~microbe)

## 66 v 99 issue explanation ## 
  ## error fixed above before joining with sample key
  ##there is a density 60 sample in microbe 0, water 0.6 treatment, which isn't right, 
  ## these should only have 0, 12, 24, 48
  ##this is unique ID 99. why is it included?

#microbe = CN_clean %>%
 # filter(microbe == 0, water == 0.6)

  ## ah, seems like 99 should maybe be 66
  ## all m0, w0.6 samples should end in 1, 4, 6, and 8; there is no 66 but there should be
  ##there is a 99 and there shouldn't be.
  ## I think the labels had JUST the number on them so they would not get confused, so it 
  ## could make sense for someone to misinterpret the 66 as 99
  ## will change in the data 

## 24 dens, w1, m1 has only 4 samples while it should have 5?
# dens24 = CN_clean %>%
  # filter(water == 1, microbe == 1)

  ## sample 236 is missing... why?
  ## ah, found note in df, there were no more indiv in this pot at the time of sampling.

w75 = CN_clean %>%
  filter(water == 0.75, ACAM != "focal", days_post_germ == 56)


w75 %>%
  group_by(ACAM) %>%
  summarise(reps = n()) %>%
ggplot(aes(x=ACAM, y=reps)) +
  geom_bar(stat = "identity")
## the uneven sampling may be due to not sampling certain pots as there were not enough focals
## wouldn't worry too much about this.

## List of contaminated samples; current as of 4/13/2025
rm.contaminated = c(15, 25, 84, 85, 114, 103)

# Final DF ####
CN_final = CN_clean %>%
  filter(days_post_germ == 56,
         ACAM != "focal",
         !unique.ID %in% rm.contaminated)
  
## clean up env
rm(CN_all, CN_dat, CN_dat_Julia, CN_dat_Other, CN_data_raw, w75, exp_design, sample_key)
