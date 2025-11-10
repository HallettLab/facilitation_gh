## AMF result exploration
#setwd("C:/Users/carme/Documents/Repositories/tetianna_AMF")

## load packages
library(tidyverse)

## read in data
draftAMF <- read.csv("data/AMF/AMF_quantification_20250506.csv")
AMF_key <- read.csv("data/AMF/AMF_sample_key.csv")

## data checks
colnames(draftAMF)
names(draftAMF)
str(draftAMF)

unique(draftAMF$root2)
unique(draftAMF$root1)

## fixing value in root 2 column to be a number
draftAMF[draftAMF$root2 == "1 ALOT", ]
draftAMF[draftAMF$root1 == "`1", ]

## base R notation; need to specify df name & column for it to recognize column
draftAMF[draftAMF$root2 == "1 ALOT", ]$root2 = 1
draftAMF[draftAMF$root1 == "`1", ]$root1 = 1

draftAMF[draftAMF$root2 == "1 ALOT", ]$root2

## change root2 col to integer
draftAMF$root2 = as.integer(draftAMF$root2)
draftAMF$root1 = as.integer(draftAMF$root1)

hist(draftAMF$root1)
hist(draftAMF$root10)

## re-format data before calculating % colonization
draftAMF_long = draftAMF %>%
  
  pivot_longer(cols = c("root1", "root2", "root3", "root4", "root5", "root6", "root7", "root8", "root9", "root10"), names_to = "root_num", values_to = "AMF") %>%
  
  filter(!is.na(AMF),
         !(Slide_ID == "236 B" & root_num %in% c("root8", "root9", "root10"))) %>% 
 
   mutate(Slide_ID = ifelse(Slide_ID == "246", "246 B", Slide_ID)) %>%
  
  ## calculate % colonization
  group_by(Slide_ID) %>%
  
  summarize(percent_colonization = (sum(AMF)/n())*100) %>%
  
  mutate(root.sp.ID = "B",
         unique.ID = strsplit(Slide_ID, " ") %>%
           sapply(head, 1)) %>%
  select(-Slide_ID)


draftAMF_long$unique.ID = as.integer(draftAMF_long$unique.ID)

#check = draftAMF_long %>%
 # filter(Slide_ID == "")
  
  #filter(percent_colonization > 100)

#check236 = draftAMF %>%
 # filter(Slide_ID == "236 B")

hist(draftAMF_long$percent_colonization)

## join AMF data with sample key
names(AMF_key)
names(draftAMF_long)

## change unique.ID to Slide_ID
## fix A's & B's

AMF_results = left_join(draftAMF_long, AMF_key, by = c("unique.ID", "root.sp.ID")) %>%
  filter(!is.na(microbe), 
         microbe == 1)

ggplot(AMF_results, aes(x=as.factor(ACAM_density), y=percent_colonization)) +
  geom_boxplot(linewidth = 1) +
  geom_jitter(size = 3) +
  theme_classic() +
  xlab("Acmispon Density") +
  ylab("Percent Colonization") +
  theme(text = element_text(size = 30))

ggsave("tetianna_AMF/figures/ACAM_dens_perc_coln.png", width = 10, height = 6)

## Colonization in Water Levels Fig
AMF_results %>%
  mutate(water.text = ifelse(water == 1, "High", "Low")) %>%
ggplot(aes(x=as.factor(water.text), y=percent_colonization)) +
  geom_boxplot(linewidth = 1) +
  geom_jitter(size = 3) +
  theme_classic() +
  xlab("Water Level") +
  ylab("Percent Colonization") +
  theme(text = element_text(size = 30))

ggsave("tetianna_AMF/figures/waterlevel_perc_coln.png", width = 10, height = 6)


ggplot(AMF_results, aes(x=as.factor(water), group = interaction(water, ACAM_density), y=percent_colonization, color = as.factor(ACAM_density))) +
  geom_boxplot() +
  geom_jitter() +
  #geom_point() +
  theme_classic() +
  xlab("Water Level") +
  ylab("Percent Colonization") +
  facet_wrap(~ACAM_density)


table(AMF_results$water, AMF_results$ACAM_density)


ggplot(AMF_results, aes(x=as.factor(microbe), y=percent_colonization)) +
  geom_boxplot()
