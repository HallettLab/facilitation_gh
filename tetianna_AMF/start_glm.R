## AMF result exploration
setwd("C:/Users/carme/Documents/Repositories/tetianna_AMF")

## load packages
library(tidyverse)
library(lme4)

## read in data
draftAMF <- read.csv("AMF_quantification_20250127.csv")
AMF_key <- read.csv("AMF_sample_key.csv")

# Clean Data ####
## fixing value in root 2 column to be a number
draftAMF[draftAMF$root2 == "1 ALOT", ]
draftAMF[draftAMF$root2 == "1 ALOT", ]$root2 = 1

## change root2 col to integer
draftAMF$root2 = as.integer(draftAMF$root2)

## reformat data to long version
AMF_long = draftAMF %>%
  
  pivot_longer(cols = c("root1", "root2", "root3", "root4", "root5", "root6", "root7", "root8", "root9", "root10"), names_to = "root_num", values_to = "AMF") %>%
  
  filter(!is.na(AMF)) %>%
  
  mutate(root.sp.ID = "B",
         unique.ID = strsplit(Slide_ID, " ") %>%
           sapply(head, 1)) %>%
  select(unique.ID, Data_point, root_num, AMF)

## change unique ID to an integer
AMF_long$unique.ID = as.integer(AMF_long$unique.ID)

## remove ACAM samples from key before joining
AMF_key2 = AMF_key %>%
  select(unique.ID, block, water, microbe, rep, ACAM_density) %>%
  distinct()

## join data and treatment key
AMF_mdat = left_join(AMF_long, AMF_key2, by = c("unique.ID")) %>%
  filter(!is.na(microbe), 
         ACAM_density != 24, 
         microbe == 1)

# Model ####
m_random = glmer(AMF ~ as.factor(water) + as.factor(ACAM_density) + (1|unique.ID), family = "binomial", data = AMF_mdat)

summary(m_random)

m_nested = glmer(AMF ~ water + ACAM_density + (1|unique.ID/root_num), family = "binomial", data = AMF_results)

summary(m1)




#%>% 
  
  ## calculate % colonization
  group_by(Slide_ID) %>%
  
  summarize(percent_colonization = (sum(AMF)/n())*100) %>%
  
  mutate(root.sp.ID = "B",
         unique.ID = strsplit(Slide_ID, " ") %>%
           sapply(head, 1)) %>%
  select(-Slide_ID)



glm()



