## Header ##
## 
## Structural Equation Model
##
## Purpose: Run the structural equation model and test the fit of the model.
##
## Author: Carmen Watkins

# Set Up ####
## load packages 
library(piecewiseSEM)
library(lmerTest)
library(lme4)
library(GGally)
library(multcompView)
library(tidyverse)
library(semEffect)
library(tidygraph)
library(ggraph)
library(car)

#install.packages("ggraph")

## load AII & treatment data
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")

## load nutrient data
source("1_data_cleaning/clean_CN_dat.R")
rm(CN_clean) ## this contains multiple timepoints; use only final time point in analyses  

## prep data ####
## join intrxn & nutrient data together
test = left_join(brho_AII, CN_final[,1:7], by = c("unique.ID")) %>%
  filter(!is.na(delta13C)) %>%
  mutate(micr = ifelse(microbe == "Sterilized", 0, 1),
         rain = ifelse(water == "Low", 0.6,
                       ifelse(water == "High", 1, 0.75)))

## get rid of these data for the moment... 
## can't think of a way of including them yet

## will be using only brho AII for this as we do not have leaf nutrient data for ACAM

## check for correlation, VIF, and multicolinearity
## with all data
ggpairs(test,
        columns = c(10,17, 21, 22),
        upper = list(continuous = "cor"))
ggsave("../outputs/figures/corrmatrixall.png", height = 6, width = 8)

## then with cows model
ggpairs(test,
        columns = c(10,17, 21, 22),
        upper = list(continuous = "cor"),
        mapping = aes(color = water), 
        title = "Water")
#ggsave("corrmatrixcows.png", height = 6, width = 8)

hist(test$WtN)
hist(test$delta13C)
hist(test$NIntA)
hist(test$num.bg.indiv)

## possible model structure
    ## this is concerning as delta13C and WtN are not present in all rows; how does this affect the way the model runs...? may need to specify some way of having the model not remove data
    ## will probably just have to run model with subset of data
    ## currently starting with an additive model and seeing if water level affects the multigroup analysis
    ## should think if there is scientific reason for interactions between factors and justify before running model either way...

sem1 = psem(
  
  lmer(NIntA ~ rain + num.bg.indiv + micr + delta13C + WtN + (1|block), 
       data = test),
  
  lmer(WtN ~ rain + num.bg.indiv + micr + (1|block), data = test),
  
  lmer(delta13C ~ rain + num.bg.indiv + micr + (1|block), data = test),
  
  lmer(num.bg.indiv ~ rain + (1|block), data = test),
  
  data = test)

sem1

summary(sem1)
plot(sem1)
## Global goodness of fit:
## Fisher's C = 5.561 with P-value = 0.234 and on 4 degrees of freedom

effects_plot <- sem_effects(
  object = sem1, 
  target = "NIntA",       # The downstream variable of interest
  plot = TRUE,         # Tells the function to build the ggplot
  total_only = FALSE   # Shows separate bars for direct, indirect, and total effects
)

# 4. Modify or display the plot 
# Since it returns a ggplot2 object, you can add native themes or labels
effects_plot

## save table
sem_coefs <- coefs(sem1)

write.csv(sem_coefs, "../outputs/sem_coefficient_table.csv", row.names = F)

edges <- data.frame(
  from = sem_coefs$Predictor,
  to = sem_coefs$Response,
  weight = sem_coefs$Std.Estimate,
  p_val = sem_coefs$P.Value,
)

nodes <- data.frame(name = unique(c(edges$from, edges$to)))

graph_data <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
ggraph(graph_data, layout = "linear", circular = FALSE) + 
  geom_edge_arc(aes(edge_width = abs(weight), color = weight), 
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(5, 'mm'), 
                end_cap = circle(5, 'mm')) +
  geom_node_point(size = 8, color = "skyblue") +
  geom_node_text(aes(label = name), vjust = -1, fontface = "bold") +
  scale_edge_color_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) +
  scale_edge_width(range = c(0.5, 2.5)) +
  theme_void() +
  labs(title = "Piecewise SEM Path Diagram via ggplot/ggraph",
       edge_color = "Standardized Estimate",
       edge_width = "Effect Magnitude")


vif(lmer(NIntA ~ rain + micr + num.bg.indiv + delta13C + WtN + (1|block), 
         data = test))
vif(lmer(WtN ~ rain + micr + num.bg.indiv + (1|block), data = test))
vif(lmer(delta13C ~ rain + micr + num.bg.indiv + (1|block), data = test))
vif(lm(num.bg.indiv ~ rain, data = test))

summary(lm(num.bg.indiv ~ rain, data = test))





# OLD ####
test_multi <- multigroup(sem1, group = "rain")


t1 = lmer(NIntA ~ water + microbe + num.bg.indiv + delta13C + WtN + (1|block), data = test)
summary(t1)


### Multi-group analysis
## Goal: to identify whether a single global model is sufficient to describe the data, or whether some or all paths vary by some grouping variable

## unclear if this can be done with a categorical variable with 3 levels...?


## Cows: with richness -> synchrony pathway
cows_psem_w <- psem(
  
  lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(cows_psem_w)

vif(lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10))
