
library(tidyverse)
theme_set(theme_classic())

## create BRHO focal in ACAM 
test3 <- expand.grid(
  ACAM = c(0, 3, 6, 12, 18, 24, 36, 48, 60, 84), 
  water = c(0.5, 0.75, 1), 
  rep = 1:5, ## replicated across 5 pots
  microbe = c(0,1),
  BRHO = 3
) %>%
  mutate(bkgrd = "ACAM", 
         focal = "BRHO")

## create AMME focal in BRHO
test4 <- expand.grid(
  BRHO = c(0, 12, 24, 48),
  water = c(0.5, 0.75, 1), 
  microbe = 1,
  rep = 1:5, 
  ACAM = 3
) %>%
  mutate(bkgrd = "BRHO",
         focal = "ACAM")

## join together
test5 <- rbind(test3, test4)

## visualize density gradients
ggplot(test5, aes(ACAM, BRHO)) +
  geom_point() + 
  xlab("ACAM Dens") +
  ylab("BRHO Dens") +
  ggtitle("Gradient design, reps x 5, pots = 360")
#save
#ggsave("gradient.design.png", width = 5, height = 3)

## create unique ID column
test5 <- test5 %>%
  mutate(unique.ID = 1:360)

## create 45 blocks of 8 pots
set.seed(4)
blocks <- rep.int(1:45, 8)

## shuffle randomly
shuffled <- sample(blocks, size = 360, replace = FALSE)

## join with pot data, now have randomly assigned pots to blocks
test5 <- test5 %>%
  mutate(block = shuffled) %>%
  select(unique.ID, block, water, microbe, rep, bkgrd, focal, ACAM, BRHO) ## change col order

colnames(test5)

## save file
write.csv(test5, "experimental_design.csv")



#?sample


#OLD
#test <- expand.grid(
 # dens = c(0, 3, 6, 12, 75), 
#  water = c("lo", "med", "hi"), 
 # rep = 1:8, ## replicated across 8 pots
  #microbe = c(0,1), 
#  seeds = NA,
 # brho = 3
#)

#test1 <- expand.grid(
#  dens = 3, 
 # water = c("lo", "med", "hi"), 
  #rep = 1:8, ## replicated across 8 pots
#  microbe = 1, 
 # seeds = NA,
  #brho = c(3, 12, 36)
#)

#test1a <- rbind(test, test1)

#ggplot(test1a, aes(dens, brho)) +
 # geom_point() + 
 # coord_cartesian(ylim= c(0,4)) +
  #xlab("ACAM Dens") +
  #ylab("BRHO Dens") +
  #ggtitle("Current Design, reps x 8")

#ggsave("current.design.png", width = 5, height = 3)

test2 <- expand.grid(
  dens = c(0, 3, 6, 12, 36, 48, 75), 
  water = c("lo", "med", "hi"), 
  rep = 1:4, ## replicated across 4 plots
  microbe = c(0,1), ## sample mult pumpkins from each plant
  seeds = NA,
  brho = c(0, 12, 36)
)

ggplot(test2, aes(dens, brho)) +
  geom_point() + 
  #coord_cartesian(ylim= c(0,4)) +
  xlab("ACAM Dens") +
  ylab("BRHO Dens") +
  ggtitle("Response surface design (?), reps x 4")

ggsave("response.surface.png", width = 5, height = 3)


