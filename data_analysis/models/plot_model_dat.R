
load("data_cleaning/clean_model_dat.R")

## Explore modeling data

# BRHO ####
table(brho.model$water)
## relatively even # of samples in each water treatment - that's good

## water 0.6
w60 = brho.model %>%
  filter(water == 0.6)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w60, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w60, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w60, aes(x=seeds.out.percap)) +
  geom_density() 

## this looks a lot better than distrib of total seeds
ggplot(w60, aes(x=seeds.out)) +
  geom_density() 


## water 0.75
w75 = brho.model %>%
  filter(water == 0.75)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w75, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w75, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w75, aes(x=seeds.out.percap)) +
  geom_density()

## this looks a lot better than distrib of total seeds
ggplot(w75, aes(x=seeds.out)) +
  geom_density() 

## water 1
w100 = brho.model %>%
  filter(water == 1)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w100, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w100, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w100, aes(x=seeds.out.percap)) +
  geom_density()

## the distrib of total seeds out looks similar for this water treatment only
ggplot(w100, aes(x=seeds.out)) +
  geom_density() 

## since the other two have such a stark difference, it's probably smart to model as percap seeds


# ACAM ####
## water 0.6 ####
w60a = acam.model %>%
  filter(water == 0.6)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w60a, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w60a, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w60a, aes(x=seeds.out.percap)) +
  geom_density() 

## this looks a lot better than distrib of total seeds
ggplot(w60a, aes(x=seeds.out)) +
  geom_density() 

## water 0.75 ####
w75a = acam.model %>%
  filter(water == 0.75)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w75a, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 
ggplot(w75a, aes(x=num.bg.indiv, y=seeds.out.percap)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w75a, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w75a, aes(x=seeds.out.percap)) +
  geom_density() 

## this looks a lot better than distrib of total seeds
ggplot(w75a, aes(x=seeds.out)) +
  geom_density() 

## water 1 ####
w100a = acam.model %>%
  filter(water == 1)

## total seeds by bg density (doesn't capture INTRAsp)
ggplot(w100a, aes(x=num.bg.indiv, y=seeds.out)) +
  geom_point() 
ggplot(w100a, aes(x=num.bg.indiv, y=seeds.out.percap)) +
  geom_point() 

## distrib of num bg indiv
ggplot(w100a, aes(x=num.bg.indiv)) +
  geom_density() 

## distrib of percap seeds
ggplot(w100a, aes(x=seeds.out.percap)) +
  geom_density() 

## this looks a lot better than distrib of total seeds
ggplot(w100a, aes(x=seeds.out)) +
  geom_density()
