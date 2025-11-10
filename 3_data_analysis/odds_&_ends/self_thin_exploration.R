
## SELF THINNING
library(tidyverse)

jdat = read.csv("data/seedling_data/Julia_sampling_20250413.csv")

colnames(jdat)

##cleaning 3 day data 
day3 = jdat %>%
  select(Unique.ID,Block,Background,Focal,X3_Day_Sampled,X._BRHO_including.focal,X._ACAM,Focal_Biomass,NOTES) %>%
  mutate(Focal_Biomass_g = Focal_Biomass, 
         density_ACAM = X._ACAM,
         density_BRHO = X._BRHO_including.focal,
         sampled = ifelse(X3_Day_Sampled == "X", "yes", "no"), 
         days_post_germination = 3) %>%
  select(Unique.ID,Block,Background,Focal,days_post_germination,sampled,density_ACAM,density_BRHO,Focal_Biomass_g)


ggplot(day3, aes(x=Focal_Biomass_g)) + 
  geom_histogram() +
  facet_wrap(~Focal, scales = "free")


## cleaning 2 week (14 day) sampling 
day14 = jdat %>%
  select(Unique.ID,Block,Background,Focal,X2_week_Sampled,X._BRHO_including.focal.1,X._ACAM.1,Focal_Biomass.1,NOTES) %>%
  mutate(Focal_Biomass_g = Focal_Biomass.1, 
         density_ACAM = X._ACAM.1,
         density_BRHO = X._BRHO_including.focal.1,
         sampled = ifelse(X2_week_Sampled == "X"| X2_week_Sampled == "x", "yes", "no"), 
         days_post_germination = 14) %>%
  select(Unique.ID,Block,Background,Focal,days_post_germination,sampled,density_ACAM,density_BRHO,Focal_Biomass_g)

ggplot(day14, aes(x=Focal_Biomass_g)) + 
  geom_histogram() +
  facet_wrap(~Focal, scales = "free")

##cleaning 1 month data 
day28 <- jdat %>%
  select(Unique.ID,Block,Background,Focal,X1.month.sample,X._BRHO_including.focal.2,X._ACAM.2,Focal_Biomass.2,NOTES) %>%
  mutate(Focal_Biomass_g = Focal_Biomass.2, 
         density_ACAM = X._ACAM.2,
         sampled = ifelse(X1.month.sample == "X"|X1.month.sample == "x", "yes", "no"),
         days_post_germination = 28,
         density_BRHO = X._BRHO_including.focal.2) %>%
  select(Unique.ID,Block,Background,Focal,days_post_germination,sampled,density_ACAM,density_BRHO,Focal_Biomass_g)


ggplot(day28, aes(x=Focal_Biomass_g)) + 
  geom_histogram() +
  facet_wrap(~Focal, scales = "free")

## 2 month sampling
day56 = jdat %>%
  select(Unique.ID,Block,Background,Focal,X2.month.sample,X._BRHO_including.focal.3,X._ACAM.3,Focal_Biomass.3,NOTES)%>%
  mutate(Focal_Biomass_g = Focal_Biomass.3, 
         density_ACAM = X._ACAM.3,
         sampled = ifelse(X2.month.sample == "X"|X2.month.sample == "x", "yes", "no"),
         days_post_germination = 56,
         density_BRHO = X._BRHO_including.focal.3) %>%
  select(Unique.ID,Block,Background,Focal,days_post_germination,sampled,density_ACAM,density_BRHO,Focal_Biomass_g)

ggplot(day56, aes(x=Focal_Biomass_g)) + 
  geom_histogram() +
  facet_wrap(~Focal, scales = "free")


##combining data from 3, 14, 28, 56 days 
seedling_dat = rbind(day3, day14, day28, day56)

ggplot(seedling_dat, aes(x = Focal_Biomass_g, fill = as.factor(days_post_germination))) +
  geom_histogram() +
  facet_wrap(~Focal*days_post_germination)


sdat_final = seedling_dat %>%
  filter(Focal == "BRHO") %>%
  mutate(unique.ID = Unique.ID,
         block = Block
         ) %>%
  select(-Unique.ID, -Block)


ggplot(sdat_final, aes(x=days_post_germination, y = density_ACAM, group = Unique.ID)) +
  geom_point()+
  geom_line()

## join in final sampling data


final_dens = binter_for_RII_seed_analyses %>%
  filter(water == 0.75, microbe == 1) %>%
  mutate(Background = "ACAM", 
         Focal = "BRHO",
         days_post_germination = "112",
         sampled = "Y",
         density_BRHO = total.focal.indiv,
         density_ACAM = num.bg.indiv, 
         Focal_Biomass_g = total.bio.percap) %>%
  select(unique.ID, block, Background, Focal, days_post_germination, sampled, density_BRHO, Focal_Biomass_g, density_ACAM)
  
alldat = rbind(sdat_final, final_dens) %>%
  mutate(days_post_germination = as.numeric(days_post_germination))

ggplot(alldat, aes(x=days_post_germination, y = density_ACAM, group = unique.ID)) +
  geom_point()+
  geom_line() 

b_trt = binter_for_RII_seed_analyses %>%
  select(unique.ID, water, microbe, rep, ACAM)

dat = left_join(alldat, b_trt, by = c("unique.ID"))


ggplot(dat, aes(x=days_post_germination, y = density_ACAM, group = unique.ID)) +
  geom_point()+
  geom_line() +
  facet_wrap(~ACAM, scales = "free")


final = dat %>%
  filter(days_post_germination == 112) %>%
  select(unique.ID, density_ACAM, Focal_Biomass_g)

names(final) = c("unique.ID", "final_density", "final_biomass_g")

test = dat %>%
  left_join(final, by = c("unique.ID")) %>%
  group_by(unique.ID, ACAM, final_biomass_g) %>%
  summarise(self_thin = max(final_density)/max(density_ACAM, na.rm = T),
            num_thinned = max(density_ACAM, na.rm = T) - max(final_density)) %>%
  mutate(ACAM = as.character(ACAM))
  
ggplot(test, aes(x = num_thinned)) +
  geom_histogram()

ggplot(test, aes(x = self_thin)) +
  geom_histogram()

ggplot(test, aes(x=self_thin, y=final_biomass_g)) +
  geom_point() + 
  facet_wrap(~ACAM) +
  geom_smooth(method = "lm")


nut_st = left_join(test, CN_final, by = c("unique.ID", "ACAM"))

ggplot(nut_st, aes(x=self_thin, y=delta15N)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(nut_st, aes(x=self_thin, y=delta13C)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(nut_st, aes(x=self_thin, y=WtN)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(nut_st, aes(x=self_thin, y=CN)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ACAM)

