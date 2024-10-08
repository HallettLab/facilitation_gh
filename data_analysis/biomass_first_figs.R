
library(tidyverse)


## read in data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/data/"
  
} else {
  # 
  lead <- ""
} 

biodat = read.csv(paste0(lead, "biomass/Adult/BRHO_focal_individual_processing.csv")) %>%
  mutate(bio.per.cap = total.biomass.g/num.focal.indiv)

hist(biodat$inflor.g)
hist(biodat$total.biomass.g)

ggplot(biodat, aes(x=as.factor(microbe), y=total.biomass.g))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~water)

ggplot(biodat, aes(x=ACAM, y=bio.per.cap))+
  geom_point()+
  facet_grid(microbe~water) +
  geom_smooth(method = "lm")

ggplot(biodat, aes(x=as.factor(ACAM), y=bio.per.cap))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(microbe~water) #+
  #geom_smooth(method = "lm")






