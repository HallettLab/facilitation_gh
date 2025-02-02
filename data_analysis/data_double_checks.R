## Data Checks 

# Set up ####

## check number of reps per treatment
table(brho$microbe, brho$water)
## number of pots per treat
## 44 each in sterilized
## 46, 47, 45 in live soil

## check number of reps in each density x water
## live soil
table(brho[brho$microbe == 1,]$water, brho[brho$microbe == 1,]$ACAM)

## sterilized soil
table(brho[brho$microbe == 0,]$water, brho[brho$microbe == 0,]$ACAM)

## check if rep number matches; yes
unique(brho$rep)

table(acam$water, acam$BRHO)
## 5 reps of every water x density of ACAM ; that's good!! 


test = anti_join(brho, abkgrd, by =c("block", "water", "microbe", "rep"))


abkgrd[abkgrd$unique.ID == 292,]
## this was meant to be a 3 planted ACAM, there were no bg indiv at the end

brho.model[brho.model$unique.ID == 292,]
## this worked out in the final data



nacheck = abkgrd %>%
  filter(is.na(num.bg.indiv))

unique(abkgrd$num.dead.bg.indiv) ## this has some values in it; these might need to be added into the background individual totals??

unique(abkgrd$bkgrd)
unique(abkgrd$processing.notes)


## in the models we need to have some way of accounting for the number of individals that may have been thinned ...

brhoRP = brho %>%
  mutate(replant = ifelse(X0 %in% replanted$unique.ID, "Y", "N"))


brhoRP %>%
  mutate(bio.percap = total.biomass.g/num.focal.indiv) %>%
  group_by(replant, water, microbe) %>%
  summarise(meanbio = mean(bio.percap, na.rm = TRUE), 
            sebio = calcSE(bio.percap)) %>%

ggplot(aes(x = water, y=meanbio, fill = replant)) +
  geom_errorbar(aes(ymin = meanbio - sebio, ymax = meanbio + sebio), width = 0.05) +
  geom_point(aes(fill = replant), colour = "black", pch = 21, size = 3) +
  facet_wrap(~microbe) +
  ylab("Mean Per Capita Biomass") +
  xlab("Water Treatment")+
  scale_fill_manual(values = c("white", "black"))
ggsave("figures/MS_draft2/replanted_by_water_microbe.png", width = 6, height = 3)


brhoRP %>%
  mutate(bio.percap = total.biomass.g/num.focal.indiv) %>%
  group_by(replant, water, microbe, ACAM) %>%
  summarise(meanbio = mean(bio.percap, na.rm = TRUE), 
            sebio = calcSE(bio.percap)) %>%

  filter(microbe == 1) %>%
  
ggplot(aes(x = ACAM, y=meanbio, fill = replant)) +
  geom_errorbar(aes(ymin = meanbio - sebio, ymax = meanbio + sebio)) +
  geom_point(aes(fill = replant), colour = "black", pch = 21, size = 3) +
  facet_wrap(~water) +
  scale_fill_manual(values = c("white", "black")) +
  ylab("Mean Per Capita Biomass") +
  xlab("Planted ACAM Density")
ggsave("figures/MS_draft2/replanted_by_dens.png", width = 8, height = 3)

