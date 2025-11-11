## Data Checks 

## Purpose: explore quirks of data, generate figures for supplement, etc. 

## List of Data Checks
## check number of reps per treatment
## check NA values in data
## check replanted vs. original planting date
## check self-thinning


# Set up ####
## read in cleaned model data
source("data_cleaning/clean_model_dat.R")

## read in raw data
file_loc = "/Users/carme/University of Oregon Dropbox/Carmen Watkins/Facilitation_GH/data/biomass/Adult/"

## BRHO
brho = read.csv(paste0(file_loc, "BRHO_focal_individual_processing_20240903.csv"))
bbkgrd = read.csv(paste0(file_loc, "BRHO_bkgrd_sample_processing_20241117.csv"))

## ACAM
acam = read.csv(paste0(file_loc, "ACAM_focal_individual_processing_20240903.csv"))
abkgrd = read.csv(paste0(file_loc, "ACAM_bkgrd_sample_processing_20240903.csv"))

ed = read.csv("data/experimental_design_20250104.csv")



# Check Rep Num ####

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

## check for match between brho focals and acam backgrounds
test = anti_join(brho, abkgrd, by =c("block", "water", "microbe", "rep"))

abkgrd[abkgrd$unique.ID == 292,]
## this was meant to be a 3 planted ACAM, there were no bg indiv at the end

brho.model[brho.model$unique.ID == 292,]
## this worked out in the final data

# Check NA values ####
## brho ####
brho[is.na(brho$num.focal.indiv),] ## ID 79 needs num focal indiv
## this sample had 1 focal ; the other was veg and not collected.
## this may not adequately account for the number of background individuals in the model

## should I do per-capita seed production of focals and include density only in the interaction term??

brho[is.na(brho$total.biomass.g),] ## ID 172 was too green; no sample taken for this; this should be removed



nacheck = abkgrd %>%
  filter(is.na(num.bg.indiv))

unique(abkgrd$num.dead.bg.indiv) ## this has some values in it; these might need to be added into the background individual totals??

unique(abkgrd$bkgrd)
unique(abkgrd$processing.notes)

## acam ####



# Check notes ####
## brho focals ####
unique(brho$notes)

brho.notes = brho %>%
  filter(notes != "")

## need to go through these in detail to ensure that all the correct individuals are added to the correct places

unique(brho$processing.notes)
unique(brho$partial.completed)

brho.partials = brho %>%
  filter(partial.completed != "X", 
         !sample.collected %in% c("X", "x"))
## okay, 38 samples need to be checked for number of focals
## these indicate there was a partial collection started; not clear if these were ones where the focal never matured. 
## these will all need to have background 'brho' individuals included in their models that somehow don't affect the seed output

write.csv(brho.partials, "data/double_checks/brho_focal_partial_checks.csv", row.names = F)

## RESPROUTED BRHO INDIVIDUALS ARE NOT TAKEN INTO ACCOUNT YET!!!

## acam backgrounds ####
unique(abkgrd$notes)
## these notes should all be the same as in the brho.notes since they were initially included together in the sampling datasheet

unique(abkgrd$processing.notes)

unique(abkgrd$partial.completed)

## acam focals ####
unique(acam$notes)
unique(acam$num.resprouted.BRHO.focals)
unique(acam$processing.notes)
unique(acam$issues)
unique(acam$partial.completed)
unique(acam$total.biomass.g)

# Replanted pots ####
replanted = ed %>%
  filter(date == "1/27/24") %>%
  select(unique.ID)

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

# Self-thinning ####
## in the models we need to have some way of accounting for the number of individals that may have been thinned ...

binter$ACAM = as.character(binter$ACAM)

bfocal_ed = left_join(binter, ed, by = c("unique.ID", "block", "water", "microbe", "rep", "ACAM"))


thinning = bfocal_ed %>%
  select(unique.ID, num.focal.indiv, num.bg.indiv, ACAM, ind.proj.pot, date, focal.germ., bkgrd.germ., notes.1, thinning, thinning_r3) %>%
  filter(ACAM != 0) %>%
  mutate(focal.indiv.adjusted = ifelse(thinning %in% c("thinned to 2 focals 2/11", "thinned to 2 focals 2/8", "thinned to 2 focals 2/14", "thinned to 2 indiv 2/9 or 2/11?", "thinned focals to 2 2/8", "thinned to 2 focals 2/9", "thinned to 2 focals 2/9 or 2/11?", "thinned to 2 focals 2/16", "thinned to 2 focals 2/16; save all for CW"), 2, NA),
         replant = ifelse(date == "1/27/24", "Y", "N"), 
         
         focal.indiv.adjusted = ifelse(replant == "Y" & thinning_r3 %in% c("thinned to 2 focals late Feb; ok now", "ok", "thinned to 2 focals 3/1", "thinned to 2 indiv 3/1", "thinned to 2 indiv 2/28", "thinned to 2 indiv late Feb; ok now"), 2, focal.indiv.adjusted))

ggplot(thinning, aes(x=num.focal.indiv, focal.indiv.adjusted)) +
  geom_point() +
  facet_wrap(~replant)
## okay, of this first batch thinned to 2 focals before 2/16, none of these were replanted; good to know!! 

replant = thinning %>%
  filter(replant == "Y")

## replants should be okay if thinned by 3/1; approx 1 month post-planting

na_thinning = thinning %>%
  filter(is.na(focal.indiv.adjusted))



