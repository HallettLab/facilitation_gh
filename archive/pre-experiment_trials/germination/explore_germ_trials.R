

germ <- read.csv("germination_trials.csv")
library(tidyverse)
library(lubridate)

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

tot.germ <- rowSums(germ[, c(6:14)], na.rm = T)
species <- germ$species
treat <- germ$treatment
rep <- germ$rep
moldy <- germ$num.moldy
actual.num <- germ$actual.num.seeds

totgerm <- data.frame(species = species, treatment = treat, rep = rep, tot.germ = tot.germ, actual.seeds = actual.num, num.moldy = moldy) %>%
  mutate(germ.prop = tot.germ/actual.seeds) %>%
  mutate(sterilize = ifelse(treatment %in% c("sterilize", "sterilize-scarify", "sterilize-scarify-soak", "sterilize-soak"), "sterilized", "not sterilized")) 

germ2 <- germ %>%
  select(1:14) %>%
  pivot_longer(cols = 6:14, names_to = "date", values_to = "num.germ") %>%
  mutate(date2 = substr(date, start = 2, stop = 11),
         date3 = mdy(date2)) %>%
  group_by(species, treatment, date3) %>%
  summarise(mean.germ = mean(num.germ, na.rm = TRUE), se.germ = calcSE(num.germ)) %>% 
  filter(!is.na(se.germ)) %>%
  mutate(sterilize = ifelse(treatment %in% c("sterilize", "sterilize-scarify", "sterilize-scarify-soak", "sterilize-soak"), "sterilized", "not sterilized")) 

ggplot(germ2[germ2$species == "ACAM",], aes(x=date3, y=mean.germ, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  theme_classic() +
  geom_errorbar(aes(ymin = mean.germ - se.germ, ymax = mean.germ + se.germ), width = 0.2) +
  facet_wrap(~sterilize) +
  ylab("# Seeds Germinated") +
  xlab("")

ggsave("ACAM_germination_timeseries.png", width = 8, height = 3)

ggplot(germ2[germ2$species == "BRHO",], aes(x=date3, y=mean.germ, color = treatment)) +
  geom_line() +
  geom_point(size = 3) +
  theme_classic() +
  geom_errorbar(aes(ymin = mean.germ - se.germ, ymax = mean.germ + se.germ), width = 0.2) +
  ylab("# Seeds Germinated") +
  xlab("") +
  ggtitle("BRHO Germination")
  
ggsave("BRHO_germination_timeseries.png", width = 5, height = 3)

ggplot(totgerm[totgerm$species == "ACAM",], aes(x=treatment, y=germ.prop)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Germination Proportion") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("ACAM germination trials") +
  geom_jitter()

ggsave("ACAM_germination_totals.png", width = 6, height = 4)

ggplot(totgerm[totgerm$species == "BRHO",], aes(x=treatment, y=germ.prop)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Germination Proportion") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("BRHO germination trials") +
  geom_jitter() +
  coord_cartesian(ylim = c(0,1))

ggsave("BRHO_germination_totals.png", width = 4, height = 3)


