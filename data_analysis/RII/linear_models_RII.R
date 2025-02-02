
# Set up ####

## load packages
library(lmerTest)
library(performance)
library(multcomp)
library(emmeans)
library(car)

## read in data
source("data_analysis/RII_figures.R")

# Live Soil Model ####
## final dens ####
mfd.re = lmer(RII ~ num.bg.indiv*water + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])

check_model(mfd.re)
## num bg indiv * water and water both have moderate VIF which is not ideal

summary(mfd.re)
Anova(mfd.re, type = 3, test.statistic = "F")
## all 3 are signif

summary(glht(mfd.re, linfct=mcp(water="Tukey")))

## planted dens ####
mpd.re = lmer(RII ~ ACAM*water + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])

check_model(mpd.re)
## ACAM*water has moderate VIF, but water is ok

## use type III when want to account for interactions
summary(mpd.re)
Anova(mpd.re, type = 3, test.statistic = "F")
## all 3 are signif

# Sterilized Soil Model ####
## final dens ####
mfds.re = lmer(RII ~ num.bg.indiv*water + (1|block), data = brho_RII[brho_RII$microbe == "Sterilized Soil",])

check_model(mfds.re)
## num bg indiv * water and water both have moderate VIF which is not ideal

summary(mfds.re)
Anova(mfds.re, type = 3, test.statistic = "F")
## all 3 are signif


## planted dens ####
mpds.re = lmer(RII ~ ACAM*water + (1|block), data = brho_RII[brho_RII$microbe == "Sterilized Soil",])

check_model(mpds.re)
## ACAM*water has moderate VIF, but water is ok

## use type III when want to account for interactions
summary(mpds.re)
Anova(mpds.re, type = 3, test.statistic = "F")
## all 3 are signif

# Within water treatments ####
## Live Soil ####
m1L = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "High",])

summary(m1L)

TukeyHSD(aov(m1L))

m0.75L = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "Intermediate",])

summary(m0.75L)

TukeyHSD(aov(m0.75L))

m0.6L = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "Low",])

summary(m0.6L)

TukeyHSD(aov(m0.6L))


## Sterilized Soil ####
m1 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Sterilized Soil" & brho_RII$water == "High",])

summary(m1)

TukeyHSD(aov(m1))

m0.75 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Sterilized Soil" & brho_RII$water == "Intermediate",])

summary(m0.75)

TukeyHSD(aov(m0.75))

m0.6 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Sterilized Soil" & brho_RII$water == "Low",])

summary(m0.6)

TukeyHSD(aov(m0.6))





unique(brho_RII$microbe)








## check first whether random effect is necessary
## check also whether interaction is needed

m.re = lmer(RII ~ num.bg.indiv*water + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])
check_model(m.re)
## all diagnostics look pretty good, except the ACAM:water interaction has a fairly high VIF (near 10)

summary(m.re)
Anova(m.re, test.statistic = "F", type = 3)

## try some sort of tukey test??
#m.re = lmer(RII ~ as.factor(ACAM)*as.factor(water) + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])


lmerTest::difflsmeans(m.re, pairwise = TRUE, ddf = "Kenward-Roger")

emmeans(m.re)

emmeans(m.re, list(pairwise ~ water*num.bg.indiv), adjust = "tukey")


summary(glht(m.re, linfct=mcp(water="Tukey")))


m.l = lm(RII ~ ACAM*water, data = brho_RII[brho_RII$microbe == "Live Soil",])
check_model(m.l)
## all diagnostics look pretty good, except the ACAM:water interaction has a fairly high VIF (near 10)

anova(m.re, m.l) ## no significant difference

## should consider no interaction model comparison
m.are = lmer(RII ~ ACAM+water + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])
check_model(m.are)
## all diagnostics look pretty good, except the ACAM:water interaction has a fairly high VIF (near 10)

summary(m.are)

m.al = lm(RII ~ ACAM+water, data = brho_RII[brho_RII$microbe == "Live Soil",])
check_model(m.al)
## all diagnostics look pretty good, except the ACAM:water interaction has a fairly high VIF (near 10)

anova(m.are, m.al) ## again, no difference b/w the models

anova(m.re, m.are)
anova(m.al, m.l)

anova(m.re, m.l)


# Sterilized Soil Model ####
m.re2 = lmer(RII ~ ACAM*water + (1|block), data = brho_RII[brho_RII$microbe == "Sterilized Soil",])
summary(m.re2)
Anova(m.re2, test.statistic = "F")

m.l2 = lm(RII ~ ACAM*water, data = brho_RII[brho_RII$microbe == "Sterilized Soil",])

anova(m.re2, m.l2)



# OLD ####
m1 = lmer(RII ~ num.bg.indiv*water + (1|block), data = brho_RII[brho_RII$microbe == "Live Soil",])
check_model(m1)

summary(m1)
anova(m1)

glht(m1, linfct = mcp(factor = "Tukey"))


m2 = lm(RII ~ as.factor(ACAM) + water, data = brho_RII[brho_RII$microbe == "Live Soil",])

m2 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "High",])

summary(m2)
anova(m2) ## no significant effect of density

TukeyHSD(aov(m2))


m3 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "Intermediate",])
summary(m3)
anova(m3)

TukeyHSD(aov(m3))

m4 = lm(RII ~ as.factor(ACAM), data = brho_RII[brho_RII$microbe == "Live Soil" & brho_RII$water == "Low",])
summary(m4)
anova(m4)





check_model(m2)
emmeans(m2, list(pairwise ~ ACAM*water), adjust = "tukey")

TukeyHSD(aov(m2))



anova(m1, m2)


emmeans(m1, list(pairwise ~ ACAM), adjust = "tukey")


m3 = lm(RII ~ as.factor(ACAM)*water, data = brho_RII[brho_RII$microbe == "Sterilized Soil",])
summary(m3)
anova(m3)

