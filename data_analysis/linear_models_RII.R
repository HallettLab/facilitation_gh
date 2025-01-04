
# Set up ####

## load packages
library(lmerTest)
library(performance)
library(multcomp)
library(emmeans)
library(car)

## read in data
source("data_analysis/explore_focal_density_patterns.R")

# Live Soil Model ####
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

