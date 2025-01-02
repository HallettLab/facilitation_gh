
## load packages
library(lmerTest)
library(performance)
library(multcomp)
library(emmeans)

## read in data
source("data_analysis/explore_focal_density_patterns.R")

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

