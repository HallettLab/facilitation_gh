
## Leaf Nutrient Models
library(car)

hist(CN_bio$delta13C)
# hist(CN_bio$delta15N)

## could run linear model for effect of microbe treat + dens + water among just w1 and w0.6
## then could do dens x water with m1 only data?

m13c = lm(delta13C ~ num.bg.indiv * as.factor(water), data = CN_bio[CN_bio$microbe == 1,])
summary(m13c)

Anova(m13c, type = 3)

mpercN = lm(WtN ~ num.bg.indiv * as.factor(water), data = CN_bio[CN_bio$microbe == 1,])
summary(mpercN)

Anova(mpercN, type = 3)



