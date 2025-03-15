# BRHO ####

hist(brho_RII$NIntA)

## model all together
m1 = lm(NIntA ~ num.bg.indiv * water * microbe, data = brho_RII)
summary(m1)

m2 = lm(NIntA ~ num.bg.indiv + water + microbe, data = brho_RII)
summary(m2)

m2 = lmer(NIntA ~ ACAM * water * microbe + (1|block), data = brho_RII)
summary(m2)

AIC(m1) ## this model is preferred by having more negative AIC value
AIC(m2)

check_model(m1)

brho_tab = as.data.frame(Anova(m1, type = 3, test.statistic = "F")) %>%
  mutate(species = "B. hordeaceus") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  rownames_to_column() %>%
  select(species, rowname, `Sum Sq`,  Df, `F value`, `Pr(>F)`) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, ".", " ")))))

write.csv(brho_tab, "data_analysis/RII/tables/brho_ANOVA_tab.csv", row.names = F)

# ACAM ####
m1a = lm(NIntA ~ num.bg.indiv + water, data = acam_RII)
summary(m1a)
