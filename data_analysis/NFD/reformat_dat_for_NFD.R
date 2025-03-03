


## reformat data for N/F diffs

brho_mp = brho_stat_posteriors %>%
  group_by(water) %>%
  summarise(lam = mean(lambda),
            a_bb = mean(alpha_brho),
            a_ba = mean(alpha_acam))



acam_mp = acam_stat_posteriors %>%
  group_by(water) %>%
  summarise(lam = mean(lambda),
            a_aa = mean(alpha_acam),
            a_ab = mean(alpha_brho))




