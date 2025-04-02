


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

write.csv(brho_mp, "brho_mp.csv")


write.csv(acam_mp, "acam_mp.csv")


brho_sig_mp = brho_sig_posteriors %>%
  group_by(water) %>%
  summarise(lam = mean(lambda),
            a_bb = mean(alpha_brho),
            a_init = mean(alpha_initial), 
            a_slope = mean(alpha_slope),
            c = mean(c),
            N_opt = mean(N_opt))

acam_sig_mp = acam_sig_posteriors %>%
  group_by(water) %>%
  summarise(lam = mean(lambda),
            a_aa = mean(alpha_acam),
         #   a_ab = mean(alpha_brho),
            a_init = mean(alpha_initial), 
            a_slope = mean(alpha_slope),
            c = mean(c),
            N_opt = mean(N_opt))

write.csv(brho_sig_mp, "brho_sig_mp.csv")


write.csv(acam_sig_mp, "acam_sig_mp.csv")





