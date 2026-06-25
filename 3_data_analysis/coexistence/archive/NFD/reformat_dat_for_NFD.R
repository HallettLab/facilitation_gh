
sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")

## reformat data for N/F diffs
sig_means = sigposts %>%
  group_by(water, focal) %>%
  summarise(lam = mean(lambda),
            a_intra = mean(alpha_intra),
            a_init = mean(alpha_initial), 
            a_slope = mean(alpha_slope),
            c = mean(c),
            N_opt = mean(N_opt))


statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv") 

acam = statposts %>%
  filter(focal == "ACAM")

names(acam) = c("disp", "lambda", "alpha_inter", "alpha_intra", "water", "focal", "post_num")


brho = statposts %>%
  filter(focal == "BRHO")
names(brho) = c("disp", "lambda", "alpha_intra", "alpha_inter", "water", "focal", "post_num")


stat_means = rbind(acam, brho) %>%
  group_by(water, focal) %>%
  summarise(lam = mean(lambda),
            a_intra = mean(alpha_intra),
            a_inter = mean(alpha_inter))

## reformat data for N/F diffs
stat_means = statposts %>%
  group_by(water, focal) %>%
  summarise(lam = mean(lambda),
            a_intra = mean(alpha_intra),
            a_inter = mean(alpha_initial))
            #a_slope = mean(alpha_slope),
           # c = mean(c),
           # N_opt = mean(N_opt))

names(statposts)


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





