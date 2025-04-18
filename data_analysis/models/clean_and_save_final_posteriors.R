## create one model dataframe to use going forward

source("data_analysis/models/evaluate/load_models.R")


# Static ####
## prep df's for joining
brho_stat_posteriors2 = brho_stat_posteriors %>%
  mutate(focal = "BRHO") %>%
  group_by(water) %>%
  mutate(post_num = 1:2500)

acam_stat_posteriors2 = acam_stat_posteriors %>%
  mutate(focal = "ACAM") %>%
  group_by(water) %>%
  mutate(post_num = 1:2500)

## join
stat_posts = rbind(brho_stat_posteriors2, acam_stat_posteriors2)

write.csv(stat_posts, "data/model_posteriors/stat_posts_20250401.csv", row.names = F)

# Sigmoidal ####
brho_sig_posteriors2 = brho_sig_posteriors %>%
  mutate(focal = "BRHO",
         alpha_intra = alpha_brho) %>%
  group_by(water) %>%
  mutate(post_num = 1:2500) %>%
  select(-alpha_brho)

acam_sig_posteriors2 = acam_sig_posteriors %>%
  mutate(focal = "ACAM",
         alpha_intra = alpha_acam) %>%
  group_by(water) %>%
  mutate(post_num = 1:2500) %>%
  select(-alpha_acam)

sig_posts = rbind(brho_sig_posteriors2, acam_sig_posteriors2)

write.csv(sig_posts, "data/model_posteriors/sig_posts_20250401.csv", row.names = F)
