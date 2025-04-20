
ggplot(acam_stat_posteriors, aes(x=lambda)) +
  geom_density()

micA = acam_stat_posteriors %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)

write.csv(micA, "data/model_posteriors/acam_soil_comp_posts_20250419.csv")
write.csv(micB, "data/model_posteriors/brho_soil_comp_posts_20250419.csv")

statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")



micB = brho_stat_posteriors %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)


ggplot(aposts) +
  geom_density(aes(x=lambda, color = "red")) +
  geom_density(aes(x=lambda_m0)) +
  facet_wrap(~water)


ggplot(aposts) +
  geom_density(aes(x=alpha_brho, color = "red")) +
  geom_density(aes(x=alpha_brho_m0)) +
  facet_wrap(~water)

ggplot(aposts) +
  geom_density(aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x=alpha_acam_m0)) +
  facet_wrap(~water)



ggplot(micB) +
  geom_density(aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x=alpha_acam_m0)) +
  facet_wrap(~water)

ggplot(micB) +
  geom_density(aes(x=lambda, color = "red")) +
  geom_density(aes(x=lambda_m0)) +
  facet_wrap(~water)


ggplot(micB) +
  geom_density(aes(x=alpha_brho, color = "red")) +
  geom_density(aes(x=alpha_brho_m0)) +
  facet_wrap(~water)

ggplot(micB) +
  geom_density(aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x=alpha_acam_m0)) +
  facet_wrap(~water)

