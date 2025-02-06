## Plot prior distributions with posteriors!


ggplot() +
  geom_density(data = acam_stat_posteriors, aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x = rnorm(10000, 0.185, 0.25)))

ggplot() +
  geom_density(data = acam_stat_posteriors, aes(x=alpha_brho, color = "red")) +
  geom_density(aes(x = rnorm(10000, -0.024, 0.25)))

ggplot() +
  geom_density(data = acam_stat_posteriors, aes(x=lambda, color = "red")) +
  geom_density(aes(x = rnorm(10000, 62, 30)))





