
library(ggpubr)

## parameter values
sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")
statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

acam = statposts %>%
  filter(focal == "ACAM") %>%
  mutate(alpha_intra = alpha_acam,
         alpha_inter = alpha_brho) %>%
  select(-alpha_acam, -alpha_brho)

brho = statposts %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_intra = alpha_brho,
         alpha_inter = alpha_acam) %>%
  select(-alpha_acam, -alpha_brho)

stat2 = rbind(acam, brho)

#798234,#a3ad62,#d0d3a2,#fdfbe4,#f0c6c3,#df91a3,#d46780

a = ggplot(sigposts, aes(x=alpha_intra, color = focal)) +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  coord_cartesian(xlim = c(-0.085, -0.015)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab(" ") +
  labs(color = "Species")


b = ggplot(stat2, aes(x=alpha_intra, color = focal)) +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  coord_cartesian(xlim = c(-0.085, -0.015)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab("Intraspecific Alpha") +
  labs(color = "Species")

ggarrange(a, b, ncol = 1, common.legend = TRUE, labels = "AUTO", legend = "bottom")

ggsave("figures/Apr2025/Supp/sig_v_stat_a_intra.png", width = 7, height = 4)

a = ggplot(sigposts, aes(x=lambda, color = focal)) +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  coord_cartesian(xlim = c(0, 600)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab(" ") +
  labs(color = "Species")


b = ggplot(stat2, aes(x=lambda, color = focal)) +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  coord_cartesian(xlim = c(0, 600)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab("Lambda") +
  labs(color = "Species")

ggarrange(a, b, ncol = 1, common.legend = TRUE, labels = "AUTO", legend = "bottom")

ggsave("figures/Apr2025/Supp/sig_v_stat_lambda.png", width = 7, height = 4)


ggplot(stat2, aes(x=alpha_intra, y=alpha_inter, color = focal)) +
 # geom_density(linewidth = 1) +
  geom_point()+
  facet_wrap(~water) +
  coord_cartesian(xlim = c(-0.09, 0.03), ylim = c(-0.09, 0.03)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab("INTRAspecific Alpha") +
  ylab("INTERspecific Alpha") +
  labs(color = "Species") +
  geom_abline(slope = 1)


b = ggplot(stat2, aes(x=lambda, color = focal)) +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  coord_cartesian(xlim = c(0, 600)) +
  scale_color_manual(values = c("#df91a3", "#798234")) +
  xlab("Lambda") +
  labs(color = "Species")
