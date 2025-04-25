
## Plot IGR diffs with m0 and m1

#source("data_analysis/compare_soil/calc_IGR.R")
#igr_microbe = rbind(igr_m1, igr_m0)
#write.csv(igr_microbe, "data_analysis/compare_soil/igr_20250424_final_models.csv", row.names = F)

igr_microbe = read.csv("data_analysis/compare_soil/igr_20250424_final_models.csv") 

gr_df = igr_microbe %>%
  mutate(log_lambda = log(lambda)) %>%
  select(focal, water, microbe, igr, log_lambda) %>%
  pivot_longer(cols = c("igr", "log_lambda"), names_to = "type", values_to = "growth_rate") %>%
  mutate(type = ifelse(type == "igr", "Invasion", "Intrinsic"),
         soil = ifelse(microbe == 0, "Sterilized", "Live"),
         water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         water.text = fct_relevel(water.text, "Low", "Intermediate", "High"))

b = gr_df %>%
  filter(focal == "BRHO") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=int, y=growth_rate, color = as.factor(soil))) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  facet_wrap(~water.text) +
  scale_color_manual(values = c("#02401B", "#81A88D")) +
  labs(color = "Soil") +
  ylab("Grass Growth Rate") +
  xlab(NULL) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

a = gr_df %>%
  filter(focal == "ACAM") %>%
  mutate(int = interaction(soil, type)) %>%
  ggplot(aes(x=int, y=growth_rate, color = as.factor(soil))) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot() +
  facet_wrap(~water.text) +
  scale_color_manual(values = c("#400227", "#d2a2a7")) +
  labs(color = "Soil") +
  ylab("Legume Growth Rate") +
  xlab(NULL) +
  theme(text = element_text(size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(a, b, ncol = 1, labels = "AUTO", align = 'v')

ggsave("figures/final_diss/Fig3_igr_mutualism.png", width = 8, height = 6)


ggplot(igr_microbe, aes(x=alpha_inter, y=igr, color = as.factor(water))) +
  geom_point() +
  facet_grid(microbe~focal, scales = "free")
