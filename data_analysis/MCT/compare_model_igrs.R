

library(wesanderson)
library(ggpattern)

pal = wes_palette("Moonrise3")

sig = igr_sig_dat %>%
  mutate(model = "sigmoidal") %>%
  select(-dens)


stat = igr_stat_dat %>%
  mutate(model = "static")

igr_both = rbind(sig, stat)


igr_both %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         focal_model = paste0(focal, ", ", model)) %>%
  ggplot(aes(x=as.factor(focal_model), y=igr, group = interaction(model, focal), color = as.factor(focal))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(size = 0.6) +
  geom_boxplot(linewidth = 0.5) +
  facet_wrap(~water.text) +
  scale_color_manual(values = c(pal[2], pal[3])) +
  ylab("Invasion Growth Rate") +
  xlab("Model Type") +
  theme(text = element_text(size = 15)) +
  labs(color = "Focal Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("data_analysis/MCT/figures/igr_both_models_boxplot.png", width = 9, height = 4)


igr_both %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         focal_model = paste0(focal, ", ", model),
         focal.grp = ifelse(focal == "ACAM", "Legume", "Grass")) %>%
  filter(model == "sigmoidal") %>%
  ggplot(aes(x=as.factor(focal.grp), y=igr, color = as.factor(focal.grp))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(size = 0.6) +
  geom_boxplot(linewidth = 0.5) +
  facet_wrap(~water.text) +
  scale_color_manual(values = c(pal[3], pal[2])) +
  ylab("Invasion Growth Rate") +
  xlab(NULL) +
  theme(text = element_text(size = 15)) +
  labs(color = "Focal Species")

ggsave("data_analysis/MCT/figures/igr_sig_models_boxplot_focal_color.png", width = 9, height = 4)


igr_both %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low")),
         focal_model = paste0(focal, ", ", model),
         focal.grp = ifelse(focal == "ACAM", "Legume", "Grass")) %>%
  #filter(focal.grp == "Legume") %>%
  filter(water.text == "High") %>%
  ggplot(aes(x=as.factor(focal.grp), y=igr, group = interaction(focal.grp, model), color = as.factor(focal.grp), linetype = as.factor(model))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(size = 0.6) +
  geom_boxplot(linewidth = 0.75) +
  scale_color_manual(values = c(pal[3], pal[2])) +
  ylab("Invasion Growth Rate") +
  xlab(NULL) +
  theme(text = element_text(size = 15)) +
  labs(color = "Focal", linetype = "Model") +
  scale_linetype_manual(values = c("solid", "dashed"))

ggsave("data_analysis/MCT/figures/igr_model_comp_boxplot_focal_color.png", width = 6, height = 4)

