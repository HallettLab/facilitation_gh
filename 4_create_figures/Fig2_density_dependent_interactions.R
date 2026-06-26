
## Figure 2

## read in sig alphas
source("3_data_analysis/create_sigmoidal_alphas_df.R")

## read in additive intensity index
source("2_calculate_interactions/additive_intensity_index/calc_additive_intensity_index.R")

## read in leaf nutrient data
source("3_data_analysis/analyse_leaf_nutrients.R")

## panel a: sigmoidal interaction coefficient
## plot brho
b_plot = alpha_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y=alpha_inter, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  ylab("Raw Interspecific Interaction") +
  xlab("Legume Density") +
  labs(color = "Water Level") +
  ggtitle("B. hordeaceus") +
  theme(text = element_text(size=13))

## plot acam
a_plot = alpha_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y=alpha_inter, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=mean.alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  ylab(" ") +
  xlab("Grass Density") +
  labs(color = "Water Level") +
  ggtitle("A. americanus") +
  theme(text = element_text(size=13))

ggarrange(b_plot, a_plot, common.legend = TRUE, legend = "right")

#ggsave("raw_alpha_sigmoidal_plots.png", width = 10, height = 4.5, bg = "white")


## panel b: additive intensity index
## panels c/d: leaf nutrient 'explanatory variables'

