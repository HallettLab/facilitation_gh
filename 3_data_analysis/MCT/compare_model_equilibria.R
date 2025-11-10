##plot equilibrium together
names(equil)
names(equil_sig)

equil2 = equil %>%
  mutate(model = "static")

equil_sig2 = equil_sig %>%
  mutate(model = "sigmoidal")

equil_both = rbind(equil2, equil_sig2)

equil_both %>%
  ggplot(aes(x=n_star, color = model)) +
  geom_density(linewidth = 1) +
  facet_grid(water~species) +
  xlab("Equilibrium value") +
  ylab("Density")

## ggsave("data_analysis/MCT/figures/equil_vals_model_comp.png", width = 6, height = 5)
