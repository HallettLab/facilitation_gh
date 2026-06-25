
acam = read.csv("../outputs/posteriors/acam_sig_posts_LO_20260107.csv")
brho = read.csv("../outputs/posteriors/brho_sig_posts_LO_20260107.csv")

names(acam)
names(brho)

acam2 = acam %>%
  select(-X) %>%
  mutate(alpha_intra = alpha_acam, 
         focal = "ACAM") %>%
  select(-alpha_acam) %>%
  group_by(water) %>%
  mutate(post_num = 1:4000)

brho2 = brho %>%
  select(-X) %>%
  mutate(alpha_intra = alpha_brho,
         focal = "BRHO") %>%
  select(-alpha_brho) %>%
  group_by(water) %>%
  mutate(post_num = 1:4000)

sigposts = rbind(acam2, brho2)

# Calc 80% HDI ####
sigposts80 = sigposts %>%
  select(-disp) %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         N_opt >= hdi(N_opt, credMass = 0.8)["lower"],
         N_opt <= hdi(N_opt, credMass = 0.8)["upper"],
         c >= hdi(c, credMass = 0.8)["lower"],
         c <= hdi(c, credMass = 0.8)["upper"],
         alpha_slope >= hdi(alpha_slope, credMass = 0.8)["lower"],
         alpha_slope <= hdi(alpha_slope, credMass = 0.8)["upper"],
         alpha_initial >= hdi(alpha_initial, credMass = 0.8)["lower"],
         alpha_initial <= hdi(alpha_initial, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"])









