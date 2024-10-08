library(tidyverse)

theme_set(theme_classic())

fc <- read.csv("flowering_check_updated.csv")

fc2 <- fc %>%
  mutate(flowering = ifelse(BRHO.flowering == "yes", BRHO.flowering, BRHO.flowering.followup))

ggplot(fc2, aes(x=flowering)) +
  geom_bar()




