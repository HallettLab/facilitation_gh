hist(brho.model$seeds.out)


m1w1 = brho.model %>%
  filter(microbe == 1, water == 1)

m1w75 = brho.model %>%
  filter(microbe == 1, water == 0.75)

m1w60 = brho.model %>%
  filter(microbe == 1, water == 0.6)

hist(m1w1$seeds.out)


ggplot(m1w1, aes(x=seeds.out)) +
  geom_density()

ggplot(m1w75, aes(x=seeds.out)) +
  geom_density()

ggplot(m1w60, aes(x=seeds.out)) +
  geom_density()










