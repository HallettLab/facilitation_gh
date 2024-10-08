library(tidyverse)
sb <- read.csv("reshuffled_blocks_41924.csv")

set.seed(242)

## shuffle sterile blocks
sterile.blocks <- sb %>%
  filter(microbe == 0) %>%
  mutate(position4 = sample(position1.3, 17, replace = FALSE))

## shuffle inoc blocks
inoc.blocks <- sb %>%
  filter(microbe == 1) %>%
  mutate(position4 = sample(position1.3, 25, replace = FALSE))

## add back together to create new df
shuffled.blocks <- rbind(sterile.blocks, inoc.blocks)

write.csv(shuffled.blocks, "reshuffled_blocks_20240419.csv")
