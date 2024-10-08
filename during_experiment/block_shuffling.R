## randomize block placement

## set up env
library(tidyverse)
set.seed(38)

## create df of current blocks/position nums  
    ## use initial block placement as the position number
lo <- data.frame(block = c(1:42), position1 = c(1:42))

## create a vector of sterile blocks
sterile <- c(1:4, 9:12, 19:22, 27:30, 42)

## add microbial treat data in
lo.mic <- lo %>%
  mutate(microbe = ifelse(block %in% sterile, 0, 1))
      
## shuffle sterile blocks
sterile.blocks <- lo.mic %>%
  filter(microbe == 0) %>%
  mutate(position2 = sample(position1, 17, replace = FALSE))

## shuffle inoc blocks
inoc.blocks <- lo.mic %>%
  filter(microbe == 1) %>%
  mutate(position2 = sample(position1, 25, replace = FALSE))

## add back together to create new df
shuffled.blocks <- rbind(sterile.blocks, inoc.blocks)

write.csv(shuffled.blocks, "shuffled_blocks.csv")

