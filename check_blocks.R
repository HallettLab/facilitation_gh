## read in data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/set_up/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/set_up/"
  
} else {
  
  lead <- ""
} 

seeding <- read.csv(paste0(lead, "seeding.csv"))
seeding2 <- read.csv(paste0(lead, "seeding2.csv"))
seeding3 <- read.csv(paste0(lead, "seeding3.csv"))

## 10 dens x 3 water x 4 reps x 2 inoc 
10 * 3 * 4 * 2 ## 240
4 * 3 * 5 ## 60

4 * 3 * 2 ## 24

ggplot(seeding, aes(x=block)) +
  geom_histogram(bins = 42) +
  geom_hline(yintercept = 8)

ggplot(seeding2, aes(x=block)) +
  geom_histogram(bins = 42) +
  geom_hline(yintercept = 8)



blocks <- seeding2 %>%
  group_by(block) %>%
  summarise(num_pots = n())

low_blocks <- blocks %>%
  filter(num_pots < 8)




prog <- seeding3 %>%
  group_by(focal.seeded) %>%
  summarise(num.done = n())






