### Historical projects for Browns Valley ###
library(tidyverse)
library(lubridate)

theme_set(theme_classic())

## Pull in the prism data and clean
rain = read_csv("data/PRISM_brownsvalley_long.csv", skip = 10) %>%
  mutate(ppt = `ppt (inches)`*2.54*10) %>% ## change to mm
  separate(Date, c("year", "month")) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>%
  mutate(year = ifelse(month == 12 | month == 11 | month == 10 | month == 9, year + 1, year)) #%>%
  #mutate(season = "Early",
       #  season = ifelse(month == 2 | month == 3 | month == 4, "Late", season)) %>%
 # filter(month != 5, month != 6, month!= 7, month != 8)

## Summarize by year 
## Using 50% as the cutoff 
rainsummary = rain %>%
  group_by(year) %>%
  summarize(ppt = sum(ppt)) %>%
  mutate(raintype = "int",
         raintype = ifelse(ppt < quantile(ppt, 0.3), "low", raintype),
         raintype = ifelse(ppt > quantile(ppt, 0.6), "high", raintype)) 

## check rainfall patterns for site description
last50 = rainsummary %>%
  filter(year %in% c(1967:2016))

## Visualize the rainfall scenarios
#pdf("Rainfal history.pdf", width = 10, height = 8)

ggplot(rainsummary, aes(x=year, y=ppt)) +
  geom_line(linewidth = 1)+
  #geom_point(aes(color = raintype), size = 3) + 
  #theme_bw() + 
  labs(x="Year", y="Annual rainfall (mm)") +
  theme(text = element_text(size = 16))

ggsave("figures/dissertation_talk/hist_rainfall.png", width = 12, height = 4.5)
  
  #scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a"))

#ggsave("data_analysis/MCT/figures/historic_rainfall.png", width = 10, height = 3)










