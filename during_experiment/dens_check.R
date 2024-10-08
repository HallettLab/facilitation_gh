library(tidyverse)

## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/experimental_design/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Facilitation_GH/experimental_design/"
  
} else {
  
  lead <- ""
} 

dens <- read.csv(paste0(lead, "experimental_design_dens1.csv"))

theme_set(theme_classic())

ACAMb <- dens %>%
  filter(bkgrd == "ACAM", !is.na(microbe),
         !is.na(ACAM))

ACAMb$ACAM <- as.factor(ACAMb$ACAM)
ACAMb$water <- as.factor(ACAMb$water)
ACAMb$bkgrd.germ. <- as.integer(ACAMb$bkgrd.germ.)

ACAMb <- ACAMb %>%
  mutate(ACAM = fct_relevel(ACAM, "0", "3", "6", "12", "18", "24", "36", "48", "60", "84")) %>%
  filter(!is.na(microbe), !is.na(water), !is.na(ACAM))


ggplot(ACAMb[ACAMb$bkgrd.germ. < 300,], aes(x=bkgrd.germ., y=ACAM, color = water)) +
  geom_point(size = 1.5) +
  facet_wrap(~microbe*water) +
  scale_color_manual(values = c("#68abb8","#f3e79b", "#8b3058" )) +
  ylab("Planted Density") +
  xlab("Germinated Bkgrd Indiv")

ggsave("dens_trt_check.png", width = 8, height = 6)


#ACAMb[ACAMb$bkgrd.germ. > 300,]
#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674
#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c
#f3e79b,#fac484,#f8a07e,#eb7f86,#ce6693,#a059a0,#5c53a5
#ffc6c4,#f4a3a8,#e38191,#cc607d,#ad466c,#8b3058,#672044



