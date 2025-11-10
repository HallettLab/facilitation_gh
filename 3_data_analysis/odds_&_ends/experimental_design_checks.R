

ed = read.csv("data/experimental_design_20250104.csv")

names(ed)


hist(ed$focal.germ.)
hist(ed$bkgrd.germ.)

unique(ed$bkgrd.germ.)

nacheck = ed %>%
  filter(is.na(bkgrd.germ.) | bkgrd.germ. == "")




germdat = ed %>%
  select(unique.ID, block, water, microbe, rep, bkgrd, ACAM, BRHO)


## replanted individuals
## thinning 


unique(ed$bkgrd.seeded)
unique(ed$focal.seeded)

unique(ed$date)

replanted = ed %>%
  filter(date == "1/27/24") %>%
  select(unique.ID)

## should probably show that replanted and block by block biomass is not crazy different?; or else will need to take this into account






