

## Read in data 
source("data_analysis/RII/additive_intensity_index.R")

source("data_cleaning/clean_CN_dat.R")

## C isotope ratios increase as water availability decreases across spatial gradients


## iWUE = [C_a (b - D13C)] / 1.6 * (b - a)

## C_a = atmospheric CO2 concentration in micromol/mol (eqn 5)
##a = 4.4%; diffusive fractionation coefficient of 13CO2 as it passes thru stomata
##b = 27%; the fractionation coefficient during carboxylation of 13CO2 by Rubisco enzyme
## D13C = (d13Ca - d13Cp) / [(1 + d13Cp) / 1000]

a = 4.4
b = 27

D13C = (d13Ca - d13Cp) / ((1 + d13Cp) / 1000)

WUE = CN_final %>%
  mutate(WUE = )