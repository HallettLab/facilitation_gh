
w1dat = dat

w75 = dat

w60 = dat


hist(w1dat$seeds.out, breaks = 30)
hist(w75$seeds.out, breaks = 30)
hist(w60$seeds.out, breaks = 30)
## ah, these have fairly different distributions
## will need to model as negative binomial to account for this likely


hist(w1dat$num.bg.indiv, breaks = 30)
hist(w75$num.bg.indiv, breaks = 30)
hist(w60$num.bg.indiv, breaks = 30)

## rule of thumb, need about 10 data points per parameter that you are estimating

## alpha_brho
## alpha_initial
## alpha_slope
## N_opt
## C
## lambda

## dispersion parameter



