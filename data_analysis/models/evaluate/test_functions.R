

## lambda, Amin, Aslopes, c, N_acam, N0, N_brho, alpha_brho

lambda = 350
alpha_brho = -0.05


Amin = 0
Aslopes = 0
c = 0
N_acam = c(1:70)
N0 = 5
N_brho = c(5)


fec_function = function(lambda, Amin, Aslopes, c, N_acam, N0, N_brho, alpha_brho) {
  
  e = exp(Aslopes*(N_acam-N0)) # c is stretching the graph horizontally 
  #if((N-N0) >10 ){
  #  e = exp(-Aslopes*(10))
  #}
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha_acam = (a/(1 + e)) + d
  
  fecundity = N_brho*lambda * exp(-(N_brho*alpha_brho) - (N_acam*alpha_acam))
  
  return(fecundity)
  
}


testdat = data.frame(density = c(0:70), fecundity = fec_function(lambda = lambda, Amin = Amin, Aslopes = Aslopes, c = c, N_acam = c(0:70), N0 = N0, N_brho = 5, alpha_brho = alpha_brho), water = "Low")


ggplot(testdat, aes(x=density, y=fecundity, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  coord_cartesian(xlim = c(0,70)) +
  scale_fill_manual(values = c("#de8a5a")) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed")


