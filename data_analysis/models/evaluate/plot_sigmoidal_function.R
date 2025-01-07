
## create alpha function
alpha_function4  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  #if((N-N0) >10 ){
  #  e = exp(-Aslopes*(10))
  #}
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (a/(1 + e)) + d
  
  return(alpha)
  
}

## high water
postw1df = as.data.frame(post_w1)

Aminw1 = median(postw1df$alpha_initial)
Aslopesw1 = median(postw1df$alpha_slope)
cw1 = median(postw1df$c)
N0w1 = median(postw1df$N_opt)

w1plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw1, Aslopesw1, cw1, N = c(0:80), N0w1), water = "High")


## intermediate water
postw0.75df = as.data.frame(post_w0.75)

Aminw0.75 = median(postw0.75df$alpha_initial)
Aslopesw0.75 = median(postw0.75df$alpha_slope)
cw0.75 = median(postw0.75df$c)
N0w0.75 = median(postw0.75df$N_opt)

w0.75plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw0.75, Aslopesw0.75, cw0.75, N = c(0:80), N0w0.75), water = "Intermediate")

## low water
postw0.6df = as.data.frame(post_w0.6)

Aminw0.6 = median(postw0.6df$alpha_initial)
Aslopesw0.6 = median(postw0.6df$alpha_slope)
cw0.6 = median(postw0.6df$c)
N0w0.6 = median(postw0.6df$N_opt)

w0.6plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw0.6, Aslopesw0.6, cw0.6, N = c(0:80), N0w0.6), water = "Low")


plotdat = rbind(w1plotdat, w0.75plotdat, w0.6plotdat)

ggplot(plotdat, aes(x=density, y=alpha, color = water)) +
  geom_point() +
  coord_cartesian(xlim = c(0,10))







