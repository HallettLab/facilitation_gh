



alpha_function4  <- function(Amin, Aslopes,c,x,N0){
 # e = exp(Aslopes*(x-N0)) # c is stretching the graph horizontally 
  #if((N-N0) >10 ){
  #  e = exp(-Aslopes*(10))
  #}
  a = c*(1-exp(Aslopes*(x-N0))) #stretching the graph vertically
  d = Amin
  y = (a/(1 + exp(Aslopes*(x-N0)))) + d
  
  return(y)
}

Amin = 0.35
Aslopes = -0.998
c = -0.355
N0 = 2.3

curve( Amin +  ((c* (1 - exp(Aslopes*(x-N0)))) / (1 + exp(Aslopes*(x-N0))) ), from=1, to=84, n=300, xlab="xvalue", ylab="yvalue", 
        col="blue", lwd=2, main="Sigmoidal Function"  )
