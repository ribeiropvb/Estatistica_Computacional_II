
f_beta <- function(x, a, b){
  x <- (1/beta(a,b))*x^(a-1)*(1-x)^(b-1)
  return(x)
}

rbeta_mi <- function(n, a, b){
  while(cont<n){
    u <- runif(1)
    j <- j+1
    y <- runif(1) # gerando valores da densidade g
    c <- ceiling(f(0.5, a, b))
    if((f(y, a, b)/c)>u) {
      cont <- cont+1 
      x[cont] <- y              
    }
  }
  return(x)
}
