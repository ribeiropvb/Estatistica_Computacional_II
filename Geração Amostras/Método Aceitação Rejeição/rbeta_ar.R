
f_beta <- function(x, a, b){
  x <- (1/beta(a,b))*x^(a-1)*(1-x)^(b-1)
  return(x)
}

rbeta_mi <- function(n, a, b){
  x <- j <- cont <- 0
  while(cont<n){
    u <- runif(1)
    j <- j+1
    y <- runif(1) # gerando valores da densidade g
    c <- max(f_beta(seq(0,1,by=0.01), a, b))
    if((f_beta(y, a, b)/c)>u) {
      cont <- cont+1 
      x[cont] <- y              
    }
  }
  return(x)
}
