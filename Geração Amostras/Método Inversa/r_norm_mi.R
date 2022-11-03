r_norm_mi <- function(n){
  u <- runif(n/2)
  v <- runif(n/2)
  
  z1 <- cos(2*pi*v)*sqrt(-2*log(u))
  z2 <- cos(2*pi*u)*sqrt(-2*log(v))
  x <- c(z1,z2)
  return(x)
}