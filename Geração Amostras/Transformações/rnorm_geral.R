rnorm_geral <- function(n, mu, sigma2){
  rnorm_mi <- function(n){
    u <- runif(n/2)
    v <- runif(n/2)
    
    z1 <- cos(2*pi*v)*sqrt(-2*log(u))
    z2 <- cos(2*pi*u)*sqrt(-2*log(v))
    x <- c(z1,z2)
    return(x)
  }
  x <- rnorm_mi(n)*sqrt(sigma2) + mu
  return(x)
}