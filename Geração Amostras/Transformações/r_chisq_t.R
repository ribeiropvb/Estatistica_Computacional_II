rchisq_t <- function(n, gl){
  rnorm_mi <- function(n){
    if(n %% 2 == 1){
      n <- n+1
      u <- runif(n/2)
      v <- runif(n/2)
      
      z1 <- cos(2*pi*v)*sqrt(-2*log(u))
      z2 <- cos(2*pi*u)*sqrt(-2*log(v))
      x <- c(z1,z2)
      x <- x[-1]
    }else{
      u <- runif(n/2)
      v <- runif(n/2)
      
      z1 <- cos(2*pi*v)*sqrt(-2*log(u))
      z2 <- cos(2*pi*u)*sqrt(-2*log(v))
      x <- c(z1,z2)
    }
    return(x)
  }
  x <- 1:n %>%
    map(function(x,n) rnorm_mi(n = gl)) %>% 
    map(function(x) x^2) %>% 
    map_dbl(sum)
  if(x %% 2 == 1) x <- x[-1]
  return(x)
}
