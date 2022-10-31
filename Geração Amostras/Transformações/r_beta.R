r_exp <- function(n, lambda){
  u=runif(n)
  x=-(1/lambda)*log(1-u) # aplicando a funcao inversa 
  return(x)
}

r_gamma <- function(n, r, lambda){
  x <- 1:n %>% 
    map(~r_exp(r,lambda)) %>% 
    map_dbl(sum)
  return(x)
}

r_beta <- function(n,r,b){
  X <- r_gamma(n,r,1)
  Y <- r_gamma(n,b,1)
  x <- X/(Y+X)
  return(x)
}