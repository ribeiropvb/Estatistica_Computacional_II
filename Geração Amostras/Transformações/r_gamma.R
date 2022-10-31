r_exp <- function(n, lambda){
  u=runif(n)
  x=-(1/lambda)*log(1-u)
  return(x)
}

r_gamma <- function(n, r, lambda){
  library(tidyverse)
  x <- 1:n %>% 
    map(~r_exp(r,lambda)) %>% 
    map_dbl(sum)
  return(x)
}
