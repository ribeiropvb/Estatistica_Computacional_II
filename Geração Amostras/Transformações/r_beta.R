r_beta <- function(n,r,b){
  library(tidyverse)
  
  # Funcao para gerar exponenciais
  
  r_exp <- function(n, lambda){
    u=runif(n)
    x=-(1/lambda)*log(1-u) # aplicando a funcao inversa 
    return(x)
  }
  
  # Funcao para gerar Gama
  
  r_gamma <- function(n, r, lambda){
    library(tidyverse)
    x <- 1:n %>% 
      map(~r_exp(r,lambda)) %>% 
      map_dbl(sum)
    return(x)
  }
  
  # Gerar gama por X/(X+Y)
  # Tal que o primeiro parametro, da Beta, e' herdado de X
  # e o segundo parametro e' herdade de Y
  
  X <- r_gamma(n,r,1)
  Y <- r_gamma(n,b,1)
  x <- X/(Y+X)
  return(x)
}
