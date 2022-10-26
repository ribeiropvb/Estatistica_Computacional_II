
library(tidyverse)

rm(list=ls())

dbin <- function(x,n,p){
  x <- choose(n,x)*p^x*(1-p)^(n-x)
  return(x)
}

rbinom_mi <- function(m=1, p=rep(0.5,2)) {
  
  fda_categ = cumsum(p) # Calculo da acumulada F
  u = runif(m)          # Sorteio de m valores da uniforme (0,1)

  # Aplicacao da inversa generalizada de F
  # para obter x ~ categorica(p)

    x = findInterval(
    u, fda_categ
  )
  
  return(x)
}

m = 1000                # número de pontos sorteados

p <- dbin(0:3,3,0.5)
# parâmetro da distribuicao
# (vetor de probabilidades das categorias)

x <- rbinom_mi(m,p)

