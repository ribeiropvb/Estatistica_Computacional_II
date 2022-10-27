rnbinom_mi <- function(n, r, p){
  source('https://raw.githubusercontent.com/ribeiropvb/Estatistica_Computacional_II/main/Gera%C3%A7%C3%A3o%20Amostras/M%C3%A9todo%20Inversa/rgeom_mi.R')
  
  library(dplyr)
  library(purrr)
  
  x <- 1:n %>%
    map(function(x) rgeom_mi(r, p)) %>% 
    map_dbl(sum)
  return(x)
}