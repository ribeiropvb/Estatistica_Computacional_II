rcateg = function(m=1, p=rep(0.5,2)) {
  
  fda_categ = cumsum(p) # Calculo da acumulada F
  
  u = runif(m)          # Sorteio de m valores da uniforme (0,1)
  
  #
  # Aplicacao da inversa generalizada de F
  # para obter x ~ categorica(p)
  #
  
  # Funcao nativa do R que localiza em qual intervalo do vetor
  # fda_categ cada valor do vetor u se encontra
  x = findInterval(
    u, fda_categ
  )
  return(x)
} 

rcateg(100,c(0.4,0.3,0.1,0.2))
