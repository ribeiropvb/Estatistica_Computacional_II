
# Uniforme 0, 1. Intervalo 0, 1 ####

theta_MC <- function(n, funct){
  a <- runif(n)
  return(mean(funct(a)))
}

# Uniforme a, b. Intervalo a, b ####

fun <- function(x) return(x^2)
theta_MC2 <- function(n, funct, a, b){
  if (a>b) stop("'b' deve ser maior do que 'a'")
  u <- runif(n,a,b)
  x <- (b-a)*mean(funct(u))
  return(x)
}

# Uniforme 0, 1. Intervalo a, b ####

theta_MC3 <- function(n, funct, a, b){
  #if (a>b) stop("'b' deve ser maior do que 'a'")
  u <- runif(n)
  y <- (b-a)*u+a
  x <- (b-a)*mean(funct(y))
  return(x)
}
