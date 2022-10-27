rgeom_mi <- function(n, p){
  u <- runif(n)
  x <- floor(log(1-u)/log(1-p))
  return(x)
}