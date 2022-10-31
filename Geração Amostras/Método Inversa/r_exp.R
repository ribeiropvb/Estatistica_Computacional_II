r_exp <- function(n, lambda){
  u=runif(n)
  x=-(1/lambda)*log(1-u)
  return(x)
}