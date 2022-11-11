stat_break_hist <- function(x, type = c('sturges','scott','FD')){

  n <- length(x)

  t <- match.arg(type)
  larg <- switch(
    t
    , sturges = diff(range(x) / ceiling(1 + log2(n)))
    , scott = 3.49*sd(x)*n^(-1/3)
    , FD = 2*IQR(x)*n^(-1/3)
  )
  min_ <- min(x)
  max_ <- max(x)
  nclasses <- ceiling((max_-min_)/larg) #quantidade de classes
  breaks <- min_ + larg * 0:nclasses # limites das classes
  r <- list(
    nClasses = nclasses
    , Largura = larg
    , breaks = breaks
  )
  return(r)
}
