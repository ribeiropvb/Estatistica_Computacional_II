stat_break_hist <- function(x, type = c('sturges','scott','FD')){
  Sturges <- function(x){
    n <- length(x)
    nclasses <- ceiling(1 + log2(n)) #quantidade de classes
    larg <- diff(range(x) / nclasses) #largura das classes
    breaks <- min(x) + larg * 0:nclasses # limites das classes
    r <- list(
      nClasses = nclasses
      , Largura = larg
      , breaks = breaks
    )
    return(r)
  }
  
  Scott <- function(x){
    n <- length(x)
    larg=3.49*sd(x)*n^(-1/3)
    min_=min(x)
    max_=max(x)
    nclasses <- ceiling((max_-min_)/larg) #quantidade de classes
    breaks <- min_ + larg * 0:nclasses # limites das classes
    r <- list(
      nClasses = nclasses
      , Largura = larg
      , breaks = breaks
    )
    return(r)
  }

    FD <- function(x){
    n <- length(x)
    Q1 <- quantile(x, probs = 0.25)
    Q3 <- quantile(x, probs = 0.75)
    larg <- 2*IQR(x)*n^(-1/3)
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
  
  t <- match.arg(type)
  x <- switch(
    t
    , sturges = Sturges(x)
    , scott = Scott(x)
    , FD = FD(x)
  )
  return(x)
}
