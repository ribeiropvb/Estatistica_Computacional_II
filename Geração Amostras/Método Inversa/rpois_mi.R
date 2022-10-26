
rpois_mi <- function (lambda) {
  ## step 1
  U <- runif(1)
  ## step 2
  i <- 0
  p <- exp(-lambda)
  F <- p
  ## you need an "infinite" loop
  ## no worry, it will "break" at some time
  repeat {
    ## step 3
    if (U < F) {
      X <- i
      break
    }
    ## step 4
    i <- i + 1
    p <- lambda * p / i  ## I have incremented i, so it is `i` not `i + 1` here
    F <- F + p
    ## back to step 3
  }
  return(X)
}

x <- replicate(250, rpois_mi(4))
