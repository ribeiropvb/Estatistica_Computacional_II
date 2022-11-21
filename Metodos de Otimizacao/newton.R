newton <- function (fun, x0, dfun = NULL, maxiter = 500, tol = 1e-08, ...){
  stopifnot(is.function(fun))
  require(numDeriv)
  if (is.null(dfun)) {
    dfun <- function(x, ...){
      genD(func = f, x = x0)$D[1]
    }
  }
  x <- x0
  fx <- fun(x, ...)
  dfx <- dfun(x, ...)
  niter <- 0
  diff <- tol + 1
  while (diff >= tol && niter <= maxiter) {
    niter <- niter + 1
    if (dfx == 0) {
      warning("Slope is zero: no further improvement possible.")
      break
    }
    diff <- -fx/dfx
    x <- x + diff
    diff <- abs(diff)
    fx <- fun(x, ...)
    dfx <- dfun(x, ...)
  }
  if (niter > maxiter) {
    warning("Maximum number of iterations 'maxiter' was reached.")
  }
  return(list(root = x, f.root = fx, niter = niter, estim.prec = diff))
}