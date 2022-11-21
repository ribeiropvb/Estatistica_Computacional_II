newton.raphson <- function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}




NR_H <- function(f, x0, par, tol = 1e-08, maxiter = 1000){
  
  dif_ <- 1
  theta <- matrix(NA, nrow=2, ncol=maxiter)
  theta[, 1] <- x0
  cont <- 1
  
  while(abs(dif_)>= tol & cont<=maxiter){
    #cat("cont+1=", cont+1, "\n")
    theta[ ,cont+1] <- theta[ ,cont] - solve(hessian(log_vero, theta[ ,cont]))%*%grad(log_vero, theta[ ,cont])
    #cat("theta[", cont+1, "]=", theta[ ,cont+1], "\n")
    dif_ <- sqrt(sum((theta[ ,cont+1]-theta[ ,cont])^2))
    #cat("dif=", dif_, "\n")
    cont <- cont+1
  }
  x <- theta[, cont]
  names(x) <- par
  r <- list(
    niter = cont,
    res = x
  )
  return(r)
}
