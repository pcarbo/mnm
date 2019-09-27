# TO DO: Explain here what this function does, and how to use it.
sim.mvr.simple <- function (n, b, V) {

  # Get the number of conditions.
  m <- length(b)
    
  # Simulate the data samples, X, from the standard normal.
  x <- rnorm(n)

  # Simulate the responses, Y.
  Y <- matrix(0,n,m)
  for (i in 1:n)
    Y[i,] <- rmvnorm(1,b*x[i],V,method = "chol")

  # Output the simulated data (x) and responses (Y).
  return(list(x = x,Y = Y))
}
