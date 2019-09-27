# Draw a n x m matrix Y from the matrix normal distribution
# MN(x*b',I,V). Input arguments x and b should be vectors of length n
# and m, respectively, and input V should be an m x m covariance
# matrix.
sim.mvr.simple <- function (x, b, V) {

  # Get the number of samples (n) and conditions (m).
  n <- length(x)
  m <- length(b)
    
  # Simulate the responses, Y.
  Y <- matrix(0,n,m)
  for (i in 1:n)
    Y[i,] <- rmvnorm(1,b*x[i],V,method = "chol")

  # Output the simulated responses.
  return(Y)
}
