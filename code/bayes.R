# TO DO: Summarize contents of this file.

# TO DO: Explain here what this function does, and how to use it.
bayes.mvr <- function (x, Y, V, S0) {

  # Compute the least-squares estimate and its covariance.
  b <- drop(x %*% Y)/sum(x^2)
  S <- V/sum(x^2)

  # Compute the log-Bayes factor.
  logbf <- (as.numeric(determinant(S)$modulus) +
            - as.numeric(determinant(S0 + S)$modulus) 
            + dot(b,solve(S,b)) - dot(b,solve(S0 + S,b)))/2

  # Compute the posterior mean and covariance assuming a multivariate
  # normal prior with zero mean and covariance S0.
  # S1  <- solve(solve(S0) + solve(S))
  # mu1 <- 
  
  # Return the least-squares estimate (b, S), the posterior mean and
  # standard deviation (mu1, S1) and the log-Bayes factor (logbf).
  return(list(b = b,S = S,logbf = logbf))
}
