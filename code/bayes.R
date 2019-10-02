# Compute useful quantities in Bayesian multivariate regression, in
# which Y ~ MN(xb',I,V) and b ~ N(0,S0), where "N" denotes the
# multivariate normal distribution and "MN" denotes the matrix normal
# distribution. 
#
# The outputs are: b, the least-squares estimate of the regression
# coefficients; S, the covariance of b; mu1, the posterior mean of the
# regression coefficients; S1, the posterior covariance of the
# regression coefficients; and p1, the posterior probability that the
# coefficient is not zero.
#
# Input argument p0 specifies the prior probability that the
# coefficient is not zero.
bayes.mvr <- function (x, Y, V, S0, p0) {

  # Compute the least-squares estimate and its covariance.
  b <- drop(x %*% Y)/sum(x^2)
  S <- V/sum(x^2)

  # Compute the log-Bayes factor.
  logbf <- (as.numeric(determinant(S)$modulus) +
            - as.numeric(determinant(S0 + S)$modulus) 
            + dot(b,solve(S,b)) - dot(b,solve(S0 + S,b)))/2

  # Compute the posterior probability that the coefficient is nonzero.
  p1 <- sigmoid(log(p0/(1 - p0)) + logbf)
      
  # Compute the posterior mean and covariance assuming a multivariate
  # normal prior with zero mean and covariance S0.
  n   <- ncol(Y)
  I   <- diag(n)
  S1  <- solve(solve(S0) + solve(S))
  mu1 <- solve(S %*% solve(S0) + I,b)
  
  # Return the least-squares estimate (b, S), the posterior mean and
  # standard deviation (mu1, S1) and the log-Bayes factor (logbf).
  return(list(b = b,S = S,mu1 = mu1,S1 = S1,logbf = logbf,p1 = p1))
}
