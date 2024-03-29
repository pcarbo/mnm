# Compute useful quantities in Bayesian multivariate regression, in
# which Y ~ MN(xb',I,V) and b ~ N(0,S0), where "N" denotes the
# multivariate normal distribution and "MN" denotes the matrix normal
# distribution. 
#
# The outputs are: b, the least-squares estimate of the regression
# coefficients; S, the covariance of b; mu1, the posterior mean of the
# regression coefficients given that the coefficients are not all
# zero; S1, the posterior covariance of the regression coefficients
# given that the coefficients are not all zero; and p1, the posterior
# probability that the coefficients are not all zero.
#
# Input argument p0 specifies the prior probability that the
# coefficients are not all zero.
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
  # standard deviation (mu1, S1), the log-Bayes factor (logbf), and the
  # posterior inclusion probability (p1).
  return(list(b = b,S = S,mu1 = mu1,S1 = S1,logbf = logbf,p1 = p1))
}

# This is similar to the "bayes.mvr" function, the main difference
# being that the regression coefficients b are assigned a
# mixture-of-normals prior with mixture weights w0 and covariance
# matrices S0 (this should be a list of the same length as w0).
#
# The outputs are: the log-Bayes factor (logbf), the posterior
# inclusion probability (p1), the posterior assignment probabilities
# (w1), the posterior mean of the coefficients given that all the
# coefficients are not nonzero (mu1), and the posterior covariance of
# the coefficients given that all the coefficients are not zero (S1).
#
# Input argument p0 specifies the prior probability that the
# coefficients are not all zero.
bayes.mvrmix <- function (x, Y, V, w0, S0, p0) {

  # Get the number of variables (n) and the number of mixture
  # components (k).
  n <- ncol(Y)
  k <- length(S0)

  # Compute the Bayes factors and posterior statistics separately for
  # each mixture component.
  out <- vector("list",k)
  for (j in 1:k)
    out[[j]] <- bayes.mvr(x,Y,V,S0[[j]],p0)

  # Compute the posterior assignment probabilities for the latent
  # indicator variable.
  logbf <- sapply(out,function (x) x$logbf)
  w1    <- softmax(logbf + log(w0))

  # Compute the posterior mean (mu1) and covariance (S1) of the
  # regression coefficients.
  A   <- matrix(0,n,n)
  mu1 <- rep(0,n)
  for (j in 1:k) {
    wj  <- w1[j]
    muj <- out[[j]]$mu1
    Sj  <- out[[j]]$S1
    mu1 <- mu1 + wj*muj
    A   <- A   + wj*(Sj + tcrossprod(muj))
  }
  S1 <- A - tcrossprod(mu1)
  
  # Compute the log-Bayes factor as a linear combination of the
  # individual BFs foreach mixture component.
  u     <- max(logbf)
  logbf <- u + log(sum(w0 * exp(logbf - u)))
  
  # Compute the posterior probability that the coefficient is nonzero.
  p1 <- sigmoid(log(p0/(1 - p0)) + logbf)

  # Return the log-Bayes factor (logbf), the posterior inclusion
  # probability (p1), the posterior assignment probabilities (w1), the
  # posterior mean of the coefficients (mu1), and the posterior
  # covariance of the coefficients (S1).
  return(list(logbf = logbf,p1 = p1,w1 = w1,mu1 = mu1,S1 = S1))
}
