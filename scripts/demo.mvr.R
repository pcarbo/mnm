# A very short script demonstrating the use of bayes.mvr and
# bayes.mvrmix to compute posterior statistics for a simple Bayesian
# multivariate regression with a multivariate normal prior and a
# mixture-of-normals prior.

# SCRIPT PARAMETERS
# -----------------
# This script parameters specify the number of samples to simulate
# (n) and the parameters of the model used to simulate the data:
#
#   Y ~ MN(xb',I,V)
#
# where "N" denotes the multivariate normal distribution and "MN"
# denotes the matrix normal distribution.
#
# Script parameter p0 specifies the prior probability that the
# regression coefficient is not zero, and S0 specifies the prior
# covariance, b ~ N(0,S0).
n  <- 100
b  <- c(-0.25,0.21)
V  <- rbind(c(1.0,0.2),
            c(0.2,0.4))
S0 <- rbind(c(1.0,-0.8),
            c(-0.8,1.2))
p0 <- 0.01

# These two script parameters specify the mixture weights and
# covariances for the mixture-of-normals prior.
w0    <- c(0.1,0.1,0.8)
S0mix <- list(S0_1 = rbind(c(1.0,0.0),
                           c(0.0,1.2)),
              S0_2 = rbind(c(1.0,0.5),
                           c(0.5,1.2)),
              S0_3 = rbind(c(1.0,-0.8),
                           c(-0.8,1.2)))

# SET UP ENVIRONMENT
# ------------------
# Load some function definitions.
source("../code/misc.R")
source("../code/datasim.R")
source("../code/bayes.R")

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# SIMULATE DATA
# -------------
# Simulate data from the multivariate regression model, Y ~ MN(xb',I,V).
cat("Generating simulated data set.\n")
x <- rnorm(n)
x <- x - mean(x)
Y <- sim.mvr(x,b,V)

# COMPUTE POSTERIOR STATTISTICS
# ----------------------------
# Also compute least-squares estimates.
cat("Performing Bayesian computations.\n")
fit1 <- bayes.mvr(x,Y,V,S0,p0)
print(fit1)

# Compute posterior statistics for the multivariate Bayesian
# regression with a mixture-of-normals prior.
fit2 <- bayes.mvrmix(x,Y,V,w0,S0mix,p0)
print(fit2)
