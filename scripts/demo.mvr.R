# A very short script demonstrating the use of bayes.mvr to compute
# posterior statistics for a simple Bayesian multivariate regression.

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
b  <- c(-0.25,0.25)
V  <- rbind(c(1.0,0.2),
            c(0.2,0.4))
S0 <- rbind(c(1.0,0.8),
            c(0.8,1.2))
p0 <- 0.01

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
out <- bayes.mvr(x,Y,V,S0,p0)
print(out)
