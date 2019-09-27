# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
# TO DO: Explain what these parameters are for.
n  <- 100
b  <- c(-1,2)
V  <- rbind(c(1.0,0.2),
            c(0.2,0.4))
S0 <- rbind(c(1.0,0.8),
            c(0.8,1.2))
    
# SET UP ENVIRONMENT
# ------------------
# TO DO: Explain what these lines of code do.
source(file.path("..","code","misc.R"))
source(file.path("..","code","datasim.R"))
source(file.path("..","code","bayes.R"))

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# SIMULATE DATA
# -------------
# TO DO: Explain what these lines of code do.
cat("Generating simulated data set.\n")
x <- rnorm(n)
x <- x - mean(x)
Y <- sim.mvr(x,b,V)

# COMPUTE STUFF
# -------------
cat("Performing Bayesian computations.\n")
# TO DO.

# TESTING
library(abind)
source("../code/temp.R")
bhat <- drop(t(Y) %*% x)/sum(x^2)
out1 <- multivariate_regression(t(bhat),1/sum(x^2),V,S0)
out2 <- bayes.mvr(x,Y,V,S0)
