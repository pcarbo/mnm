# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
n <- 1000
b <- 
V <- 

# SET UP ENVIRONMENT
# ------------------
library(mvtnorm)
source(file.path("..","code","datasim.R"))

# SIMULATE DATA
# -------------
cat("Generating data set.\n")
out <- sim.mvr.simple(n,b,V)
x   <- out$x
Y   <- out$Y

# COMPUTE STUFF
# -------------
# TO DO.
