# TO DO: Explain here what this script does, and how to use it.

# SCRIPT PARAMETERS
# -----------------
n <- 1000
b <- c(-1,2)
V <- rbind(c(1.0,0.2),
           c(0.2,0.4))

# SET UP ENVIRONMENT
# ------------------
library(mvtnorm)
source(file.path("..","code","datasim.R"))

# SIMULATE DATA
# -------------
cat("Generating simulated data set.\n")
x <- rnorm(n)
Y <- sim.mvr.simple(x,b,V)

# COMPUTE STUFF
# -------------
# TO DO.
