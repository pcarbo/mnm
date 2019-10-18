# TO DO: Summarize contents of this file.

# Return the dot product of vectors x and y.
dot <- function (x,y)
  sum(x*y)

# Return the sigmoid of the elements of x. The sigmoid function is
# also known as the logistic link function. It is the inverse of
# logit(x).
sigmoid <- function (x) {
  if (x > -500)
    y <- 1/(1 + exp(-x))
  else
    y <- 0
  return(y)
}

# Compute the softmax of vector x in a more numerically prudent manner
# that avoids overflow or underflow.
softmax <- function (x) {
  y <- exp(x - max(x))
  return(y/sum(y))
}
