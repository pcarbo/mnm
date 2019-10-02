multivariate_regression <- function(bhat, xtx_inv, V, U) {
  S = lapply(1:length(xtx_inv), function(j) xtx_inv[j] * V)
  S_inv = lapply(1:length(S), function(j) solve(S[[j]]))
  post_cov = lapply(1:length(S), function(j) U %*% solve(diag(nrow(U)) +
      S_inv[[j]] %*% U))
  post_b1 = do.call(cbind, lapply(1:length(S),
      function(j) post_cov[[j]] %*% (S_inv[[j]] %*% bhat[j,])))
  lbf = log(sapply(1:length(S),
      function(j) sqrt(det(S[[j]])/det(S[[j]]+U))*exp(0.5*t(bhat[j,]) %*%
         S_inv[[j]]%*%post_cov[[j]]%*%S_inv[[j]]%*%bhat[j,])))
  post_b2 = lapply(1:length(post_cov),
      function(j) tcrossprod(post_b1[,j]) + post_cov[[j]])
  return(list(b1 = t(post_b1), b2 = aperm(abind(post_b2, along = 3),
                                   c(2,1,3)), lbf = lbf,post_cov = post_cov))
}

