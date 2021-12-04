lm_anova_fit <- function(x){
  Y <- x[,1]
  X <- x[,-1]
  nrow <- length(Y)

  H <- X %*% solve(t(X) %*% X) %*% t(X)
  AR <- H - matrix(1, nrow, nrow)/nrow
  SSR <- t(Y) %*% AR %*% Y

  return(SSR)
}
