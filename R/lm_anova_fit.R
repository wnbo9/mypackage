#'Linear Regression Anova Fit
#'
#'Get the sum of squares due to regression (SSR), of a given matrix
#'
#'@param x input matrix, consisting of Y and X
#'
#'@return the SSR of the given matrix
#'
#'@examples
#'ax <- as.matrix(mt_cars[,1:3])
#'lm_anova_fit(ax)
#'
#'@export
#'
lm_anova_fit <- function(x){
  Y <- x[,1]
  X <- x[,-1]
  nrow <- length(Y)

  H <- X %*% solve(t(X) %*% X) %*% t(X)
  AR <- H - matrix(1, nrow, nrow)/nrow
  SSR <- t(Y) %*% AR %*% Y

  return(SSR)
}
