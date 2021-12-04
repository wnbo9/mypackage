lm_fit <- function (formula, data){

  #1. get the formula of the fitted model and the data to fit the model
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"),
             names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  term <- attr(mf, "terms")
  intercept <- attr(term, "intercept")


  #2. fit the model
  ##2.1 extract Y and X matrix
  nrow <- dim(mf)[1]
  ncol <- dim(mf)[2]
  Y <-  mf[,1]
  if (intercept == 1){
    X <- cbind("(Intercept)"=rep(1,nrow), mf[,2:ncol])
  } else {
    X <- mf[,2:ncol]
  }
  ncol <- dim(X)[2]
  X <- as.matrix(X)
  ##2.2 test if we can use the data to fit the model
  ###2.2.1 insufficient data
  if (nrow < ncol) {
    print("Insufficient data to fit coefficients of the model...")
    return(NULL)
  }
  ###2.2.2 invertible matrix t(X)*X
  mat <- try(solve(t(X) %*% X), silent = T)
  if (class(mat)[1] == "try-error"){
    print("Invertible matrix results failure to fit the model...")
    return(NULL)
  }
  ##2.3 obtain coefficients
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  Y_hat <- X %*% beta_hat
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  residuals <- Y - Y_hat

  AY <- diag(1, nrow) - matrix(1, nrow, nrow)/nrow
  AR <- H - matrix(1, nrow, nrow)/nrow
  AE <- diag(1, nrow) - H
  SSY <- t(Y) %*% AY %*% Y
  SSR <- t(Y) %*% AR %*% Y
  SSE <- t(Y) %*% AE %*% Y

  #3. test hypotheses
  sigma_2hat <- SSE / (nrow - ncol)
  var_beta <- as.numeric(sigma_2hat) * solve(t(X) %*% X)
  se_beta <- sqrt(diag(var_beta))
  t_stat <- beta_hat / se_beta
  t_p <- 2 * (1 - pt(abs(t_stat), nrow-ncol))
  par_info <- cbind(beta_hat, se_beta, t_stat, t_p)
  colnames(par_info) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(par_info) <- colnames(X)

  R_2 <- SSR/SSY
  R_2adj <- 1 - (SSE/(nrow-ncol))/(SSY/(nrow-1))

  F_stat <- (SSR/(ncol-1))/(SSE/(nrow-ncol))
  F_p = 1 - pf(F_stat, ncol-1, nrow-ncol)

  z <- list(coefficients = beta_hat,
            residuals = residuals,
            fitted.values = Y_hat,
            MSE = sigma_2hat,
            n = nrow,
            p = ncol,
            par_info = par_info,
            R_square = R_2,
            R_square_adjusted = R_2adj,
            t_statistics = t_stat,
            t_pvalue = t_p,
            F_statistics = F_stat,
            F_pvalue = F_p,
            terms = term,
            model = deparse(match.call()))

  #4. report the result
  ##4.1 prepare
  coeffcient_Table <- beta_hat
  colnames(coeffcient_Table) <- "Estimated"
  ##4.2 output the result
  cat("\nCall:\n")
  cat(deparse(match.call()), "\n")
  cat("\nCoefficients:\n")
  printCoefmat(coeffcient_Table, digits = 6)

  return(z)
}
