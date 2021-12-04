lm_anova <- function(x){
  cat("Analysis of Variance Table:\n")
  cat("Response:", as.character(x[["terms"]][[2]]), "\n")

  X <- x[["X"]]
  Y <- x[["Y"]]
  n <- x[["n"]]
  p <- x[["p"]]

  anova_Table <- matrix(1, nrow = p, ncol=5)
  colnames(anova_Table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  rownames(anova_Table) <- c(colnames(X)[-1], "Residuals")
  anova_Table[p,1] <- n-p
  anova_Table[p,2] <- x[["MSE"]]*(n-p)

  adata <- cbind(Y, X)
  for (i in 3:(p+1)) {
    ax <- adata[,c(1,2:i)]
    SSR <- lm_anova_fit(ax)
    if (i == 3){
      anova_Table[1,2] <- SSR
    } else {
      anova_Table[i-2,2] <- SSR - anova_Table[i-3,2]
    }
  }

  anova_Table[,3] <- anova_Table[,2] / anova_Table[,1]
  anova_Table[,4] <- anova_Table[,3] / anova_Table[p,3]

  anova_Table[,5] <-  1 - pf(anova_Table[,4], 1, n-p)

  anova_Table[p,4:5] <- NA
  printCoefmat(anova_Table, P.values = TRUE, na.print = "", cs.ind = 1L)
}
