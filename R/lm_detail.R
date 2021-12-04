lm_detail <- function(x){

  cat("\nCall:\n")
  cat(x[["model"]], "\n")

  cat("\nResiduals:\n")
  residuals <- round(as.numeric(x[["residuals"]]), 3)
  res_quan <- quantile(residuals)
  res_Table <- matrix(res_quan, nrow = 1)
  rownames(res_Table) <- ""
  colnames(res_Table) <- c("Min","1Q", "Median", "3Q", "Max")
  printCoefmat(res_Table, digits = max(3, getOption("digits") - 3))


  cat("\nCoefficients:\n")
  printCoefmat(x[["par_info"]], digits = max(3, getOption("digits") - 3),  P.values = TRUE)

  n <- x[["n"]]
  p <- x[["p"]]
  res_df <- n - p
  MSE <- x[["MSE"]]
  RSE <- round(sqrt(MSE), 3)
  cat("\nResidual standard error:",  RSE, "on", res_df, "degress of freedom\n")

  R_2 <- round(x[["R_square"]], 4)
  R_2adj <- round(x[["R_square_adjusted"]], 4)
  cat("Multiple R-squared:", paste0(R_2, ","), "Adjusted R-squared:", R_2adj)

  F_stat <- round(x[["F_statistics"]], 2)
  F_p <- format.pval(x[["F_pvalue"]], digits = 4)
  cat("\nF-statistic:", F_stat, "on", p-1, "and", res_df, "DF, p-value:", F_p)
}
