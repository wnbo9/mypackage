lm_detail <- function(x){


  cat("Residuals:\n", sep = "")

  cat("\nCoefficients:\n", sep = "")
  printCoefmat(coeffcient, P.values = TRUE)
}
