test_that("lm_anova_fit works", {
  set.seed(2021)
  xx <- matrix(rnorm(12,0,1),4,3)
  xx <- data.frame(xx)
  colnames(xx) <- c("outcome", "x1", "x2")
  bx <- as.matrix(xx[,1:2])

  expect_equal(as.numeric(round(lm_anova_fit(bx), 6)), -0.219496)
})
