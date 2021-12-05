test_that("lm_detail works", {
  set.seed(2021)
  xx <- matrix(rnorm(12,0,1),4,3)
  xx <- data.frame(xx)
  colnames(xx) <- c("outcome", "x1", "x2")
  m1 <- lm_fit(outcome ~ x1 + x2, xx)
  expect_equal(class(lm_detail(m1)), "numeric")
})
