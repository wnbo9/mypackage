test_that("multiplication works", {
  x = c(1:1000)
  y = c(1:1000)
  dat = cbind(y, x)
  dat = data.frame(dat)
  colnames(dat) = c("y", "x")
  expect_warning(lm_fit(y~x, dat))
})
