test_that("lm_fit works", {
  dat = mt_cars[,1:3]
  dat = data.frame(dat)

  expect_equal(class(lm_fit(mpg ~ cyl, dat)), "list")
  expect_equal(lm_fit(mpg ~ cyl, dat)$p, 2)
  expect_equal(lm_fit(mpg ~ cyl + disp - 1, dat)$p, 2)
  expect_equal(lm_fit(mpg ~ cyl + disp, dat[1:2,]), -1)
})
