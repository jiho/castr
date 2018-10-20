context("Name masking")

x <- 1:5
ref_slide <- slide(x, fun=mean, k=1)
ref_integrate <- integrate(x, depth=x, fun=mean, from=1, to=5)

test_that("slide avoids name masking", {
  mean <- "bar"
  expect_equal(slide(x, mean, k=1), ref_slide)
  expect_equal(slide(x, base::mean, k=1), ref_slide)
})

test_that("integrate avoids name masking", {
  expect_equal(integrate(x, depth=x, fun=mean, from=1, to=5), ref_integrate)
  expect_equal(integrate(x, depth=x, fun=base::mean, from=1, to=5), ref_integrate)
})
