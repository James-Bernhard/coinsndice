test_that("one-sample p test works", {
  expect_equal(as.numeric(p_test(x=8, n=10)$p.value), 2*pnorm(8/10, mean=0.5, sd=sqrt(.5*.5/10), lower=FALSE))
})

# More tests to come
