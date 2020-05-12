test_that("csa works", {
  expect_output(str(csa(rnorm(1000), wn = TRUE, chk = TRUE)), "List of 2")
})
