library(skater)
library(testthat)

test_that("Degree inference from kinship coefficient", {
  expect_equal(kin2degree(.25), 1L)
  expect_equal(kin2degree(.125), 2L)
  expect_equal(kin2degree(.0625), 3L)
  expect_identical(kin2degree(0), NA_integer_)
  expect_identical(kin2degree(c(.5, .25, .125, .0625, .03125)), c(0L, 1L, 2L, 3L, NA))
})
