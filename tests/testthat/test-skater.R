library(skater)
library(testthat)

test_that("Degree tibble", {
  expect_equal(3:9 %>% purrr::map(dibble) %>% purrr::map_int(nrow), (3:9)+2)
  expect_error(dibble(12))
  expect_warning(dibble(10))
  expect_error(dibble(0))
  expect_warning(dibble(2))
})

test_that("Degree inference from kinship coefficient", {
  expect_equal(kin2degree(.25), 1L)
  expect_equal(kin2degree(.125), 2L)
  expect_equal(kin2degree(.0625), 3L)
  expect_identical(kin2degree(0), NA_integer_)
  expect_identical(kin2degree(c(.5, .25, .125, .0625, .03125, 0, .25, .5)), c(0L, 1L, 2L, 3L, NA, NA, 1L, 0L))
})

test_that("Expect errors on on plot_pedigree()", {
  famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
  fam <- read_fam(famfile)
  peds <- fam2ped(fam)
  expect_error(plot_pedigree(peds))
  expect_error(plot_pedigree(peds, file=paste0(tempfile(), ".pdf")))
  expect_error(plot_pedigree(peds$ped[[1]], file=paste0(tempfile(), ".pdf")))
})
