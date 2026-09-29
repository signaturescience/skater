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

test_that("ibd2kin() looks up the genetic map by chromosome name", {
  hapibd_fp <- system.file("extdata", "GBR.sim.ibd.gz", package="skater", mustWork=TRUE)
  hapibd_seg <- suppressMessages(read_ibd(hapibd_fp, source = "hapibd"))
  gmapfile <- system.file("extdata", "sexspec-avg-min.plink.map", package="skater", mustWork=TRUE)
  gmap <- read_map(gmapfile)

  ## segments on a single chromosome: dropping an unrelated chromosome from the map must not change the result
  seg20 <- dplyr::filter(hapibd_seg, chr == 20)
  expect_equal(ibd2kin(seg20, dplyr::filter(gmap, chr != 1)), ibd2kin(seg20, gmap))

  ## a map restricted to the chromosomes with segments gives the same result as the full map
  seg2122 <- dplyr::filter(hapibd_seg, chr %in% c(21, 22))
  expect_equal(ibd2kin(seg2122, dplyr::filter(gmap, chr %in% c(21, 22))), ibd2kin(seg2122, gmap))

  ## pedsim segments (character chr) agree with the same segments as numeric chr
  pedsim_fp <- system.file("extdata", "GBR.sim.seg.gz", package="skater", mustWork=TRUE)
  pedsim_ibd1 <- read_ibd(pedsim_fp, source = "pedsim")$IBD1
  expect_equal(ibd2kin(dplyr::mutate(pedsim_ibd1, chr = as.numeric(chr)), gmap, type = "IBD1"),
               ibd2kin(pedsim_ibd1, gmap, type = "IBD1"))

  ## a chromosome with segments but no map data is an informative error
  expect_error(ibd2kin(seg2122, dplyr::filter(gmap, chr == 21)), "Chromosome '22'")
})
