# Compute kinship coefficient from IBD segments

This function is used to retrieve a relatedness measure from IBD
segments. The relatedness value returned is the kinship coefficient.

## Usage

``` r
ibd2kin(.ibd_data, .map, type = NULL)
```

## Arguments

- .ibd_data:

  Tibble with IBD segments created using the
  [read_ibd](https://signaturescience.github.io/skater/reference/read_ibd.md)
  function

- .map:

  Tibble with the genetic map data created using the
  [read_map](https://signaturescience.github.io/skater/reference/read_map.md)
  function

- type:

  Type of IBD to use for kinship coefficient calculation; must be
  `'IBD1'`, `'IBD2'`, or `NULL` (both IBD1 and IBD2 will be treated the
  same); default is `NULL`

## Value

Tibble with three columns:

1.  id1 (sample identifier 1)

2.  id2 (sample identifier 2)

3.  kinship (kinship coefficent derived from shared segments)

## Details

The input data should be pairwise IBD segments prepared via
[read_ibd](https://signaturescience.github.io/skater/reference/read_ibd.md).
The function will internally loop over each chromosome, and use a
specified genetic map to convert shared segments to genetic units. After
doing so, the function converts the shared length to a kinship
coefficient by summing \\0.5\*IBD2 + 0.25\*IBD1\\.

Note that the data read in by
[read_ibd](https://signaturescience.github.io/skater/reference/read_ibd.md)
when `source="pedsim"` returns a list with separate tibbles for IBD1 and
IBD2 segments. The current implementation of this function requires
running this function independently on IBD1 and IBD2 segments, then
summarizing (adding) the corresponding proportions. See examples.

## References

http://faculty.washington.edu/sguy/ibd_relatedness.html

## Examples

``` r
pedsim_fp <- system.file("extdata", "GBR.sim.seg.gz", package="skater", mustWork=TRUE)
pedsim_seg <- read_ibd(pedsim_fp, source = "pedsim")
gmapfile <- system.file("extdata", "sexspec-avg-min.plink.map", package="skater", mustWork=TRUE)
gmap <- read_map(gmapfile)
ibd1_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD1, .map=gmap, type="IBD1")
ibd2_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD2, .map=gmap, type="IBD2")
dplyr::bind_rows(ibd1_dat,ibd2_dat) %>%
  dplyr::group_by(id1,id2) %>%
  dplyr::summarise(kinship = sum(kinship), .groups = "drop")
#> # A tibble: 48 × 3
#>    id1               id2               kinship
#>    <chr>             <chr>               <dbl>
#>  1 testped1_g1-b1-i1 testped1_g2-b1-i1   0.245
#>  2 testped1_g1-b1-i1 testped1_g2-b2-i1   0.245
#>  3 testped1_g1-b1-i1 testped1_g3-b1-i1   0.136
#>  4 testped1_g1-b1-i1 testped1_g3-b2-i1   0.124
#>  5 testped1_g1-b1-s1 testped1_g2-b1-i1   0.245
#>  6 testped1_g1-b1-s1 testped1_g2-b2-i1   0.245
#>  7 testped1_g1-b1-s1 testped1_g3-b1-i1   0.109
#>  8 testped1_g1-b1-s1 testped1_g3-b2-i1   0.121
#>  9 testped1_g2-b1-i1 testped1_g2-b2-i1   0.254
#> 10 testped1_g2-b1-i1 testped1_g3-b1-i1   0.245
#> # ℹ 38 more rows
```
