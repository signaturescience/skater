# Read IBIS coef output file

Reads in an `ibis` [results file](https://github.com/williamslab/ibis).
Input `file` must have six columns, whitespace delimited:

1.  id1 (member 1)

2.  id2 (member 2)

3.  Kinship Coefficient

4.  IBD2 (ratio of IBD2/All SNPS)

5.  Segment count

6.  Kinship Degree

## Usage

``` r
read_ibis(file)
```

## Arguments

- file:

  Input file path

## Value

A tibble containing the 6 columns from the ibis file.

## Examples

``` r
ibisFile <- system.file("extdata", "3gens.ibis.coef", package="skater", mustWork=TRUE)
ibis <- read_ibis(ibisFile)
ibis
#> # A tibble: 378 × 6
#>    id1               id2                     k  ibd2 segment_count degree
#>    <chr>             <chr>               <dbl> <dbl>         <int>  <int>
#>  1 testped1_g1-b1-i1 testped1_g1-b1-s1 0.00138     0             0     -1
#>  2 testped1_g1-b1-s1 testped1_g2-b1-s1 0.00138     0             0     -1
#>  3 testped1_g1-b1-s1 testped1_g2-b1-i1 0.251       0             1      1
#>  4 testped1_g1-b1-s1 testped1_g2-b1-i2 0.251       0             1      1
#>  5 testped1_g1-b1-s1 testped1_g2-b2-s1 0.00138     0             0     -1
#>  6 testped1_g1-b1-s1 testped1_g2-b2-i1 0.251       0             1      1
#>  7 testped1_g1-b1-s1 testped1_g2-b2-i2 0.251       0             1      1
#>  8 testped1_g1-b1-s1 testped1_g3-b1-i1 0.251       0             1      1
#>  9 testped1_g1-b1-s1 testped1_g3-b1-i2 0.251       0             1      1
#> 10 testped1_g1-b1-s1 testped1_g3-b1-i3 0.0689      0             1      3
#> # ℹ 368 more rows
```
