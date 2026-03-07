# Read AKT kin output file

Reads in an `akt kin` [results
file](https://illumina.github.io/akt/#kin). Input `file` must have seven
columns, whitespace delimited:

1.  id1 (member 1)

2.  id2 (member 2)

3.  IBD0 (ratio of IBD0/All SNPS)

4.  IBD1 (ratio of IBD1/All SNPS)

5.  Kinship Coefficient

6.  NSNPS

## Usage

``` r
read_akt(file)
```

## Arguments

- file:

  Input file path

## Value

A tibble containing the 7 columns from the akt file.

## Examples

``` r
aktFile <- system.file("extdata", "3gens.akt", package="skater", mustWork=TRUE)
akt <- read_akt(aktFile)
akt
#> # A tibble: 780 × 7
#>    id1               id2                  ibd0   ibd1  ibd2        k nsnps
#>    <chr>             <chr>               <dbl>  <dbl> <dbl>    <dbl> <int>
#>  1 testped1_g1-b1-i1 testped1_g1-b1-s1 1       0      0     -0.0522  83992
#>  2 testped1_g1-b1-s1 testped1_g2-b1-s1 1       0      0     -0.0573  83979
#>  3 testped1_g1-b1-s1 testped1_g2-b1-i1 0.00202 0.895  0.103  0.267   83997
#>  4 testped1_g1-b1-s1 testped1_g2-b2-s1 1       0      0     -0.0836  83984
#>  5 testped1_g1-b1-s1 testped1_g2-b2-i1 0.00265 0.997  0      0.204   83989
#>  6 testped1_g1-b1-s1 testped1_g3-b1-i1 0.907   0.0927 0      0.00631 83985
#>  7 testped1_g1-b1-s1 testped1_g3-b2-i1 0.623   0.377  0      0.0589  83986
#>  8 testped1_g1-b1-s1 testped2_g1-b1-s1 1       0      0     -0.0766  83994
#>  9 testped1_g1-b1-s1 testped2_g1-b1-i1 1       0      0     -0.125   83986
#> 10 testped1_g1-b1-s1 testped2_g2-b1-s1 1       0      0     -0.0479  83978
#> # ℹ 770 more rows
```
