# Read PLINK-formatted .fam file

Reads in a [PLINK-formatted .fam
file](https://www.cog-genomics.org/plink/1.9/formats#fam). Input `file`
must have six columns:

1.  Family ID

2.  Individual ID

3.  Father ID

4.  Mother ID

5.  Sex

6.  Affected Status

## Usage

``` r
read_fam(file)
```

## Arguments

- file:

  Input file path

## Value

A tibble containing the 6 columns from the fam file.

## Examples

``` r
famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
fam <- read_fam(famfile)
fam
#> # A tibble: 64 × 6
#>    fid      id                dadid             momid               sex affected
#>    <chr>    <chr>             <chr>             <chr>             <int>    <int>
#>  1 testped1 testped1_g1-b1-s1 0                 0                     1        1
#>  2 testped1 testped1_g1-b1-i1 0                 0                     2        1
#>  3 testped1 testped1_g2-b1-s1 0                 0                     1        1
#>  4 testped1 testped1_g2-b1-i1 testped1_g1-b1-s1 testped1_g1-b1-i1     2        1
#>  5 testped1 testped1_g2-b2-s1 0                 0                     1        1
#>  6 testped1 testped1_g2-b2-i1 testped1_g1-b1-s1 testped1_g1-b1-i1     2        1
#>  7 testped1 testped1_g3-b1-i1 testped1_g2-b1-s1 testped1_g2-b1-i1     2        1
#>  8 testped1 testped1_g3-b2-i1 testped1_g2-b2-s1 testped1_g2-b2-i1     1        1
#>  9 testped2 testped2_g1-b1-s1 0                 0                     2        1
#> 10 testped2 testped2_g1-b1-i1 0                 0                     1        1
#> # ℹ 54 more rows
```
