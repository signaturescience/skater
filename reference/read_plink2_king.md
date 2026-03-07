# Read PLINK KING table

Reads in the output from `plink2 --make-king-table`
([documentation](https://www.cog-genomics.org/plink/2.0/distance#make_king)).
Input `file` must have six columns, tab delimited:

1.  id1 (member 1)

2.  id2 (member 2)

3.  nsnps

4.  hethet: proportion of sites where both are heterozygous

5.  k: Kinship Coefficient

## Usage

``` r
read_plink2_king(file)
```

## Arguments

- file:

  Input file path

## Value

A tibble containing the 6 columns from the `plink2 --make-king-table`
output.

## References

<https://www.cog-genomics.org/plink/2.0/distance#make_king>

## Examples

``` r
plink2kingFile <- system.file("extdata", "plink2-king-table.tsv", package="skater", mustWork=TRUE)
plink2king <- read_plink2_king(plink2kingFile)
plink2king
#> # A tibble: 45 × 6
#>    id1      id2       nsnps hethet   ibd0         k
#>    <chr>    <chr>     <int>  <dbl>  <dbl>     <dbl>
#>  1 g1-b1-i1 g1-b1-s1 590330 0.0639 0.0326 -0.0115  
#>  2 g1-b1-s1 g2-b1-s1 590330 0.0649 0.0324 -0.00387 
#>  3 g1-b1-i1 g2-b1-s1 590330 0.0628 0.0332 -0.0140  
#>  4 g1-b1-s1 g2-b1-s2 590330 0.0649 0.0325 -0.000565
#>  5 g1-b1-i1 g2-b1-s2 590330 0.0631 0.0334 -0.0182  
#>  6 g2-b1-s1 g2-b1-s2 590330 0.0643 0.0329 -0.00832 
#>  7 g1-b1-s1 g2-b1-i1 590330 0.0899 0       0.247   
#>  8 g1-b1-i1 g2-b1-i1 590330 0.0875 0       0.244   
#>  9 g2-b1-i1 g2-b1-s1 590330 0.0640 0.0321 -0.000468
#> 10 g2-b1-i1 g2-b1-s2 590330 0.0641 0.0325 -0.00622 
#> # ℹ 35 more rows
plink2king %>% dplyr::filter(k>0.01)
#> # A tibble: 20 × 6
#>    id1      id2       nsnps hethet    ibd0     k
#>    <chr>    <chr>     <int>  <dbl>   <dbl> <dbl>
#>  1 g1-b1-s1 g2-b1-i1 590330 0.0899 0       0.247
#>  2 g1-b1-i1 g2-b1-i1 590330 0.0875 0       0.244
#>  3 g1-b1-s1 g2-b1-i2 590330 0.0898 0       0.246
#>  4 g1-b1-i1 g2-b1-i2 590330 0.0892 0       0.248
#>  5 g2-b1-i1 g2-b1-i2 590330 0.105  0.00864 0.244
#>  6 g1-b1-s1 g2-b2-i1 590330 0.0922 0       0.253
#>  7 g1-b1-i1 g2-b2-i1 590330 0.0888 0       0.245
#>  8 g2-b1-i1 g2-b2-i1 590330 0.103  0.00926 0.233
#>  9 g2-b1-i2 g2-b2-i1 590330 0.110  0.00571 0.272
#> 10 g1-b1-s1 g2-b2-i2 590330 0.0928 0       0.254
#> 11 g1-b1-i1 g2-b2-i2 590330 0.0876 0       0.242
#> 12 g2-b1-i1 g2-b2-i2 590330 0.105  0.00957 0.239
#> 13 g2-b1-i2 g2-b2-i2 590330 0.104  0.0102  0.231
#> 14 g2-b2-i1 g2-b2-i2 590330 0.106  0.00807 0.248
#> 15 g1-b1-s1 g2-b3-i1 590330 0.0903 0       0.248
#> 16 g1-b1-i1 g2-b3-i1 590330 0.0879 0       0.245
#> 17 g2-b1-i1 g2-b3-i1 590330 0.113  0.00857 0.268
#> 18 g2-b1-i2 g2-b3-i1 590330 0.107  0.00835 0.252
#> 19 g2-b2-i1 g2-b3-i1 590330 0.0991 0.00978 0.219
#> 20 g2-b2-i2 g2-b3-i1 590330 0.0998 0.0113  0.214
```
