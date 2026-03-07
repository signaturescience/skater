# Pedigree to pairwise kinship

Converts a pedigree class object from
[fam2ped](https://signaturescience.github.io/skater/reference/fam2ped.md)
to a pairwise list of relationships and their expected/theoretical
kinship coefficient.

## Usage

``` r
ped2kinpair(ped)
```

## Arguments

- ped:

  A "pedigree" class object from
  [fam2ped](https://signaturescience.github.io/skater/reference/fam2ped.md).

## Value

A tibble containing all pairwise kinship coefficients from the input
pedigree.

## Examples

``` r
famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
famfile %>%
  read_fam() %>%
  fam2ped() %>%
  dplyr::mutate(kinpairs=purrr::map(ped, ped2kinpair)) %>%
  dplyr::select(fid, kinpairs) %>%
  tidyr::unnest(cols=kinpairs)
#> # A tibble: 288 × 4
#>    fid      id1               id2                   k
#>    <chr>    <chr>             <chr>             <dbl>
#>  1 testped1 testped1_g1-b1-s1 testped1_g1-b1-s1 0.5  
#>  2 testped1 testped1_g1-b1-i1 testped1_g1-b1-s1 0    
#>  3 testped1 testped1_g1-b1-s1 testped1_g2-b1-s1 0    
#>  4 testped1 testped1_g1-b1-s1 testped1_g2-b1-i1 0.25 
#>  5 testped1 testped1_g1-b1-s1 testped1_g2-b2-s1 0    
#>  6 testped1 testped1_g1-b1-s1 testped1_g2-b2-i1 0.25 
#>  7 testped1 testped1_g1-b1-s1 testped1_g3-b1-i1 0.125
#>  8 testped1 testped1_g1-b1-s1 testped1_g3-b2-i1 0.125
#>  9 testped1 testped1_g1-b1-i1 testped1_g1-b1-i1 0.5  
#> 10 testped1 testped1_g1-b1-i1 testped1_g2-b1-s1 0    
#> # ℹ 278 more rows
```
