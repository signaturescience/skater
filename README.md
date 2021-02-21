
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skater

<!-- badges: start -->

[![R-CMD-check](https://github.com/signaturescience/skater/workflows/R-CMD-check/badge.svg)](https://github.com/signaturescience/skater/actions)
<!-- badges: end -->

**S**NP-based **K**inship **A**nalysis **T**esting and **E**valuation:
miscellaneous **R** utilties.

## Installation

Install from GitHub:

``` r
remotes::install_github("signaturescience/skater", auth_token = github_pat(quiet))
```

A GitHub [personal access token](https://github.com/settings/tokens)
must be in the environment variable `GITHUB_PAT`.

## Examples

``` r
library(skater)
```

### Pedigrees and PLINK .fam files

Read a PLINK-formatted .fam file:

``` r
famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
fam <- read_fam(famfile)
fam
#> # A tibble: 64 x 6
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
#> # … with 54 more rows
```

Convert each family into a pedigree object (see the [kinship2
vignette](https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html)):

``` r
peds <- fam2ped(fam)
peds
#> # A tibble: 8 x 3
#>   fid      data             ped       
#>   <chr>    <list>           <list>    
#> 1 testped1 <tibble [8 × 5]> <pedigree>
#> 2 testped2 <tibble [8 × 5]> <pedigree>
#> 3 testped3 <tibble [8 × 5]> <pedigree>
#> 4 testped4 <tibble [8 × 5]> <pedigree>
#> 5 testped5 <tibble [8 × 5]> <pedigree>
#> 6 testped6 <tibble [8 × 5]> <pedigree>
#> 7 testped7 <tibble [8 × 5]> <pedigree>
#> 8 testped8 <tibble [8 × 5]> <pedigree>
```

In the example above, the resulting tibble is nested by family ID. The
`data` column contains the individual family information, while the
`ped` column contains the pedigree object for that family. You can
unnest any particular family:

``` r
peds %>% 
  dplyr::filter(fid=="testped1") %>% 
  tidyr::unnest(cols=data)
#> # A tibble: 8 x 7
#>   fid      id              dadid          momid            sex affected ped     
#>   <chr>    <chr>           <chr>          <chr>          <int>    <dbl> <list>  
#> 1 testped1 testped1_g1-b1… <NA>           <NA>               1        1 <pedigr…
#> 2 testped1 testped1_g1-b1… <NA>           <NA>               2        1 <pedigr…
#> 3 testped1 testped1_g2-b1… <NA>           <NA>               1        1 <pedigr…
#> 4 testped1 testped1_g2-b1… testped1_g1-b… testped1_g1-b…     2        1 <pedigr…
#> 5 testped1 testped1_g2-b2… <NA>           <NA>               1        1 <pedigr…
#> 6 testped1 testped1_g2-b2… testped1_g1-b… testped1_g1-b…     2        1 <pedigr…
#> 7 testped1 testped1_g3-b1… testped1_g2-b… testped1_g2-b…     2        1 <pedigr…
#> 8 testped1 testped1_g3-b2… testped1_g2-b… testped1_g2-b…     1        1 <pedigr…
```

You can also look at a single pedigree:

``` r
peds$ped[[1]]
#> Pedigree object with 8 subjects
#> Bit size= 4
```

Or plot that pedigree:

``` r
plot(peds$ped[[1]], mar=c(1,4,1,4))
```

<img src="man/figures/README-plotped-1.png" width="100%" />

The `plot_pedigree` function in the skater package will walk over a list
of pedigree objects, writing a multi-page PDF to file, with each page
containing a pedigree from each of the families:

``` r
plot_pedigree(peds$ped, file="3gens.ped.pdf")
```

The `ped2kinpair()` function takes a pedigree object and produces a
pairwise list of relationships with their expected kinship coefficient.

Run on a single family:

``` r
ped2kinpair(peds$ped[[1]])
#> # A tibble: 36 x 3
#>    id1               id2                   k
#>    <chr>             <chr>             <dbl>
#>  1 testped1_g1-b1-s1 testped1_g1-b1-s1 0.5  
#>  2 testped1_g1-b1-i1 testped1_g1-b1-s1 0    
#>  3 testped1_g1-b1-s1 testped1_g2-b1-s1 0    
#>  4 testped1_g1-b1-s1 testped1_g2-b1-i1 0.25 
#>  5 testped1_g1-b1-s1 testped1_g2-b2-s1 0    
#>  6 testped1_g1-b1-s1 testped1_g2-b2-i1 0.25 
#>  7 testped1_g1-b1-s1 testped1_g3-b1-i1 0.125
#>  8 testped1_g1-b1-s1 testped1_g3-b2-i1 0.125
#>  9 testped1_g1-b1-i1 testped1_g1-b1-i1 0.5  
#> 10 testped1_g1-b1-i1 testped1_g2-b1-s1 0    
#> # … with 26 more rows
```

Map over all families:

``` r
kinpairs <- 
  peds %>% 
  dplyr::mutate(pairs=purrr::map(ped, ped2kinpair)) %>% 
  dplyr::select(fid, pairs) %>% 
  tidyr::unnest(cols=pairs)
kinpairs
#> # A tibble: 288 x 4
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
#> # … with 278 more rows
```

Note that this maps `ped2kinpair` over all `ped` objects in the input
tibble, and that relationships are not shown for between-family
relationships (which should all be zero).

### Degree inference

The `dibble()` function creates a **d**egree **i**nference tibble, with
degrees up to the specified `max_degree` (default=3), expected kinship
coefficient, and lower (`l`) and upper (`u`) inference ranges (see
[Manichaikul
2010](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3025716/)). Degree 0
corresponds to self / identity / MZ twins, with an expected kinship
coefficient of 0.5, with inference range &gt;=0.354. Anything beyond the
maximum degree resolution is considered unrelated (degree `NA`), with
expected kinship coefficient of 0.

``` r
dibble()
#> # A tibble: 5 x 4
#>   degree      k       l      u
#>    <int>  <dbl>   <dbl>  <dbl>
#> 1      0 0.5     0.354  1     
#> 2      1 0.25    0.177  0.354 
#> 3      2 0.125   0.0884 0.177 
#> 4      3 0.0625  0.0442 0.0884
#> 5     NA 0      -1      0.0442
```

The degree inference `max_degree` default is 3. Change this argument to
allow more granular degree inference ranges:

``` r
dibble(max_degree = 5)
#> # A tibble: 7 x 4
#>   degree      k       l      u
#>    <int>  <dbl>   <dbl>  <dbl>
#> 1      0 0.5     0.354  1     
#> 2      1 0.25    0.177  0.354 
#> 3      2 0.125   0.0884 0.177 
#> 4      3 0.0625  0.0442 0.0884
#> 5      4 0.0312  0.0221 0.0442
#> 6      5 0.0156  0.0110 0.0221
#> 7     NA 0      -1      0.0110
```

Note that the distance between relationship degrees becomes smaller as
the relationship degree becomes more distant. The `dibble()` function
will throw a warning with `max_degree` &gt;=10, and will stop with an
error at &gt;=12.

The `kin2degree()` function infers the relationship degree given a
kinship coefficient and a `max_degree` up to which anything more distant
is treated as unrelated. Example first degree relative:

``` r
kin2degree(.25, max_degree=3)
#> [1] 1
```

Example 4th degree relative, but using the default max\_degree
resolution of 3:

``` r
kin2degree(.0312, max_degree=3)
#> [1] NA
```

Example 4th degree relative, but increasing the degree resolution:

``` r
kin2degree(.0312, max_degree=5)
#> [1] 4
```

The `kin2degree()` function is vectorized over values of `k`, so can be
used inside of a `mutate` on a tibble of kinship coefficients:

``` r
# Get two pairs from each type of relationship we have in kinpairs:
kinpairs_subset <- 
  kinpairs %>% 
  dplyr::group_by(k) %>% 
  dplyr::slice(1:2)
kinpairs_subset
#> # A tibble: 10 x 4
#> # Groups:   k [5]
#>    fid      id1               id2                    k
#>    <chr>    <chr>             <chr>              <dbl>
#>  1 testped1 testped1_g1-b1-i1 testped1_g1-b1-s1 0     
#>  2 testped1 testped1_g1-b1-s1 testped1_g2-b1-s1 0     
#>  3 testped1 testped1_g3-b1-i1 testped1_g3-b2-i1 0.0625
#>  4 testped2 testped2_g3-b1-i1 testped2_g3-b2-i1 0.0625
#>  5 testped1 testped1_g1-b1-s1 testped1_g3-b1-i1 0.125 
#>  6 testped1 testped1_g1-b1-s1 testped1_g3-b2-i1 0.125 
#>  7 testped1 testped1_g1-b1-s1 testped1_g2-b1-i1 0.25  
#>  8 testped1 testped1_g1-b1-s1 testped1_g2-b2-i1 0.25  
#>  9 testped1 testped1_g1-b1-s1 testped1_g1-b1-s1 0.5   
#> 10 testped1 testped1_g1-b1-i1 testped1_g1-b1-i1 0.5

# Infer degree out to third degree relatives:
kinpairs_subset %>% 
  dplyr::mutate(degree=kin2degree(k, max_degree=3))
#> # A tibble: 10 x 5
#> # Groups:   k [5]
#>    fid      id1               id2                    k degree
#>    <chr>    <chr>             <chr>              <dbl>  <int>
#>  1 testped1 testped1_g1-b1-i1 testped1_g1-b1-s1 0          NA
#>  2 testped1 testped1_g1-b1-s1 testped1_g2-b1-s1 0          NA
#>  3 testped1 testped1_g3-b1-i1 testped1_g3-b2-i1 0.0625      3
#>  4 testped2 testped2_g3-b1-i1 testped2_g3-b2-i1 0.0625      3
#>  5 testped1 testped1_g1-b1-s1 testped1_g3-b1-i1 0.125       2
#>  6 testped1 testped1_g1-b1-s1 testped1_g3-b2-i1 0.125       2
#>  7 testped1 testped1_g1-b1-s1 testped1_g2-b1-i1 0.25        1
#>  8 testped1 testped1_g1-b1-s1 testped1_g2-b2-i1 0.25        1
#>  9 testped1 testped1_g1-b1-s1 testped1_g1-b1-s1 0.5         0
#> 10 testped1 testped1_g1-b1-i1 testped1_g1-b1-i1 0.5         0
```
