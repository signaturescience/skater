# Kinship coefficient to degree

Infers relationship degree given a kinship coefficient.

## Usage

``` r
kin2degree(k, max_degree = 3L)
```

## Arguments

- k:

  Kinship coefficient (numeric, typically between 0 and .5, although
  KING can produce values \<0).

- max_degree:

  Max degree resolution (default 3). Used to seed
  [dibble](https://signaturescience.github.io/skater/reference/dibble.md).
  Anything below the inference range of `max_degree` will report `NA`.
  See
  [dibble](https://signaturescience.github.io/skater/reference/dibble.md).

## Value

A vector with inferred degree, up to the maximum degree in `dibble`
(anything more distant is `NA`, i.e., unrelated).

## Examples

``` r
kin2degree(0.5)
#> [1] 0
kin2degree(0.25)
#> [1] 1
kin2degree(0.125)
#> [1] 2
kin2degree(0.0625)
#> [1] 3
kin2degree(0.03125)
#> [1] NA
kin2degree(0.03125, max_degree=5)
#> [1] 4
kin2degree(-0.05)
#> [1] NA
k <- seq(.02, .5, .03)
kin2degree(k)
#>  [1] NA  3  3  2  2  2  1  1  1  1  1  1  0  0  0  0  0
kin2degree(k, max_degree=5)
#>  [1] 5 3 3 2 2 2 1 1 1 1 1 1 0 0 0 0 0
tibble::tibble(k=k) %>% dplyr::mutate(degree=kin2degree(k))
#> # A tibble: 17 × 2
#>        k degree
#>    <dbl>  <int>
#>  1  0.02     NA
#>  2  0.05      3
#>  3  0.08      3
#>  4  0.11      2
#>  5  0.14      2
#>  6  0.17      2
#>  7  0.2       1
#>  8  0.23      1
#>  9  0.26      1
#> 10  0.29      1
#> 11  0.32      1
#> 12  0.35      1
#> 13  0.38      0
#> 14  0.41      0
#> 15  0.44      0
#> 16  0.47      0
#> 17  0.5       0
```
