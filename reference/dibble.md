# Degree tibble

Creates a tibble with degree, expected kinship coefficient, and
inference boundaries.

Rows will be created up to the `max_degree`, with an additional row for
any relationship more distant than `max_degree`. The `degree` value for
the final row will be `NA`. This represents inference criteria for
"unrelated" individuals. See examples.

## Usage

``` r
dibble(max_degree = 3L)
```

## Arguments

- max_degree:

  The most distant degree you want to measure (usually between 3-9,
  default 3).

## Value

A tibble containing the degree, expected kinship coefficient (`k`),
lower (`l`) and upper (`u`) inference bounds.

## Examples

``` r
dibble(3)
#> # A tibble: 5 × 4
#>   degree      k       l      u
#>    <int>  <dbl>   <dbl>  <dbl>
#> 1      0 0.5     0.354  1     
#> 2      1 0.25    0.177  0.354 
#> 3      2 0.125   0.0884 0.177 
#> 4      3 0.0625  0.0442 0.0884
#> 5     NA 0      -1      0.0442
dibble(10)
#> Warning: max_degree should be <10
#> # A tibble: 12 × 4
#>    degree        k         l        u
#>     <int>    <dbl>     <dbl>    <dbl>
#>  1      0 0.5       0.354    1       
#>  2      1 0.25      0.177    0.354   
#>  3      2 0.125     0.0884   0.177   
#>  4      3 0.0625    0.0442   0.0884  
#>  5      4 0.0312    0.0221   0.0442  
#>  6      5 0.0156    0.0110   0.0221  
#>  7      6 0.00781   0.00552  0.0110  
#>  8      7 0.00391   0.00276  0.00552 
#>  9      8 0.00195   0.00138  0.00276 
#> 10      9 0.000977  0.000691 0.00138 
#> 11     10 0.000488  0.000345 0.000691
#> 12     NA 0        -1        0.000345
```
