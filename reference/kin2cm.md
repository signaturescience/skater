# Kinship coefficient to cM

"Converts" a kinship coefficient to put on the same scale as shared cM
using the formula \\cm \<- pmin(3560, 4\*pmax(0, k)\*3560)\\.

## Usage

``` r
kin2cm(k)
```

## Arguments

- k:

  Kinship coefficient (numeric, typically between 0 and .5, although
  KING can produce values \<0).

## Value

A vector of numeric estimated cM, ranging from 0-3560.

## References

<https://dnapainter.com/tools/sharedcmv4>.

<https://www.ancestry.com/dna/resource/whitePaper/AncestryDNA-Matching-White-Paper.pdf>.

<https://verogen.com/wp-content/uploads/2021/03/snp-typing-uas-kinship-estimation-gedmatch-pro-tech-note-vd2020058-a.pdf>.

## Examples

``` r
kin2cm(.25)
#> [1] 3560
kin2cm(.125)
#> [1] 1780
kin2cm(.0625)
#> [1] 890
dibble(9) %>% dplyr::mutate(cm=kin2cm(k))
#> # A tibble: 11 × 5
#>    degree        k         l        u     cm
#>     <int>    <dbl>     <dbl>    <dbl>  <dbl>
#>  1      0 0.5       0.354    1        3560  
#>  2      1 0.25      0.177    0.354    3560  
#>  3      2 0.125     0.0884   0.177    1780  
#>  4      3 0.0625    0.0442   0.0884    890  
#>  5      4 0.0312    0.0221   0.0442    445  
#>  6      5 0.0156    0.0110   0.0221    222. 
#>  7      6 0.00781   0.00552  0.0110    111. 
#>  8      7 0.00391   0.00276  0.00552    55.6
#>  9      8 0.00195   0.00138  0.00276    27.8
#> 10      9 0.000977  0.000691 0.00138    13.9
#> 11     NA 0        -1        0.000691    0  
```
