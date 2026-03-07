# Fam to pedigree

Converts a [PLINK-formatted fam
file](https://www.cog-genomics.org/plink/1.9/formats#fam) to a pedigree
object using
[kinship2::pedigree](https://rdrr.io/pkg/kinship2/man/pedigree.html).

## Usage

``` r
fam2ped(fam)
```

## Arguments

- fam:

  A tibble with six columns of PLINK .fam data as read in by
  [read_fam](https://signaturescience.github.io/skater/reference/read_fam.md).

## Value

A tibble with new listcol `ped` containing pedigrees from
[`kinship2::pedigree`](https://rdrr.io/pkg/kinship2/man/pedigree.html).

## Examples

``` r
famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
fam <- read_fam(famfile)
fam2ped(fam)
#> # A tibble: 8 × 3
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
