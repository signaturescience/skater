
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

## Package data

### Package data objects

Unexported objects `ped1kg` and `ped1kg_unrel` contain [pedigree data
obtained from 1000 Genomes
FTP](http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/integrated_call_samples_v3.20200731.ALL.ped)
for all 2504 individuals with sequencing data, and all individuals
unrelated to anyone else in the data (momid, dadid, siblings,
second\_order, and third\_order all =0). This code gets all the unique
IDs for anyone listed as a mother, father, sibling, child, second order,
or third order relative, *who also has sequencing data*, and removes any
*individual ID* who matches one of these relatives. See
[data-raw/generate\_sysdata.R](data-raw/generate_sysdata.R),
specifically the code that begins with the `relatives <-` assignment.

``` r
skater:::ped1kg
#> # A tibble: 2,504 x 13
#>    fid   id    dadid momid   sex affected population relationship siblings
#>    <chr> <chr> <chr> <chr> <dbl>    <dbl> <chr>      <chr>        <chr>   
#>  1 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  2 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  3 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  4 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  5 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  6 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  7 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  8 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  9 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#> 10 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#> # … with 2,494 more rows, and 4 more variables: second_order <chr>,
#> #   third_order <chr>, children <chr>, other_comments <chr>
skater:::ped1kg_unrel
#> # A tibble: 2,502 x 13
#>    fid   id    dadid momid   sex affected population relationship siblings
#>    <chr> <chr> <chr> <chr> <dbl>    <dbl> <chr>      <chr>        <chr>   
#>  1 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  2 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  3 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  4 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  5 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  6 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#>  7 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  8 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#>  9 HG00… HG00… 0     0         2        0 GBR        unrel        0       
#> 10 HG00… HG00… 0     0         1        0 GBR        unrel        0       
#> # … with 2,492 more rows, and 4 more variables: second_order <chr>,
#> #   third_order <chr>, children <chr>, other_comments <chr>
```

There are **2** samples who have known relatives in the 1000 Genomes ped
file (included in `ped1kg` but not in `ped1kg_unrel`):

``` r
skater:::ped1kg %>% 
  dplyr::anti_join(skater:::ped1kg_unrel) %>% 
  dplyr::select(fid, id, population, second_order:other_comments) %>% 
  knitr::kable()
#> Joining, by = c("fid", "id", "dadid", "momid", "sex", "affected", "population", "relationship", "siblings", "second_order", "third_order", "children", "other_comments")
```

| fid     | id      | population | second\_order | third\_order | children | other\_comments                          |
|:--------|:--------|:-----------|:--------------|:-------------|:---------|:-----------------------------------------|
| LWK001  | NA19331 | LWK        | 0             | NA19334      | NA19313  | Parent/Child directionality is uncertain |
| NA19334 | NA19334 | LWK        | NA19313       | NA19331      | 0        | 0                                        |

### Text files with unrelated founder sample IDs

After creating the `ped1kg_unrel` object in
[data-raw/generate\_sysdata.R](data-raw/generate_sysdata.R) as described
above, the code in
[data-raw/write-sampleids-1000g-unrelated.R](data-raw/write-sampleids-1000g-unrelated.R)
then writes one sample ID per line for each of those unrelated samples
to `inst/extdata`. These can be used to pass to
`bcftools view --samples-file <POP.txt>`.

The directory containing these files can be found on any system where
skater is installed. On MacOS, this is usually something like
`/Library/Frameworks/R.framework/Versions/4.0/Resources/library/skater/extdata/sampleids-1000g-unrelated`.

``` r
system.file("extdata", "sampleids-1000g-unrelated", package="skater", mustWork=TRUE)
```

The table below shows how many unrelated founders are available in any
given population:

``` r
system.file("extdata", "sampleids-1000g-unrelated", package="skater", mustWork=TRUE) %>% 
  list.files(full.names=TRUE) %>% 
  rlang::set_names(basename) %>% 
  purrr::map_int(~length(readr::read_lines(.))) %>% 
  tibble::enframe() %>% 
  knitr::kable()
```

| name    | value |
|:--------|------:|
| ACB.txt |    96 |
| ASW.txt |    61 |
| BEB.txt |    86 |
| CDX.txt |    93 |
| CEU.txt |    99 |
| CHB.txt |   103 |
| CHS.txt |   105 |
| CLM.txt |    94 |
| ESN.txt |    99 |
| FIN.txt |    99 |
| GBR.txt |    91 |
| GIH.txt |   103 |
| GWD.txt |   113 |
| IBS.txt |   107 |
| ITU.txt |   102 |
| JPT.txt |   104 |
| KHV.txt |    99 |
| LWK.txt |    97 |
| MSL.txt |    85 |
| MXL.txt |    64 |
| PEL.txt |    85 |
| PJL.txt |    96 |
| PUR.txt |   104 |
| STU.txt |   102 |
| TSI.txt |   107 |
| YRI.txt |   108 |
