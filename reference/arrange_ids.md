# Order IDs across two columns

Some types of data or results are indexed by two identifiers in two
different columns corresponding to data points for *pairs* of
observations. E.g., you may have columns called `id1` and `id2` that
index the tibble for all possible pairs of results between samples A, B,
and C. If you attempt to join two tibbles with `by=c("id1", "id2")`, the
join will fail if samples are flipped from one dataset to another. E.g.,
one tibble may have id1=A and id2=B while the other has id1=B and id2=A.
This function ensures that id1 is alphanumerically first while id2 is
alphanumerically second. See examples.

## Usage

``` r
arrange_ids(.data, .id1, .id2)
```

## Arguments

- .data:

  A tibble with two ID columns to arrange.

- .id1:

  Unquoted name of the "id1" column. See examples.

- .id2:

  Unquoted name of the "id2" column. See examples.

## Value

A tibble with id1 and id2 rearranged alphanumerically.

## Examples

``` r
d1 <- tibble::tribble(
  ~id1, ~id2, ~results1,
  "a",  "b",       10L,
  "a",  "c",       20L,
  "c",  "b",       30L
)
d2 <- tibble::tribble(
  ~id1, ~id2,  ~results2,
  "b",  "a",       101L,
  "c",  "a",       201L,
  "b",  "c",       301L
)
# Inner join fails because id1!=id2.
dplyr::inner_join(d1, d2, by=c("id1", "id2"))
#> # A tibble: 0 × 4
#> # ℹ 4 variables: id1 <chr>, id2 <chr>, results1 <int>, results2 <int>
# Arrange IDs
d1 %>% arrange_ids(id1, id2)
#> # A tibble: 3 × 3
#>   id1   id2   results1
#>   <chr> <chr>    <int>
#> 1 a     b           10
#> 2 a     c           20
#> 3 b     c           30
d2 %>% arrange_ids(id1, id2)
#> # A tibble: 3 × 3
#>   id1   id2   results2
#>   <chr> <chr>    <int>
#> 1 a     b          101
#> 2 a     c          201
#> 3 b     c          301
# Inner join
dplyr::inner_join(arrange_ids(d1, id1, id2), arrange_ids(d2, id1, id2), by=c("id1", "id2"))
#> # A tibble: 3 × 4
#>   id1   id2   results1 results2
#>   <chr> <chr>    <int>    <int>
#> 1 a     b           10      101
#> 2 a     c           20      201
#> 3 b     c           30      301
# Recursively, if you had more than two tibbles
list(d1, d2) %>%
  purrr::map(arrange_ids, id1, id2) %>%
  purrr::reduce(dplyr::inner_join, by=c("id1", "id2"))
#> # A tibble: 3 × 4
#>   id1   id2   results1 results2
#>   <chr> <chr>    <int>    <int>
#> 1 a     b           10      101
#> 2 a     c           20      201
#> 3 b     c           30      301
```
