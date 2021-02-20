#' Degree tibble
#'
#' Creates a tibble with degree, expected kinship coefficient, and inference boundaries.
#'
#' Rows will be created up to the `max_degree`, with an additional row for any
#' relationship more distant than `max_degree`. The `degree` value for the final
#' row will be `NA`. This represents inference criteria for "unrelated"
#' individuals. See examples.
#'
#' @param max_degree The most distant degree you want to measure.
#'
#' @return A tibble containing the degree, expected kinship coefficient (`k`),
#'   lower (`l`) and upper (`u`) inference bounds for that degree.
#'
#' @examples
#' dibble(3)
#' dibble(10)
#'
#' @export
dibble <- function(max_degree=3L) {
  stopifnot(is.numeric(max_degree))
  max_degree <- as.integer(round(max_degree))
  tibble::tibble(degree=0:(max_degree+1)) %>%
    dplyr::mutate(k=.5^(degree+1)) %>%
    dplyr::mutate(l=sqrt(dplyr::lead(k, default=dplyr::last(k)/2)  * k)) %>%
    dplyr::mutate(u=sqrt(dplyr::lag(k,  default=dplyr::first(k)*2) * k)) %>%
    dplyr::mutate(u=ifelse(dplyr::row_number()==1L, 1, u)) %>%
    dplyr::mutate(l=ifelse(dplyr::row_number()==dplyr::n(), -1, l)) %>%
    dplyr::mutate(degree=ifelse(dplyr::row_number()==dplyr::n(), NA_integer_, degree)) %>%
    dplyr::mutate(k=ifelse(dplyr::row_number()==dplyr::n(), 0, k))
}


#' Infer degree for a kinship coefficient
#'
#' @param k Kinship coefficient (numeric, typically between 0 and .5, although KING can produce values <0).
#' @param degree_resolution The maximum degree relationship you want to infer.
#'   Defaults to third-degree relatives. Anything lower than `degree_resolution`
#'   will produce `NA` (i.e., unrelated). See [dibble][skater::dibble].
#'
#' @return Inferred degree, up to the `degree_resolution` (anything more distant is `NA`, i.e., unrelated).
#'
#' @examples
#' infer_degree(0.0625)
#' infer_degree(0.125)
#' infer_degree(0.25)
#' infer_degree(0.5)
#'
#' @export
infer_degree <- function(k, degree_resolution=3L) {
  stopifnot(is.numeric(k))
  stopifnot(k %>% dplyr::between(-1, 1))
  d <- dibble(degree_resolution)
  as.integer(d$degree[which(purrr::map2_lgl(d$l, d$u, ~dplyr::between(k, .x, .y)))])
}
