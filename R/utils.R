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
#' @export
#'
#' @examples
#' dibble(3)
#' dibble(10)
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
