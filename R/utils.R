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
#' @return A tibble (class "dibble") containing the degree, expected kinship
#'   coefficient (`k`), lower (`l`) and upper (`u`) inference bounds.
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
    dplyr::mutate(k=ifelse(dplyr::row_number()==dplyr::n(), 0, k)) %>%
    structure(class=c("dibble", class(.)))
}


#' Infer degree for a kinship coefficient
#'
#' @param k Kinship coefficient (numeric, typically between 0 and .5, although KING can produce values <0).
#' @param dibble Degree tibble from [dibble][skater::dibble].
#'
#' @return Inferred degree, up to the maximum degree in `dibble` (anything more distant is `NA`, i.e., unrelated).
#'
#' @examples
#' d3 <- dibble(3)
#' infer_degree(0.5, d3)
#' infer_degree(0.25, d3)
#' infer_degree(0.125, d3)
#' infer_degree(0.0625, d3)
#' infer_degree(0, d3)
#' infer_degree(-0.05, d3)
#' k <- seq(.02, .5, .03)
#' infer_degree(k, d3)
#' tibble::tibble(k=k) %>% dplyr::mutate(degree=infer_degree(k, d3))
#' @export
infer_degree <- function(k, dibble) {
  stopifnot(all(is.numeric(k)))
  stopifnot(all(k %>% dplyr::between(-1, 1)))
  stopifnot("dibble" %in% class(dibble))
  vapply(k, function(k) as.integer(dibble$degree[which(purrr::map2_lgl(dibble$l, dibble$u, ~dplyr::between(k, .x, .y)))]), FUN.VALUE=1L)
}
