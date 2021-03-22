#' Degree tibble
#'
#' @description
#' Creates a tibble with degree, expected kinship coefficient, and inference boundaries.
#'
#' Rows will be created up to the `max_degree`, with an additional row for any
#' relationship more distant than `max_degree`. The `degree` value for the final
#' row will be `NA`. This represents inference criteria for "unrelated"
#' individuals. See examples.
#'
#' @param max_degree The most distant degree you want to measure (usually between 3-9, default 3).
#'
#' @return A tibble containing the degree, expected kinship coefficient (`k`),
#'   lower (`l`) and upper (`u`) inference bounds.
#'
#' @examples
#' dibble(3)
#' dibble(10)
#'
#' @export
dibble <- function(max_degree=3L) {
  if (!is.numeric(max_degree)) stop("max_degree must be numeric")
  if (max_degree<1) stop("max_degree must be >=1")
  if (max_degree<3) warning("max_degree should be >=3")
  if (max_degree>=12) stop("max_degree must be <12")
  if (max_degree>=10) warning("max_degree should be <10")
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


#' Kinship coefficient to degree
#'
#' @description
#' Infers relationship degree given a kinship coefficient.
#'
#' @param k Kinship coefficient (numeric, typically between 0 and .5, although KING can produce values <0).
#' @param max_degree Max degree resolution (default 3). Used to seed
#'   [dibble][skater::dibble]. Anything below the inference range of
#'   `max_degree` will report `NA`. See [dibble][skater::dibble].
#'
#' @return Inferred degree, up to the maximum degree in `dibble` (anything more distant is `NA`, i.e., unrelated).
#'
#' @examples
#' kin2degree(0.5)
#' kin2degree(0.25)
#' kin2degree(0.125)
#' kin2degree(0.0625)
#' kin2degree(0.03125)
#' kin2degree(0.03125, max_degree=5)
#' kin2degree(-0.05)
#' k <- seq(.02, .5, .03)
#' kin2degree(k)
#' kin2degree(k, max_degree=5)
#' tibble::tibble(k=k) %>% dplyr::mutate(degree=kin2degree(k))
#'
#' @export
kin2degree <- function(k, max_degree=3L) {
  stopifnot(all(is.numeric(k)))
  stopifnot(all(k %>% dplyr::between(-1, 1)))
  stopifnot(is.numeric(max_degree))
  d <- dibble(max_degree)
  vapply(k, function(k) as.integer(d$degree[which(purrr::map2_lgl(d$l, d$u, ~dplyr::between(k, .x, .y)))]), FUN.VALUE=1L)
}


#' Kinship coefficient to cM
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' "Converts" a kinship coefficient to put on the same scale as shared cM using the formula
#' \eqn{cm <- pmin(3560, 4*pmax(0, k)*3560)}.
#'
#'
#' @param k Kinship coefficient (numeric, typically between 0 and .5, although KING can produce values <0).
#'
#' @return Estimated cM, ranging from 0-3560.
#'
#' @examples
#' kin2cm(.25)
#' kin2cm(.125)
#' kin2cm(.0625)
#' dibble(9) %>% dplyr::mutate(cm=kin2cm(k))
#'
#' @references <https://dnapainter.com/tools/sharedcmv4>.
#' @references <https://www.ancestry.com/dna/resource/whitePaper/AncestryDNA-Matching-White-Paper.pdf>.
#' @references <https://verogen.com/wp-content/uploads/2021/03/snp-typing-uas-kinship-estimation-gedmatch-pro-tech-note-vd2020058-a.pdf>.
#'
#'
#' @export
kin2cm <- function(k) {
  stopifnot(all(is.numeric(k)))
  stopifnot(all(k %>% dplyr::between(-1, 1)))
  cm <- pmin(3560, 4*pmax(0, k)*3560)
  return(cm)
}
