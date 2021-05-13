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
  ## slow
  # degree <- vapply(k, function(k) as.integer(d$degree[which(purrr::map2_lgl(d$l, d$u, ~dplyr::between(k, .x, .y)))]), FUN.VALUE=1L)
  ## Fast
  d.lowerbound <- sort(d$l)
  d.degree <- rev(d$degree)
  degree <- d.degree[findInterval(k, d.lowerbound)]
  return(degree)
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


#' Order IDs across two columns
#'
#' @description Some types of data or results are indexed by two identifiers in
#'   two different columns corresponding to data points for _pairs_ of
#'   observations. E.g., you may have columns called `id1` and `id2` that index
#'   the tibble for all possible pairs of results between samples A, B, and C.
#'   If you attempt to join two tibbles with `by=c("id1", "id2")`, the join will
#'   fail if samples are flipped from one dataset to another. E.g., one tibble
#'   may have id1=A and id2=B while the other has id1=B and id2=A. This function
#'   ensures that id1 is alphanumerically first while id2 is alphanumerically
#'   second. See examples.
#'
#' @param .data A tibble with two ID columns to arrange.
#' @param .id1 Unquoted name of the "id1" column. See examples.
#' @param .id2 Unquoted name of the "id2" column. See examples.
#'
#' @return A tibble with id1 and id2 rearranged alphanumerically.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' d1 <- tibble::tribble(
#'   ~id1, ~id2, ~results1,
#'   "a",  "b",       10L,
#'   "a",  "c",       20L,
#'   "c",  "b",       30L
#' )
#' d2 <- tibble::tribble(
#'   ~id1, ~id2,  ~results2,
#'   "b",  "a",       101L,
#'   "c",  "a",       201L,
#'   "b",  "c",       301L
#' )
#' # Inner join fails because id1!=id2.
#' dplyr::inner_join(d1, d2, by=c("id1", "id2"))
#' # Arrange IDs
#' d1 %>% arrange_ids(id1, id2)
#' d2 %>% arrange_ids(id1, id2)
#' # Inner join
#' dplyr::inner_join(arrange_ids(d1, id1, id2), arrange_ids(d2, id1, id2), by=c("id1", "id2"))
#' # Recursively, if you had more than two tibbles
#' list(d1, d2) %>%
#'   purrr::map(arrange_ids, id1, id2) %>%
#'   purrr::reduce(dplyr::inner_join, by=c("id1", "id2"))
arrange_ids <- function(.data, .id1, .id2) {
  .id1=rlang::enquo(.id1)
  .id2=rlang::enquo(.id2)
  .data %>%
    dplyr::mutate(.x1=pmin(!!.id1, !!.id2)) %>%
    dplyr::mutate(.x2=pmax(!!.id1, !!.id2)) %>%
    dplyr::mutate(!!.id1:=.x1, !!.id2:=.x2) %>%
    dplyr::select(-.x1, -.x2)
}

