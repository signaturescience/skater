#' Fam to pedigree
#'
#' @description
#'
#' Converts a [PLINK-formatted fam file](https://www.cog-genomics.org/plink/1.9/formats#fam) to a pedigree object using [kinship2::pedigree].
#'
#'
#' @param fam A tibble with six columns of PLINK .fam data as read in by [read_fam].
#'
#' @return A tibble with new listcol `ped` containing pedigrees from `kinship2::pedigree`.
#'
#' @examples
#' famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
#' fam <- read_fam(famfile)
#' fam2ped(fam)
#'
#' @export
fam2ped <- function(fam) {
  # Replace 0 father and mother IDs with NA
  fam <- fam %>% dplyr::mutate(dplyr::across(c(dadid, momid), dplyr::na_if, 0))
  # Make affected status 1
  fam <- fam %>% dplyr::mutate(affected=1)
  # Create a listcol with the pedigree object for each family
  peds <-
    fam %>%
    tidyr::nest(data=c(-fid)) %>%
    dplyr::mutate(ped=purrr::map(data, ~with(., kinship2::pedigree(id, dadid, momid, sex, affected))))
  # Return output
  return(peds)
}


#' Plot pedigree
#'
#' @param ped List of pedigree objects from [fam2ped]
#' @param file Output file path (must end in ".pdf")
#' @param width Width of output PDF
#' @param height Height of output PDF
#'
#' @export
plot_pedigree <- function(ped, file=NULL, width=10, height=8) {
  if (is.null(file)) stop("Must provide output PDF file path.")
  if (!identical(class(ped),"list")) stop("ped must contain list of pedigree objects from fam2ped.")
  if (!all(purrr::map_chr(ped, class)=="pedigree")) stop("ped must contain list of pedigree objects from fam2ped.")
  if (!grepl("\\.pdf$", file, ignore.case=TRUE)) stop("Must provide output PDF file path.")
  grDevices::pdf(file = file, width=width, height=height, onefile=TRUE)
  purrr::walk(ped, ~kinship2::plot.pedigree(., mar=c(1,4,1,4)))
  grDevices::dev.off()
}


#' Pedigree to pairwise kinship
#'
#' @description
#'
#' Converts a pedigree class object from [fam2ped][skater::fam2ped] to a pairwise list of relationships and their expected/theoretical kinship coefficient.
#'
#'
#' @param ped A "pedigree" class object from [fam2ped][skater::fam2ped].
#'
#' @return A tibble containing all pairwise kinship coefficients from the input pedigree.
#' @examples
#' famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
#' famfile %>%
#'   read_fam() %>%
#'   fam2ped() %>%
#'   dplyr::mutate(kinpairs=purrr::map(ped, ped2kinpair)) %>%
#'   dplyr::select(fid, kinpairs) %>%
#'   tidyr::unnest(cols=kinpairs)
#' @export
ped2kinpair <- function(ped) {
  if (class(ped)!="pedigree") stop("Input must be of class 'pedigree'.")
  kinship2::kinship(ped) %>%
    corrr::as_cordf(diagonal=0.5) %>%
    corrr::shave() %>%
    corrr::stretch() %>%
    stats::na.omit() %>%
    dplyr::transmute(id1=pmin(x, y), id2=pmax(x, y), k=r) %>%
    arrange_ids(id1, id2)
}
