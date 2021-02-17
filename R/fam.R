#' Fam to pedigree
#'
#' @param famfile PLINK-formatted fam file
#' @param plotped Logical, TRUE if you want to create a PDF with pedigree drawings at the same location as the fam file with extension .pedigree.pdf
#'
#' @return A tibble with new listcol `ped` containing pedigrees from `kinship2::pedigree`.
#' @md
#' @export
fam2ped <- function(famfile, plotped=TRUE) {
  # Read in the fam file
  fam <- readr::read_delim(famfile,
                           delim=" ",
                           col_names=c("fid", "id", "dadid", "momid", "sex", "affected"),
                           col_types="ccccii")
  # Replace 0 father and mother IDs with NA
  fam <- fam %>% dplyr::mutate(dplyr::across(c(dadid, momid), dplyr::na_if, 0))
  # Make affected status 1
  fam <- fam %>% dplyr::mutate(affected=1)
  # Create a listcol with the pedigree object for each family
  peds <-
    fam %>%
    tidyr::nest(data=c(-fid)) %>%
    dplyr::mutate(ped=purrr::map(data, ~with(., kinship2::pedigree(id, dadid, momid, sex, affected))))
  # Create a plot if plotped=TRUE
  if (plotped) {
    pdfoutfile <- paste0(famfile, ".pedigree.pdf")
    message(paste("Saving PDF plots of pedigrees to file:", pdfoutfile))
    grDevices::pdf(file = pdfoutfile, width=10, height=8, onefile=TRUE)
    purrr::walk(peds$ped, ~kinship2::plot.pedigree(., mar=c(1,4,1,4)))
    grDevices::dev.off()
  }
  return(peds)
}

#' Pedigree to pairwise kinship
#'
#' @param ped A "pedigree" class object from \link[skater]{fam2ped}.
#' @return A tibble containing all pairwise kinship coefficients from the input pedigree.
#' @md
#' @export
ped2kinpair <- function(ped) {
  if (class(ped)!="pedigree") stop("Input must be of class 'pedigree'.")
  kinship2::kinship(ped) %>%
    corrr::as_cordf(diagonal=0.5) %>%
    corrr::shave() %>%
    corrr::stretch() %>%
    stats::na.omit() %>%
    dplyr::transmute(id1=pmin(x, y), id2=pmax(x, y), k=r)
}
