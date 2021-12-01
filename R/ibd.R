#' Compute kinship coefficient from IBD segments
#'
#' @description
#' This function is used to retrieve a relatedness measure from IBD segments.
#' The relatedness value returned is the kinship coefficient.
#'
#' @details
#' The input data should be pairwise IBD segments prepared via
#' \link[skater]{read_ibd}. The function will internally loop over each
#' chromosome, and use a specified genetic map to convert shared segments to
#' genetic units. After doing so, the function converts the shared length to a
#' kinship coefficient by summing \eqn{0.5*IBD2 + 0.25*IBD1}.
#'
#' Note that the data read in by [read_ibd] when `source="pedsim"` returns a
#' list with separate tibbles for IBD1 and IBD2 segments. The current
#' implementation of this function requires running this function independently
#' on IBD1 and IBD2 segments, then summarizing (adding) the corresponding
#' proportions. See examples.
#'
#' @param .ibd_data Tibble with IBD segments created using the \link[skater]{read_ibd} function
#' @param .map Tibble with the genetic map data created using the \link[skater]{read_map} function
#' @param type Type of IBD to use for kinship coefficient calculation; must be `'IBD1'`, `'IBD2'`, or `NULL` (both IBD1 and IBD2 will be treated the same); default is `NULL`
#'
#' @return Tibble with three columns:
#'
#' 1. id1 (sample identifier 1)
#' 2. id2 (sample identifier 2)
#' 3. kinship (kinship coefficent derived from shared segments)
#'
#' @export
#'
#' @examples
#' pedsim_fp <- system.file("extdata", "GBR.sim.seg.gz", package="skater", mustWork=TRUE)
#' pedsim_seg <- read_ibd(pedsim_fp, source = "pedsim")
#' gmapfile <- system.file("extdata", "sexspec-avg-min.plink.map", package="skater", mustWork=TRUE)
#' gmap <- read_map(gmapfile)
#' ibd1_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD1, .map=gmap, type="IBD1")
#' ibd2_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD2, .map=gmap, type="IBD2")
#' dplyr::bind_rows(ibd1_dat,ibd2_dat) %>%
#'   dplyr::group_by(id1,id2) %>%
#'   dplyr::summarise(kinship = sum(kinship), .groups = "drop")
#'
#' @references http://faculty.washington.edu/sguy/ibd_relatedness.html
#'
ibd2kin <- function(.ibd_data, .map, type = NULL) {

  ## get the min base pair for each chr
  ibd_min_bp <-
    .ibd_data %>%
    dplyr::group_by(chr) %>%
    dplyr::summarise(bp = min(start), .groups = "drop")

  ## get the max base pair for each chromsome
  ibd_max_bp <-
    .ibd_data  %>%
    dplyr::group_by(chr) %>%
    dplyr::summarise(bp = max(end), .groups = "drop")

  ## loop over each chromosome to get length
  ## this will be summed up and serve as denominator for kinship coefficient calculations

  ## some preprocessing on map for computational efficiency ...
  ## group_by and group_split on chr
  ## do that separately so you can get the chr names for indexing
  tmp <-
    .map %>%
    dplyr::group_by(chr)

  tmp_l <-
    tmp %>%
    dplyr::group_split() %>%
    ## get name for each list element from corresponding group key
    ## NOTE: group_key returns a tibble so you need to get chr column out as vec
    purrr::set_names(dplyr::group_keys(tmp)$chr)

  ## cleanup
  rm(tmp)

  totalchromlength <- vector()
  for(i in 1:length(unique(ibd_min_bp$chr))) {

    chrom <- unique(ibd_min_bp$chr)[i]

    ## these steps just get the min/max bp and map data for the given chromosome
    tmp_ibd_min_bp <-
      ibd_min_bp %>%
      dplyr::filter(chr == chrom)

    tmp_ibd_max_bp <-
      ibd_max_bp %>%
      dplyr::filter(chr == chrom)

    tmp_chromgpos <- tmp_l[[chrom]]

    ## use interpolate to get centimorgan lengths
    mincm <- interpolate(ibd_bp = tmp_ibd_min_bp$bp, chromgpos = tmp_chromgpos)
    maxcm <- interpolate(ibd_bp = tmp_ibd_max_bp$bp, chromgpos = tmp_chromgpos)

    ## create chrom length and append to vector
    tmp_totalchromlength <- maxcm - mincm
    totalchromlength <- c(totalchromlength, tmp_totalchromlength)
  }

  ## use ibd data and total chromlength as denominator above to get the kinship value
  ## NOTE: the total length of genome is multiplied by either 2 or 4 (depending on IBD type) to get the kinship coefficient "units"
  ## ... i.e. the probability that a randomly selected allele will be shared between two individuals
  tmp <-
    .ibd_data %>%
    dplyr::group_by(id1,id2) %>%
    dplyr::summarise(totallength = sum(length), .groups = "drop")

  ## the type argument switches the multiplier used to calculate denominator
  if(!is.null(type)) {
    if (toupper(type) == "IBD1") {
      tmp %>%
        dplyr::mutate(kinship = totallength/(4*sum(totalchromlength))) %>%
        dplyr::select(id1,id2,kinship) %>%
        arrange_ids(id1, id2)
    } else if (toupper(type) == "IBD2") {
      tmp %>%
        dplyr::mutate(kinship = totallength/(2*sum(totalchromlength))) %>%
        dplyr::select(id1,id2,kinship) %>%
        arrange_ids(id1, id2)
    } else {
      stop("If the 'type' argument is not NULL then it should be either 'IBD1' or 'IBD2'.")
    }
  } else {
    tmp %>%
      dplyr::mutate(kinship = totallength/(4*sum(totalchromlength))) %>%
      dplyr::select(id1,id2,kinship) %>%
      arrange_ids(id1, id2)
  }
}

#' Interpolate over segments
#'
#' This is an unexported helper used in in \link[skater]{ibd2kin}. The function interpolates over segments to apply genetic length to the segment. It is inspired by Python code distributed by the Browning lab ([documentation](http://faculty.washington.edu/sguy/ibd_relatedness.html)).
#'
#' @param ibd_bp Base pair for the IBD segment over which to interpolate
#' @param chromgpos Genetic map data for a specific chromosome
#'
#' @return Numeric vector with the genetic distance shared at the segment.
#'
#' @references http://faculty.washington.edu/sguy/ibd_relatedness.html
interpolate <- function(ibd_bp, chromgpos) {

  tmp_set1 <-
    chromgpos %>%
    dplyr::filter(bp < ibd_bp)
  tmp_set2 <-
    chromgpos %>%
    dplyr::filter(bp > ibd_bp)

  if(nrow(tmp_set1) > 0 && nrow(tmp_set2) > 0) {
    tmp_bp1 <- max(tmp_set1$bp)
    tmp_bp2 <- min(tmp_set2$bp)
  } else if (nrow(tmp_set1) > 0) {
    tmp_bp2 <- max(tmp_set1$bp)
    tmp_set1 <-
      tmp_set1 %>%
      dplyr::filter(bp != tmp_bp2)
    tmp_bp1 <- max(tmp_set1$bp)
  } else {
    tmp_bp1 <- min(tmp_set2$bp)
    tmp_set2 <-
      tmp_set2 %>%
      dplyr::filter(bp != tmp_bp1)
    tmp_bp2 <- min(tmp_set2$bp)
  }

  if (tmp_bp1 == tmp_bp2) {
    res <-
      chromgpos %>%
      dplyr::filter(bp == tmp_bp1) %>%
      dplyr::pull(value)
    return(res)
  } else {

    len1 <-
      chromgpos %>%
      dplyr::filter(bp == tmp_bp1) %>%
      dplyr::pull(value)

    len2 <-
      chromgpos %>%
      dplyr::filter(bp == tmp_bp2) %>%
      dplyr::pull(value)

    res <- len1+(len2-len1)*(ibd_bp - tmp_bp1)/(tmp_bp2-tmp_bp1)
    return(res)
  }
}
