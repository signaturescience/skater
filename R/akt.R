#' Read akt kin output file
#'
#' @description
#' Reads in an [AKT kin formatted file (.akt)](https://illumina.github.io/akt/#kin). Input `file` must have seven columns:
#' 1. id1 (member 1)
#' 2. id2 (member 2)
#' 3. IBD0 (ratio of IBD0/All SNPS)
#' 4. IBD1 (ratio of IBD1/All SNPS)
#  5. IBD2 (ratio of IBD2/All SNPS)
#' 5. Kinship Coefficient
#' 6. NSNPS
#'
#' @param filename Input file path
#'
#' @return A tibble containing the 7 columns from the akt file.
#'
#' @examples
#' aktFile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
#' akt <- read_fam(aktFile)
#' akt
#'
#' @export

read_akt <- function(filename){
  readr::read_table2(
  filename,  
  col_names=c("id1","id2","IBD0","IBD1","IBD_2","k_akt","Markers"), 
  col_types="ccddddi")
}
