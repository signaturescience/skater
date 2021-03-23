#' Read PLINK-formatted .fam file
#'
#' @description
#' Reads in a [PLINK-formatted .fam file](https://www.cog-genomics.org/plink/1.9/formats#fam). Input `file` must have six columns:
#' 1. Family ID
#' 2. Individual ID
#' 3. Father ID
#' 4. Mother ID
#' 5. Sex
#' 6. Affected Status
#'
#' @param file Input file path
#'
#' @return A tibble containing the 6 columns from the fam file.
#'
#' @examples
#' famfile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
#' fam <- read_fam(famfile)
#' fam
#'
#' @export
read_fam <- function(file) {
  readr::read_delim(file,
                    delim=" ",
                    col_names=c("fid", "id", "dadid", "momid", "sex", "affected"),
                    col_types="ccccii")
}


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
#' @param file Input file path
#'
#' @return A tibble containing the 7 columns from the akt file.
#'
#' @examples
#' aktFile <- system.file("extdata", "3gens.fam", package="skater", mustWork=TRUE)
#' akt <- read_fam(aktFile)
#' akt
#'
#' @export
read_akt <- function(file){
  readr::read_table2(
  file,
  col_names=c("id1","id2","IBD0","IBD1","IBD_2","k_akt","Markers"),
  col_types="ccddddi")
}
