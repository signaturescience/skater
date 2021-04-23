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


#' Read AKT kin output file
#'
#' @description
#' Reads in an `akt kin` [results file](https://illumina.github.io/akt/#kin). Input `file` must have seven columns, whitespace delimited:
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
#' aktFile <- system.file("extdata", "3gens.akt", package="skater", mustWork=TRUE)
#' akt <- read_akt(aktFile)
#' akt
#'
#' @export
read_akt <- function(file) {
  readr::read_table2(
    file,
    col_names=c("id1","id2","ibd0","ibd1","ibd2","k","nsnps"),
    col_types="ccddddi"
  ) %>%
    arrange_ids(id1, id2)
}


#' Read IBIS coef output file
#'
#' @description
#' Reads in an `ibis` [results file](https://github.com/williamslab/ibis). Input `file` must have six columns, whitespace delimited:
#' 1. id1 (member 1)
#' 2. id2 (member 2)
#' 3. Kinship Coefficient
#' 4. IBD2 (ratio of IBD2/All SNPS)
#' 5. Segment count
#' 6. Kinship Degree
#'
#' @param file Input file path
#'
#' @return A tibble containing the 6 columns from the ibis file.
#'
#' @examples
#' ibisFile <- system.file("extdata", "3gens.ibis.coef", package="skater", mustWork=TRUE)
#' ibis <- read_ibis(ibisFile)
#' ibis
#'
#' @export
read_ibis <- function(file){
  readr::read_table2(file , col_names=c("id1", "id2", "k","ibd2","segment_count", "degree"), col_types="ccddii", skip = 1) %>%
    dplyr::mutate(id1=gsub(":","_", id1)) %>%
    dplyr::mutate(id2=gsub(":","_", id2)) %>%
    arrange_ids(id1, id2)
}

#' Read PLINK KING table
#'
#' @description
#' Reads in the output from `plink2 --make-king-table` ([documentation](https://www.cog-genomics.org/plink/2.0/distance#make_king)).
#' Input `file` must have six columns, tab delimited:
#' 1. id1 (member 1)
#' 2. id2 (member 2)
#' 3. nsnps
#' 4. hethet: proportion of sites where both are heterozygous
#  5. ibd0: proportion of sites where individuals are opposite homozygotes
#' 6. k: Kinship Coefficient
#'
#' @param file Input file path
#'
#' @return A tibble containing the 6 columns from the `plink2 --make-king-table` output.
#'
#' @references <https://www.cog-genomics.org/plink/2.0/distance#make_king>
#'
#' @examples
#' plink2kingFile <- system.file("extdata", "plink2-king-table.tsv", package="skater", mustWork=TRUE)
#' plink2king <- read_plink2_king(plink2kingFile)
#' plink2king
#' plink2king %>% dplyr::filter(k>0.01)
#'
#' @export
read_plink2_king <- function(file) {
  readr::read_tsv(
    file,
    col_names=c("id1","id2","nsnps","hethet","ibd0","k"),
    col_types="cciddd",
    skip=1L
  ) %>%
    arrange_ids(id1, id2)
}
