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

#' Read IBD segment file
#'
#' @description
#' Reads in the inferred IBD segments from `hapibd` ([documentation](https://github.com/browning-lab/hap-ibd#output-files)) or IBD segment file generated by `ped-sim` ([documentation](https://github.com/williamslab/ped-sim#output-ibd-segments-file)).
#'
#' If reading a `hapibd` segment file, the input data should have the following columns:
#'
#' 1. First sample identifier
#' 2. First sample haplotype index (1 or 2)
#' 3. Second sample identifier
#' 4. Second sample haplotype index (1 or 2)
#' 5. Chromosome
#' 6. Base coordinate of first marker in segment
#' 7. Base coordinate of last marker in segment
#' 8. cM length of IBD segment
#'
#' If read a `pedsim` segment file, the input data should have the following columns:
#'
#' 1. First sample identifier
#' 2. Second sample identifer
#' 3. Chromosome
#' 4. Physical position start
#' 5. Physical position end
#' 6. IBD type
#' 7. Genetic position start
#' 8. Genetic position end
#' 9. Genetic length (end - start)
#'
#' @param file Input file path
#' @param source Source of the input file; must be one of `"hapibd"` or `"pedsim"`
#'
#' @return if `source="hapibd"`, a tibble is returned.
#' If `source="pedsim"`, a list with two tibble elements, `IBD1` and `IBD2` is returned.
#' Both the `hapibd` tibble, and the two `pedsim` tibbles contain six columns:
#'
#' 1. id1 (sample identifier 1)
#' 2. id2 (sample identifier 2)
#' 3. chr (chromosome)
#' 4. start (segment bp start coordinate)
#' 5. end (segment bp end coordinate)
#' 6. length (shared segment length in genetic units, cM)
#'
#'
#' @references <https://github.com/browning-lab/hap-ibd#output-files>
#' @references <https://github.com/williamslab/ped-sim#output-ibd-segments-file>
#'
#' @examples
#' hapibd_fp <- system.file("extdata", "GBR.sim.ibd.gz", package="skater", mustWork=TRUE)
#' hapibd_seg <- read_ibd(hapibd_fp, source = "hapibd")
#' pedsim_fp <- system.file("extdata", "GBR.sim.seg.gz", package="skater", mustWork=TRUE)
#' pedsim_seg <- read_ibd(pedsim_fp, source = "pedsim")
#' @export
read_ibd <- function(file, source) {

  if(source == "hapibd") {

    ## Does the hap ibd input contain any lines?
    ind <- suppressWarnings(length(readr::read_delim(file, n_max=1L, delim = "\t", col_types = readr::cols())) > 0)

    ## nest all the hapibd IBD reading in a condition ...
    ## ... checks that hapibd file has data in it (i.e. at least 1 row)
    if(ind) {
      seg <-
        readr::read_delim(file,
                          delim = "\t",
                          col_names = FALSE,
                          col_types = "cdcddddd") %>%
        ## select columns by indices
        dplyr::select(1,3,5,6,7,8) %>%
        ## set names
        purrr::set_names(c("id1","id2","chr","start","end","length")) %>%
        ## make sure ids are ordered
        arrange_ids(id1,id2)

    } else {
      ## create an empty tibble for segments if the hapibd input is empty
      seg <-
        dplyr::tibble(id1 = character(),
                      id2 = character(),
                      chr = character(),
                      start = character(),
                      end = character(),
                      length = character())
      message("The hapibd input appears empty. Creating empty IBD tibble.")
    }
  } else if (source == "pedsim") {

    tmp <-
      readr::read_tsv(file,
                      col_types="ccciicddd",
                      col_names=c("id1", "id2", "chr", "pstart", "pend", "type", "gstart", "gend", "glength")) %>%
      dplyr::filter(type == "IBD1" | type == "IBD2")

    ibd1 <-
      tmp %>%
      dplyr::filter(type == "IBD1") %>%
      dplyr::select(id1, id2, chr, start = pstart, end = pend, length = glength) %>%
      ## make sure ids are ordered
      arrange_ids(id1,id2)

    ibd2 <-
      tmp %>%
      dplyr::filter(type == "IBD2") %>%
      dplyr::select(id1, id2, chr, start = pstart, end = pend, length = glength) %>%
      ## make sure ids are ordered
      arrange_ids(id1,id2)

    seg <- list(IBD1 = ibd1, IBD2 = ibd2)

  } else {
    stop("The 'source' argument must be one of 'hapibd' or 'pedsim'.")
  }

  return(seg)

}


#' Read genetic map file
#'
#' @description
#'
#' This function reads in the content from a genetic map file to translate physical distance to genetic units (i.e. cM). Regardless of the source, the input file must be sex-averaged and in a tab-separated "Plink" format ([documentation](<https://www.cog-genomics.org/plink/1.9/formats#map>)) with the following four columns and no header (i.e. no column names):
#'
#' 1. Chromosome
#' 2. Identifier (ignored in `read_map()`)
#' 3. Length (genetic length within the physical position boundary)
#' 4. Position (physical position boundary)
#'
#' The columns must be in the order above. Note that only the first, third, and fourth columns are used in the function.
#'
#' @details
#'
#' The genetic map could come from different sources. One source is the HapMap map distributed by the Browning Lab ([documentation](https://bochet.gcc.biostat.washington.edu/beagle/genetic_maps/)). If this map file is used, the non-sex chromosomes can be downloaded and concatenated to a single file as follows:
#'
#' ```
#' wget https://bochet.gcc.biostat.washington.edu/beagle/genetic_maps/plink.GRCh37.map.zip
#' unzip plink.GRCh37.map.zip
#' cat *chr[0-9]*GRCh37.map | sort -k1,1 -k4,4 --numeric-sort > plink.allchr.GRCh37.map
#' ```
#'
#' Another source is a sex-specific map ("bherer") originally published by Bherer et al and recommended by the developers of `ped-sim` for simulating IBD segments ([documentation](https://github.com/williamslab/ped-sim#map-file)). To retrieve and prep this map file for simulation:
#'
#' ```
#' # Get the refined genetic map and extract
#' wget --no-check-certificate https://github.com/cbherer/Bherer_etal_SexualDimorphismRecombination/raw/master/Refined_genetic_map_b37.tar.gz
#' tar xvfpz Refined_genetic_map_b37.tar.gz
#'
#' # Format for ped-sim as per https://github.com/williamslab/ped-sim#map-file-
#' printf "#chr\tpos\tmale_cM\tfemale_cM\n" > sexspec.pedsim.map
#' for chr in {1..22}; do
#'   paste Refined_genetic_map_b37/male_chr$chr.txt Refined_genetic_map_b37/female_chr$chr.txt \
#'     | awk -v OFS="\t" 'NR > 1 && $2 == $6 {print $1,$2,$4,$8}' \
#'     | sed 's/^chr//' >> sexspec.pedsim.map;
#' done
#'
#' # Clean up
#' rm -rf Refined_genetic_map_b37*
#' ```
#'
#' After this, the `sexspec.pedsim.map` file is ready for use in simulation. However, it must be averaged and reformatted to "Plink format" to use here:
#'
#' ```
#' cat sexspec.pedsim.map | grep -v "^#" | awk -v OFS="\t" '{print $1,".",($3+$4)/2,$2}' > sexspec-avg.plink.map
#' ```
#'
#'#' The genetic maps created above are in the tens of megabytes size range. This is trivial to store for most systems but a reduced version would increase portability and ease testing. This "minimum viable genetic map" could be used for testing and as installed package data in an R package for example analysis. Read more about minimum viable genetic maps at:
#'
#' - Blog post: <https://hapi-dna.org/2020/11/minimal-viable-genetic-maps/>
#' - Github repo with python code: <https://github.com/williamslab/min_map>
#'
#' The code as written below reduces the averaged sex-specific genetic map from 833776 to 28726 positions (~30X reduction!).
#'
#' ```
#' # Get minmap script from github
#' wget https://raw.githubusercontent.com/williamslab/min_map/main/min_map.py
#'
#' # Create empty minmap
#' echo -n > sexspec-avg-min.plink.map
#'
#' # For each autosome...
#' for chr in {1..22}; do
#'   echo "Working on chromosome $chr..."
#'   # First pull out just one chromosome
#'   grep "^${chr}[[:space:]]" sexspec-avg.plink.map > tmp.${chr}
#'   # Run the python script on that chromosome.
#'   # The genetic map column is 3rd column (2nd in 0-start). Physical position is last column (3 in 0-based)
#'   python3 min_map.py -mapfile tmp.${chr} -chr ${chr} -genetcol 2 -physcol 3 -noheader -error 0.05
#'   # Strip out the header and reformat back to plink format, and append to minmap file
#'   cat min_viable_map${chr}.txt | grep -v "^#" | awk -v OFS="\t" '{print $1,".",$4,$2}' >> sexspec-avg-min.plink.map
#'   # Clean up
#'   rm -f min_viable_map${chr}.txt tmp.${chr}
#' done
#' ```
#'
#' This averaged version of the Bherer sex-specific map, reduced to a minimum viable genetic map with at most 5% error, in Plink format, is available as installed package data (see examples). This is useful for testing code, but the full genetic map should be used for most analysis operations.
#'
#' @param file Input file path
#'
#' @return A tibble containing 3 columns:
#'
#' 1. chr (chromosome)
#' 2. value (genetic length within the physical position boundary)
#' 3. bp (physical position boundary)
#'
#' @references <https://www.cog-genomics.org/plink/1.9/formats#map>
#' @references <https://bochet.gcc.biostat.washington.edu/beagle/genetic_maps/>
#' @references <https://github.com/williamslab/ped-sim#map-file>
#' @references <https://www.nature.com/articles/ncomms14994>
#' @references <https://www.nature.com/articles/ncomms14994>
#' @references <https://github.com/cbherer/Bherer_etal_SexualDimorphismRecombination>
#'
#' @examples
#' gmapfile <- system.file("extdata", "sexspec-avg-min.plink.map", package="skater", mustWork=TRUE)
#' gmap <- read_map(gmapfile)
#'
#' @export
#'
read_map <- function(file) {

  gmap <-
    readr::read_delim(file,
                    delim = "\t",
                    col_names = FALSE,
                    col_types = "dcdd") %>%
    ## select column indices
    dplyr::select(1,3,4) %>%
    ## set column names
    purrr::set_names(c("chr", "value", "bp"))

  return(gmap)

}
