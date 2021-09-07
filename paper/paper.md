---
title: "skater: An R package for SNP-based Kinship Analysis, Testing, and Evaluation"

tags:
  - R
  - genetics
  - kinship analysis
  - SNPs
  - sequencing


citation_author: Turner, S.D. et al.
  
authors:
  - name: Stephen D. Turner
    orcid: 0000-0001-9140-9028
    affiliation: "1"
  - name: V.P. Nagraj
    orcid: 0000-0003-0060-566X
    affiliation: "1"
  - name: Matthew Scholz
    orcid: 0000-0003-3686-1227
    affiliation: "1"
  - name: Shakeel Jessa
    affiliation: "1"
  - name: Carlos Acevedo
    affiliation: "1"
  - name: Jianye Ge
    orcid: 0000-0001-8724-075X
    affiliation: "2,3"
  - name: August E. Woerner
    orcid: 0000-0002-9372-1127
    affiliation: "2,3"
  - name: Bruce Budowle
    orcid: 0000-0003-4116-2930
    affiliation: "2,3"

affiliations:
  - name: Signature Science, LLC., Austin, TX 78759, USA.
    index: 1
  - name: Center for Human Identification, University of North Texas Health Science Center, Fort Worth, TX 76107, USA
    index: 2
  - name: Department of Microbiology, Immunology, and Genetics, University of North Texas Health Science Center, Fort Worth, TX 76107, USA
    index: 3
 
date: 07 September 2021
year: 2021
bibliography: bibliography.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS

---



# Summary

**Motivation:** SNP-based kinship analysis with genome-wide relationship estimation and IBD segment analysis methods produces results that often require further downstream processing and manipulation. A dedicated software package that consistently and intuitively implements this analysis functionality is needed.  
**Results:**  Here we present the skater R package for **S**NP-based **k**inship **a**nalysis, **t**esting, and **e**valuation with **R**. The skater package contains a suite of well-documented tools for importing, parsing, and analyzing pedigree data, performing relationship degree inference, benchmarking relationship degree classification, and summarizing IBD segment data.  
**Availability:** The skater package is implemented as an R package and is released under the MIT license at https\:\/\/github.com/signaturescience/skater. Documentation is available at https\:\/\/signaturescience.github.io/skater.

# Introduction

Inferring familial relationships between individuals using genetic data is a common practice in population genetics, medical genetics, and forensics. There are multiple approaches to estimating relatedness between samples, including genome-wide measures, such as those implemented in Plink [@purcell2007] or KING [@manichaikul2010], and methods that rely on identity by descent (IBD) segment detection, such as GERMLINE [@gusev2009], hap-IBD [@zhou2020], and IBIS [@seidman2020]. Recent efforts focusing on benchmarking these methods [@ramstetter2017] have been aided by tools for simulating pedigrees and genome-wide SNP data [@caballero2019]. Analyzing results from genome-wide SNP-based kinship analysis or comparing analyses to simulated data for benchmarking have to this point required writing one-off analysis functions or utility scripts that are seldom distributed with robust documentation, test suites, or narrative examples of usage. There is a need in the field for a well-documented software package with a consistent design and API that contains functions to assist with downstream manipulation, benchmarking, and analysis of SNP-based kinship assessment methods. Here we present the skater package for **S**NP-based **k**inship **a**nalysis, **t**esting, and **e**valuation with **R**.

# The skater package

The skater package provides an intuitive collection of analysis and utility functions for SNP-based kinship analysis. Functions in the package include tools for importing, parsing, and analyzing pedigree data, performing relationship degree inference, benchmarking relationship degree classification, and summarizing IBD segment data. The package is designed to adhere to "tidy" data analysis principles, and builds upon the tools released under the tidyverse R ecosystem [@Wickham2019].

## Pedigree parsing, manipulation, and analysis

The skater package has several functions for importing, parsing, and analyzing pedigree data. Pedigrees define familial relationships in a hierarchical structure. Many genomics tools for working with pedigrees start with a .fam file, which is a tabular format with one row per individual and columns for unique IDs of the mother, father, and the family unit. The skater package contains the function `read_fam()` to read in a PLINK-formatted .fam file and another function `fam2ped()` to convert the content into a pedigree object as a nested tibble with one row per family. All pedigree processing from skater internally leverages a data structure from the kinship2 package [@sinnwell2014]. Further functions such as `plot_pedigree()` produce a multi-page PDF drawing a diagram of the pedigree for each family, while `ped2kinpair()` produces a pairwise list of relationships between all individuals in the data with the expected kinship coefficients for each pair (see skater package vignette).

## Relationship degree inference and benchmarking

The skater package includes functions to translate kinship coefficients to relationship degrees. The kinship coefficients could come from `ped2kinpair()` or other kinship estimation software.

The `dibble()` function creates a **d**egree **i**nference t**ibble**, with degrees up to the specified maximum degree resolution, expected kinship coefficient, and lower and upper inference ranges as defined in @manichaikul2010. The `kin2degree()` function infers the relationship degree given a kinship coefficient and a maximum degree resolution (e.g., 7th-degree relatives) up to which anything more distant is classified as unrelated.

Once estimated kinship is converted to degree, it may be of interest to compare the inferred degree to known degrees of relatedness. When aggregated over many relationships and inferences, this can help benchmark performance of a particular kinship analysis method. The skater package adapts a `confusion_matrix()` function from @clark2021 to provide standard contingency table metrics (e.g. sensitivity, specificity, PPV, precision, recall, F1, etc.) with a new reciprocal RMSE (R-RMSE) metric. The R-RMSE metric is defined more thoroughly in the skater package vignette and may be a preferable measure of classification accuracy when benchmarking relationship degree estimation. In many kinship benchmarking analyses, classification error is treated in a categorical manner (exact match plus or minus one degree), neglecting the true amount of sharing as a real number. Taking the reciprocal of the target and predicted degree in a typical RMSE calculation results in larger penalties for more egregious misclassifications (e.g., classifying a first-degree relative pair as second-degree) than misclassifications at more distant relationships (e.g., classifying a fourth-degree relative pair as fifth-degree).

## IBD segment analysis

Tools such as hap-IBD [@zhou2020], and IBIS [@seidman2020] detect shared IBD segments between individuals. The skater package includes functionality to take those IBD segments, compute shared genomic centimorgan (cM) length, and converts that shared cM to a kinship coefficient. In addition to inferred segments, these functions can estimate "truth" kinship from simulated IBD segments [@caballero2019]. The `read_ibd()` function reads pairwise IBD segments from IBD inference tools and from simulated IBD segments. The `read_map()` function reads in genetic map in a standard format which is required to translate the total centimorgans shared IBD to a kinship coefficient using the `ibd2kin()` function.

# Conclusion

The skater R package provides a robust software package for data import, manipulation, and analysis tasks typically encountered when working with SNP-based kinship analysis tools. All package functions are internally documented with examples, and the package contains a vignette demonstrating usage, inputs, outputs, and interpretation of all key functions. The package contains internal tests that are automatically run with continuous integration via GitHub Actions whenever the package code is updated. The skater package is permissively licensed (MIT) and is easily extensible to accommodate outputs from new genome-wide relatedness and IBD segment methods as they become available.

<!-- # Acknowledgements {.unnumbered} -->

<!-- These should be included at the end of the text and not in footnotes. Please ensure you acknowledge all sources of funding, see funding section below. -->

<!-- Details of all funding sources for the work in question should be given in a separate section entitled 'Funding'. This should appear before the 'Acknowledgements' section. -->

# Acknowledgements

This work was supported in part by award 2019-DU-BX-0046 (Dense DNA Data for Enhanced Missing Persons Identification) to B.B., awarded by the National Institute of Justice, Office of Justice Programs, U.S. Department of Justice and by internal funds from the Center for Human Identification. The opinions, findings, and conclusions or recommendations expressed are those of the authors and do not necessarily reflect those of the U.S. Department of Justice.

<!-- -   The sentence should begin: 'This work was supported by ...' - -->

<!-- -   The full official funding agency name should be given, i.e. 'National Institutes of Health', not 'NIH' (full RIN-approved list of UK funding agencies) -->

<!-- -   Grant numbers should be given in brackets as follows: '[grant number xxxx]' -->

<!-- -   Multiple grant numbers should be separated by a comma as follows: '[grant numbers xxxx, yyyy]' -->

<!-- -   Agencies should be separated by a semi-colon (plus 'and' before the last funding agency) -->

<!-- -   Where individuals need to be specified for certain sources of funding the following text should be added after the relevant agency or grant number 'to [author initials]'. -->

<!-- An example is given here: 'This work was supported by the National Institutes of Health [AA123456 to C.S., BB765432 to M.H.]; and the Alcohol & Education Research Council [hfygr667789].' -->

<!-- Oxford Journals will deposit all NIH-funded articles in PubMed Central. See Depositing articles in repositories -- information for authors for details. Authors must ensure that manuscripts are clearly indicated as NIH-funded using the guidelines above. -->

# References