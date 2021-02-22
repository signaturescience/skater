library(tidyverse)

## 1000 Genomes pedigree

# Read ped file from 1kg FTP
ped1kg <- read_tsv("http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/integrated_call_samples_v3.20200731.ALL.ped", guess_max = 4000)
# Clean names
ped1kg <- ped1kg %>% janitor::clean_names()
# make first 6 names same as result from read_fam
names(ped1kg)[1:6] <- c("fid", "id", "dadid", "momid", "sex", "affected")
# limit to 2504 with sequencing data
ped1kg <- ped1kg %>% filter(phase_3_genotypes %>% near(1))
# Remove unneeded columns
ped1kg <- ped1kg %>% select(-phase_3_genotypes, -related_genotypes, -omni_genotypes, -affy_genotypes)

# Get all samples unrelated to anyone
ped1kg_unrel <-
  ped1kg %>%
  filter(dadid=="0") %>%
  filter(momid=="0") %>%
  filter(siblings=="0") %>%
  filter(second_order=="0") %>%
  filter(third_order=="0") %>%
  filter(children=="0")

usethis::use_data(ped1kg, ped1kg_unrel, internal = TRUE, overwrite = TRUE)
