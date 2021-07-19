library(tidyverse)

# Read 1000Gped file from 1kg FTP
ped1kg <- read_tsv("http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/integrated_call_samples_v3.20200731.ALL.ped", guess_max = 4000)
# Clean names
ped1kg <- ped1kg %>% janitor::clean_names()
# make first 6 names same as result from read_fam
names(ped1kg)[1:6] <- c("fid", "id", "dadid", "momid", "sex", "affected")
# limit to 2504 with sequencing data
ped1kg <- ped1kg %>% filter(phase_3_genotypes %>% near(1))
# Remove unneeded columns
ped1kg <- ped1kg %>% select(-phase_3_genotypes, -related_genotypes, -omni_genotypes, -affy_genotypes)

# Who are the relatives in the ped file (e.g., mom, dad, sibs, 2d, 3d, and children)?
relatives <-
  with(ped1kg, c(dadid, momid, siblings, second_order, third_order, children)) %>%
  strsplit(",") %>%
  unlist() %>%
  grep("0", ., invert=TRUE, value=TRUE) %>%
  unique() %>%
  print()

# See how many individual IDs are any of the relatives to any other individual ID:
ped1kg %>% filter(id %in% relatives)
# # A tibble: 2 x 13
#   fid     id      dadid momid   sex affected population relationship siblings second_order third_order children other_comments
#   <chr>   <chr>   <chr> <chr> <dbl>    <dbl> <chr>      <chr>        <chr>    <chr>        <chr>       <chr>    <chr>
# 1 LWK001  NA19331 0     0         1        0 LWK        unrel        0        0            NA19334     NA19313  Parent/Child directionality is uncertain
# 2 NA19334 NA19334 0     0         1        0 LWK        unrel        0        NA19313      NA19331     0        0

# Get all samples unrelated to anyone who also has sequencing data
ped1kg_unrel <- ped1kg %>% filter(!(id %in% relatives))

## NOTE: use_data commented out because we don't need to save this data for now ... uncomment if needed
# save objects as internal sysdata.rda
#usethis::use_data(ped1kg, ped1kg_unrel, internal = TRUE, overwrite = TRUE)

