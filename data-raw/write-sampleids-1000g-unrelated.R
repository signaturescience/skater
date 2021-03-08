library(tidyverse)
unrel_path <- here::here("inst/extdata/sampleids-1000g-unrelated")
dir.create(unrel_path, showWarnings = FALSE)
ped1kg_unrel <- skater:::ped1kg_unrel
for(pop in unique(ped1kg_unrel$population)) {
  unrelated_ids <- ped1kg_unrel %>% filter(population==pop) %>% pull(id)
  unrelated_ids_path <- paste0(file.path(unrel_path, pop), ".txt")
  message(glue::glue("Writing {length(unrelated_ids)} to path: {unrelated_ids_path}"))
  write_lines(unrelated_ids, unrelated_ids_path)
}
