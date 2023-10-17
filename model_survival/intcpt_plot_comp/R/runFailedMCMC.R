# Script to query failed species and rerun batch only for the missing species

library(tidyverse)

# load info file
sim_info <- yaml::read_yaml('_simulation_info.yml')
spIds <- sim_info$spIds

# get total number of iteration among chains to compare with
# the length of the output csv file
total_iteration <- sim_info$maxIter/2 * sim_info$nC

# As of stan version 2.30.1, each chain csv file produces
# 55 lines of metadata
# N iteration lines of samples
expected_lines <- total_iteration + 55 * sim_info$nC


# function to get total number of lines among all csv files produced
csv_nbLines <- function(path)
{
  csv_files <- list.files(path, pattern = 'csv', full.names = TRUE)

  wc_out <- system(
    paste0(
      'wc -l ',
      paste0(csv_files, collapse = ' ')
    ),
    intern = TRUE
  )

   wc_out |>
    tail(n = 1) |>
    parse_number()
}

# get species with wrong number of iterations
tibble(
    species_id = spIds,
  ) |>
  mutate(path = paste0('output/', species_id)) |>
  rowwise() |>
  mutate(nbLines = csv_nbLines(path)) |>
  filter(nbLines != expected_lines) |>
  pull(species_id) ->
species_to_run


if(length(species_to_run) > 0) {

  # remove sampled files for failed species
  file.remove(dir(paste0('output/', species_to_run), full.names = TRUE))


  # edit runMCMC R script to change seed
  mcmc_r <- readLines(
    paste0(
      'R/', sim_info$vitalRates, 'MCMC.R'
    )
  )

  # change seed number
  mcmc_r[grep('set.seed', mcmc_r)] <- 'set.seed(1.0)'

  # save R script file
  writeLines(
    mcmc_r,
    paste0(
      'R/', sim_info$vitalRates, 'MCMC.R'
    )
  )

  # edit submission batch with failed species
  sub_file <- readLines(
    paste0(
      'sub_', sim_info$vitalRates, '.sh'
    )
  )

  # change line with array IDs to insert the vector with only failed species
  sub_file[grep('--array', sub_file)] <- paste0(
    '#SBATCH --array=',
    paste0(which(spIds %in% species_to_run), collapse = ',')
  )

  # save file 
  writeLines(
    sub_file,
    paste0(
      'sub_', sim_info$vitalRates, '.sh'
    )
  )

  # submit batch
  system(
    paste0('sbatch sub_', sim_info$vitalRates, '.sh')
  )
}else{
  print('No failed species found.')
}
