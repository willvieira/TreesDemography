## Script to prepare MCMC sampling outputs to the MCMCdiagnostics report

##############################
# Steps:
  # get simulation variables and data
  # Extract posterior distribution of parameters
  # Extract diagnostics of MCMC run
  # Compute Loo
  # Save all outputs in RDS to optimize disk usage
##############################


library(tidyverse)
library(cmdstanr)
library(loo)
library(posterior)


## Get simulation info

  # load info file
  model_dir <- 'model_sizeIngrowth/time_truc'
  sim_info <- yaml::read_yaml(file.path(model_dir, '_simulation_info.yml'))

  # species_ids
  spIds <- sim_info$spIds


  # create dir to save species raw sampling output
  output_sim <- file.path('output_sim_processed', sim_info$vitalRates, sim_info$simName)
  dir.create(output_sim, recursive = TRUE)

  # raw simulation draws
  sim_path <- file.path('output_sim', sim_info$vitalRates, sim_info$simName)

##


## Loop over species

for(Sp in spIds)
{
  # load database
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  readRDS(
      gsub('../../', '', sim_info$dataSources)
    ) |>
    filter(species_id == Sp) |>
    filter(dbh < 500) ->
  dataSource


  # load parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  md_out <- as_cmdstan_fit(
    dir(
      file.path(sim_path, Sp),
      full.names = TRUE
    )
  )

  # extract sample diagnostics
  diag_out <- list(
    diag_summary = md_out$diagnostic_summary(),
    rhat = md_out$summary() |> select(variable, rhat),
    time = md_out$time()
  )

  md_out <- read_cmdstan_csv(
    dir(
      file.path(sim_path, Sp),
      full.names = TRUE
    )
  )
  
  md_out$post_warmup_draws |>
    as_draws_df() |>
    as_tibble() |>
    # keep only the last 1k iteration per chain
    filter(.iteration %in% 1001:2000) |>
    select(!contains('.')) |>
    mutate(iter = row_number()) |>
    pivot_longer(
      cols = !iter,
      names_to = 'par'
    ) ->
  params



  ## save output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # posterior of population level parameters
  saveRDS(
    params |>
      filter(!grepl(pattern = '\\[', par)),
    file = file.path(
      output_sim,
      paste0('posterior_pop_', Sp, '.RDS')
    )
  )


  # save sampling diagnostics
  saveRDS(
    diag_out,
    file = file.path(
      output_sim,
      paste0('diagnostics_', Sp, '.RDS')
    )
  )

  cat(' Species', which(Sp == spIds), 'of', length(spIds), '\n')
}
