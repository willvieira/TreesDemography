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
  model_dir <- 'model_recruitment/intcpt'
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
    filter(!is.na(plot_id)) |>
    filter(!is.na(BA_adult)) ->
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
    as_draws_matrix() |>
    as_tibble() |>
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
    params,# |>
      #filter(!grepl(pattern = '\\[', par)),
    file = file.path(
      output_sim,
      paste0('posterior_pop_', Sp, '.RDS')
    )
  )

  # # posterior of plot_id parameters
  # saveRDS(
  #   post_dist |>
  #     filter(grepl(pattern = 'mPlot', par)),
  #   file = file.path(
  #     output_sim,
  #     paste0('posterior_plot_', Sp, '.RDS')
  #   )
  # )

  # save sampling diagnostics
  saveRDS(
    diag_out,
    file = file.path(
      output_sim,
      paste0('diagnostics_', Sp, '.RDS')
    )
  )



  ## Approximate LOO-CV using subsampling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Function to compute log-likelihood
  recruit_model <- function(dt, post, log)
  {
    # dt is vector of:
    # [1] nbRecruit,
    # [2] plot_size,
    # [3] deltaTime
    
    # post is a matrix of nrow draws and ncol paramaters

    m = exp(
      post[, 'm_log']
    )
    p = exp(
      -exp(
        post[, 'p_log']
      )
    )

    # mean
    Mean <- m * dt[2] * (1 - p^dt[3]) / (1 - p)

    # likelihood
    dpois(
      x = dt[1],
      lambda = Mean,
      log = log
    )
  }

  llfun_recruit <- function(data_i, draws, log = TRUE)
    apply(
      data_i,
      1,
      function(x, y)
        recruit_model(
          x,
          y,
          log = log
        ),
        y = draws
    )

  # Matrix of posterior draws
  post_dist_lg <- params |>
    pivot_wider(
      names_from = par,
      values_from = value
    ) |>
    select(-iter) |>
    as.matrix()
  
  # compute relative efficiency
  # this is slow and optional but is recommended to allow for adjusting PSIS
  # effective sample size based on MCMC effective sample size)
  r_eff <- loo::relative_eff(
    llfun_recruit, 
    log = FALSE, # relative_eff wants likelihood not log-likelihood values
    chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/2), 
    data = dataSource |>
              select(nbRecruit, plot_size, deltaYear_plot), 
    draws = post_dist_lg,
    cores = sim_info$nC
  )

  # Loo using x samples
  loo_obj <-
    loo::loo_subsample(
      llfun_recruit,
      observations = floor(nrow(dataSource)/5), # take a subsample of size x
      cores = sim_info$nC,
      r_eff = r_eff, 
      draws = post_dist_lg,
      data = dataSource |>
              select(nbRecruit, plot_size, deltaYear_plot)
  )

  # save Loo
  saveRDS(
    loo_obj,
      file = file.path(
      output_sim,
      paste0('loo_', Sp, '.RDS')
    )
  )

  cat(' Species', which(Sp == spIds), 'of', length(spIds), '\n')
}
