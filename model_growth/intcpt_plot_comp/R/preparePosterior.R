## Script to prepare MCMC sampling outputs to the MCMCdiagnostics report

##############################
# Steps:
  # get simulation variables and data
  # Extract posterior distribution of parameters
  # Extract diagnostics of MCMC run
  # Compute Loo
  # Save all outputs in RDS to optimize disk usage
##############################


library(data.table)
library(tidyverse)
library(cmdstanr)
library(loo)
library(posterior)


## Get simulation info

  # load info file
  model_dir <- 'model_growth/intcpt_plot_comp'
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

for(Sp in spIds[7:32])
{
  # load database
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  readRDS(
      gsub('../../', '', sim_info$dataSources)
    ) |>
    filter(species_id == Sp) |>
    setDT() ->
  dataSource

  # remove dead trees
  dataSource <- dataSource[status == 1]

  # remove observations with dbh lower than 127 mm
  dataSource <- dataSource[dbh >= 127]

  # get number of measurements by tree_id
  dataSource[, nbMeasure := .N, by = tree_id]

  # filter for tree_id with at least two measures
  dataSource <- dataSource[nbMeasure > 1]

  ## compute de deltaTime between measures of dbh
  dataSource[,
    deltaTime := year_measured - lag(year_measured, 1),
    by = tree_id
  ]

  ## define previous measure
  dataSource[,
    dbh0 := lag(dbh, 1),
    by = tree_id
  ]

  ## Fill the NA first measures with their non lag information
  dataSource[is.na(deltaTime), deltaTime := 0]
  dataSource[deltaTime == 0, dbh0 := dbh]

  dataSource |>
    as_tibble() |>
    drop_na() ->
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

  # load plot_id references
  toSub <- readRDS(
    file.path(
      sim_path,
      paste0('toSub_', Sp, '.RDS')
    )
  )

  # posterior of plot_id parameters
  saveRDS(
    params |>
      filter(grepl(pattern = 'rPlot_log', par)) |>
      mutate(plot_id_seq = parse_number(par)) |>
      left_join(toSub) |>
      select(!par),
    file = file.path(
      output_sim,
      paste0('posterior_plot_', Sp, '.RDS')
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



  ## Approximate LOO-CV using subsampling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Function to compute log-likelihood
  growth_model <- function(dt, post, log)
  {
    # dt is vector of:
    # [1] dbh,
    # [2] dbh0,
    # [3] deltaTime,
    # [4] plot_id_seq,
    # [5] BA_comp_sp,
    # [6] BA_comp_inter
    
    # post is a matrix of nrow draws and ncol paramaters

    rPlot = exp(
      post[, 'r'] +
      post[, paste0('rPlot_log[', dt[4], ']')] +
      post[, 'Beta'] * (dt[5] + post[, 'theta'] * dt[6])
    )

    rPlotTime = exp(-rPlot * dt[3])

    # mean
    Mean <- dt[2] * rPlotTime + post[, 'Lmax'] * (1 - rPlotTime)

    # likelihood
    dnorm(
      x = dt[1],
      mean = Mean,
      sd = post[, 'sigma_obs'],
      log = log
    )
  }

  llfun_growth <- function(data_i, draws, log = TRUE)
    apply(
      data_i,
      1,
      function(x, y)
        growth_model(
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
    llfun_growth, 
    log = FALSE, # relative_eff wants likelihood not log-likelihood values
    chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/4), 
    data = dataSource |>
              left_join(toSub) |>
              select(dbh, dbh0, deltaTime, plot_id_seq, BA_comp_sp, BA_comp_inter), 
    draws = post_dist_lg,
    cores = sim_info$nC
  )

  # Loo using x samples
  # I keep getting 'pointwise' fail because of NA while I don't have Na..
  # Using `try()` to get over this bug
  out_obj = 'try-error'
  while(out_obj == 'try-error') {
    loo_try <- try(
        loo::loo_subsample(
          llfun_growth,
          observations = floor(nrow(dataSource)/5), # take a subsample of size x
          cores = sim_info$nC,
          r_eff = r_eff, 
          draws = post_dist_lg,
          data = dataSource |>
                  left_join(toSub) |>
                  select(dbh, dbh0, deltaTime, plot_id_seq, BA_comp_sp, BA_comp_inter)
      )
    )

    out_obj <- class(loo_try)[1]
  }
  
  loo_obj <- loo_try

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
