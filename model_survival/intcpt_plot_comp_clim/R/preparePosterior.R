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
  model_dir <- 'model_survival/intcpt_plot_comp_clim'
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
    filter(dbh0 >= 127) ->
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
      filter(grepl(pattern = 'psiPlot', par)) |>
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
  mort_model <- function(dt, post, log)
  {
    # dt is vector of:
    # [1] state1,
    # [2] deltaTime,
    # [3] plot_id_seq,
    # [5] BA_comp_sp,
    # [6] BA_comp_inter,
    # [7] bio_01_mean_scl,
    # [8] bio_12_mean_scl
    
    # post is a matrix of nrow draws and ncol paramaters

    # fixed effects
    longev_log <- 1/(1 + exp(
        -(
          post[, 'psi'] +
          post[, paste0('psiPlot[', dt[3], ']')] +
          post[, 'Beta'] * (dt[4] + post[, 'theta'] * dt[5]) +
          -post[, 'tau_temp'] * (dt[6] - post[, 'optimal_temp'])^2 +
          -post[, 'tau_prec'] * (dt[7] - post[, 'optimal_prec'])^2
        )
      )
    )

    # time component of the model
    mortality_time <- 1 - (longev_log^dt[2])

    # deal with numerical instability
    if(any(mortality_time == 1))
      mortality_time[which(mortality_time == 1)] = 0.9999999

    # likelihood
    dbinom(
      x = dt[1],
      size = 1,
      prob = mortality_time,
      log = log
    )
  }

  llfun_mort <- function(data_i, draws, log = TRUE)
    apply(
      data_i,
      1,
      function(x, y)
        mort_model(
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
    llfun_mort, 
    log = FALSE, # relative_eff wants likelihood not log-likelihood values
    chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/4), 
    data = dataSource |>
              left_join(toSub) |>
              select(mort, deltaYear, plot_id_seq, BA_comp_sp, BA_comp_inter, bio_01_mean_scl, bio_12_mean_scl),
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
        llfun_mort,
        observations = nrow(dataSource), # take a subsample of size x
        cores = sim_info$nC + 8,
        r_eff = r_eff, 
        draws = post_dist_lg,
        data = dataSource |>
                left_join(toSub) |>
                select(mort, deltaYear, plot_id_seq, BA_comp_sp, BA_comp_inter, bio_01_mean_scl, bio_12_mean_scl)
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
