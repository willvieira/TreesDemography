## Script to run recruitment bayesian model

##############################
# Steps:
  # get simulation variables and data
  # Subset
  # Run the model
  # Save model output
##############################


library(data.table)
library(cmdstanr)
library(loo)
library(tidyverse)

set.seed(0.0)



## Get simulation info

  # array (species)
  array_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

  # load info file
  sim_info <- yaml::read_yaml('_simulation_info.yml')

  # species_ids
  sp <- sim_info$spIds[array_id]
  
  # sample size
  sampleSize <- sim_info$sampleSize

  # load database
  dataSource <- readRDS(
    sim_info$dataSources[
      which(sim_info$vitalRates == 'recruit')
    ]
  )

##



## stratified sampling to define traning and validation data

# select the species
recruit_dt <- dataSource[species_id == sp]

# remove NA plots
recruit_dt <- recruit_dt[!is.na(plot_id)]

# remove NA Basal area
recruit_dt <- recruit_dt[!is.na(BA_adult)]

# if(recruit_dt[, .N] > sampleSize)
# { 
#     # number of measurement by plot_id
#     recruit_dt[, nbMeasure := length(unique(year0)) + 1, by = plot_id]
    
#     # number of obs per number of measurement
#     nbObs <- recruit_dt[, .N, by = nbMeasure][order(nbMeasure, decreasing = TRUE)]
    
#     # define filter of nbMeasurement
#     max_nbMeasure <- nbObs[cumsum(N) > sampleSize, max(nbMeasure)]

#     # select plots to keep
#     recruit_dt[nbMeasure > max_nbMeasure, sampled := 'training']

#     # In case the selected plots with higher number of measurement is not
#     # enougth to `sampleSize`, complete the sampled list by selecting plots
#     # with `nbMeasure` lower than the `max_nbMeasure` threshold
#     if(recruit_dt[sampled == 'training', .N] < sampleSize)
#     {
#         # define how many obs are still needed to reach `sampleSize`
#         nMissing <- sampleSize - recruit_dt[sampled == 'training', .N]
        
#         # list the number of obs per plot_it that were not selected yet
#         nbObs <- recruit_dt[
#             nbMeasure > (max_nbMeasure - 1) & is.na(sampled),
#             .N,
#             by = plot_id
#         ]

#         # sample random available plot up to reach `sampleSize`
#         nbObs[, sampled := FALSE]
#         keepSampling <- TRUE
        
#         while(keepSampling) {

#             # sample random plot
#             selected_plot <- nbObs[sampled == FALSE, sample(plot_id, 1)]
#             nbObs[plot_id == selected_plot, sampled := TRUE]

#             keepSampling <- ifelse(
#                 nbObs[sampled == TRUE, sum(N)] < nMissing,
#                 TRUE, FALSE
#             )
            
#             # make sure there are enough plots to sample
#             if(keepSampling)
#                 if(nbObs[sampled == FALSE, .N] < 1)
#                     keepSampling <- FALSE
#         }

#         recruit_dt[
#             plot_id %in% nbObs[sampled == TRUE, plot_id],
#             sampled := 'training'
#         ]
#     }

#     # what is not sampled, will be validation data
#     recruit_dt[is.na(sampled), sampled := 'validation']
# }

##



## define plot_id in sequence to be used in stan

  plot_id_uq <- recruit_dt[, unique(plot_id)]
  toSub <- data.table(
    plot_id = plot_id_uq,
    plot_id_seq = 1:length(plot_id_uq)
  )

  recruit_dt[
    toSub,
    plot_id_seq := i.plot_id_seq,
    on = 'plot_id'
  ]

##



## run the model

  stanModel <- cmdstan_model('stan/recruit.stan')

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = recruit_dt[, .N],
          nbRecruit = recruit_dt[, nbRecruit],
          plot_size = recruit_dt[, plot_size],
          deltaTime = recruit_dt[, deltaYear_plot],
          BA_adult_sp = recruit_dt[, BA_adult_sp],
          BA_adult = recruit_dt[, BA_adult],
          Np = recruit_dt[, length(unique(plot_id_seq))],
          plot_id = recruit_dt[, plot_id_seq]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2
  )

  # extract posterior distribution
  post_dist <- md_out$draws(format = 'df') |>
    select(!c('lp__', '.chain', '.iteration', '.draw')) |>
    pivot_longer(
      cols = everything(),
      names_to = 'par',
      values_to = 'value'
    ) |>
    group_by(par) |>
    mutate(iter = row_number()) |>
    ungroup()

  # extract sample diagnostics
  diag_out <- list(
    diag_summary = md_out$diagnostic_summary(),
    rhat = md_out$summary() |> select(variable, rhat),
    time = md_out$time()
  )

##




## save output

  # posterior of population level parameters
  saveRDS(
    post_dist |>
      filter(par %in% c('mPop_log', 'p', 'sigma_plot', 'beta')),
    file = file.path(
      'output',
      paste0('posteriorPop_', sp, '.RDS')
    )
  )

  # posterior of plot_id parameters
  saveRDS(
    post_dist |>
      filter(grepl(pattern = 'mPlot_log', par)),
    file = file.path(
      'output',
      paste0('posteriormPlot_', sp, '.RDS')
    )
  )

  # save sampling diagnostics
  saveRDS(
    diag_out,
    file = file.path(
      'output',
      paste0('diagnostics_', sp, '.RDS')
    )
  )

  # save train and validate data
  saveRDS(
    toSub,
      file = file.path(
      'output',
      paste0('toSub_', sp, '.RDS')
    )
  )

##



## Approximate LOO-CV using subsampling

  # Function to compute log-likelihood
  recruit_model <- function(dt, post, log)
  {
    # dt is vector of [1] nbRecruit, [2] plot_size, [3] deltaTime,
    # [4] plot_id_seq, [5] BA_adult_sp, and [6] BA_adult
    # post is a matrix of nrow draws and ncol paramaters
  
    # Add plot_id random effect 
    mPlot_log <- post[, paste0('mPlot_log[', dt[4], ']')]
    mPlot <- exp(
      post[, 'mPop_log'] + mPlot_log + dt[5] * post[, 'beta_m']
    )
    
    p <- exp(
      -exp(
        post[, 'p_log']
      ) +
      dt[6]^2 * 1/2 * -post[, 'beta_p']^2
    )

    # mean
    Mean <- mPlot * dt[2] * (1 - p^dt[3]) / (1 - p)

    if(any(Mean < 0))
      cat(sum(Mean < 0), ' negative values!!\n')
      
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
  post_dist_lg <- post_dist |>
    pivot_wider(
      names_from = par,
      values_from = value
    ) |>
    select(-iter) |>
    as.matrix()

  # compute relative efficiency
  # this is slow and optional but is recommended to allow for adjusting PSIS
  # effective sample size based on MCMC effective sample size)
  r_eff <- relative_eff(
    llfun_recruit, 
    log = FALSE, # relative_eff wants likelihood not log-likelihood values
    chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/2), 
    data = recruit_dt[, .(nbRecruit, plot_size, deltaYear_plot, plot_id_seq, BA_adult_sp, BA_adult)], 
    draws = post_dist_lg,
    cores = sim_info$nC
  )

  # Loo using x samples
  loo_obj <-
    loo_subsample(
      llfun_recruit,
      observations = recruit_dt[, floor(.N/10)], # take a subsample of size x
      cores = sim_info$nC,
      r_eff = r_eff, 
      draws = post_dist_lg,
      data = recruit_dt[, .(nbRecruit, plot_size, deltaYear_plot, plot_id_seq, BA_adult_sp, BA_adult)]
  )

  # save Loo
  saveRDS(
    loo_obj,
      file = file.path(
      'output',
      paste0('loo_', sp, '.RDS')
    )
  )

##
