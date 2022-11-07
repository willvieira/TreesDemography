## Script to run growth bayesian model

##############################
# Steps:
  # get simulation variables and data
  # Subset 
  # scale temperature and precipitation and save parameters
  # Run the model
  # Save model output
##############################


library(data.table)
library(cmdstanr)
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
      which(sim_info$vitalRates == 'growth')
    ]
  )

##



## stratified sampling to define traning and validation data

  # select the species
  growth_dt <- dataSource[species_id == sp]

  # remove dead trees
  growth_dt <- growth_dt[status == 1]
  
  # get number of measurements by tree_id
  growth_dt[, nbMeasure := .N, by = tree_id]

  # filter for tree_id with at least two measures
  growth_dt <- growth_dt[nbMeasure > 1]
 
  if(growth_dt[, .N] > sampleSize)
  { 
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaBA = 4

    # Size classes
    growth_dt[!is.na(dbh), sizeInt := cut(dbh, breaks = seq(floor(min(dbh)), ceiling(max(dbh)), by = deltaS), labels = FALSE, include.lowest = TRUE)]
  	
    # Longitude classes
    growth_dt[!is.na(longitude), lonInt := cut(longitude, breaks = seq(floor(min(longitude)), ceiling(max(longitude)), length.out = nbLonClasses), labels = FALSE, include.lowest = TRUE)]

  	# Latitude classes
    growth_dt[!is.na(latitude), latInt := cut(latitude, breaks = seq(floor(min(latitude)), ceiling(max(latitude)), length.out = nbLatClasses), labels = FALSE, include.lowest = TRUE)]

    # Basal area
    growth_dt[!is.na(BA_plot), BAInt := cut(BA_plot, breaks = seq(floor(min(BA_plot)), ceiling(max(BA_plot)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

  	# Derive frequencies
    N <- growth_dt[, .N]
   	freqDBH = growth_dt[, table(sizeInt)]/N
  	freqLon = growth_dt[, table(lonInt)]/N
  	freqLat = growth_dt[, table(latInt)]/N
    freqCD = growth_dt[, table(BAInt)]/N

  	ls_sizeInt = growth_dt[, unique(sizeInt)]
  	ls_lonInt = growth_dt[, unique(lonInt)]
  	ls_latInt = growth_dt[, unique(latInt)]
    ls_CPInt = growth_dt[, unique(BAInt)]

  	for (s in ls_sizeInt)
  		growth_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		growth_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		growth_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

   for (cd in ls_CPInt)
  		growth_dt[BAInt == cd, proba_cd := freqCD[as.character(cd)]]

  	growth_dt[, proba := proba_s*proba_L*proba_l*proba_cd]

    # mean inclusison probability over individual trees
    tree_prob <- growth_dt[,
      .(mean_prob = mean(proba, na.rm = TRUE), N = unique(nbMeasure)),
      by = tree_id
    ]

    # sample training data
    selected_trees <- tree_prob[
      !is.na(mean_prob),
      sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
    ]

    growth_dt[, sampled := ifelse(tree_id %in% selected_trees, 'training', NA)]

    # sample validade data
    if(growth_dt[, sum(is.na(sampled))] > sampleSize) {
      selected_trees <- tree_prob[
          !tree_id %in% selected_trees & !is.na(mean_prob),
          sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
        ]

      growth_dt[
        is.na(sampled),
        sampled := ifelse(tree_id %in% unique(selected_trees), 'validation', NA)
      ]
    }else{
      growth_dt[is.na(sampled), sampled := 'validation']
    }

    growth_dt <- growth_dt[!is.na(sampled)]

  }

##



## Compute the time distance from each measure to the first measurement of the individual
growth_dt[,
  time := year_measured - year_measured[which.min(year_measured)],
  by = tree_id
]



## run the model

  stanModel <- cmdstan_model('stan/growth.stan')

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = growth_dt[sampled == 'training', .N],
          obs_size = growth_dt[sampled == 'training', dbh],
          time = growth_dt[sampled == 'training', time]
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


## Approximate LOO-CV using subsampling

  # Function to compute log-likelihood
  growth_bertalanffy <- function(dt, post, log)
  {
    # dt is vector of [1] dbh and [2] time
    # post is matrix of [, r, sigma_obs, sigma_Lo, Lmax, mu_Lo]
    Mean <- post[, 5] *
        exp(-post[, 1] * dt[2]) +
        post[, 4] * (1 - exp(-post[, 1] * dt[2]))
    
    Sigma <- sqrt(
      (exp(-2 * post[, 1] * dt[2]) * post[, 3]^2) + post[, 2]^2
    )

    dnorm(
      x = dt[1],
      mean = Mean,
      sd = Sigma,
      log = log
    )
  }

  llfun_bertalanffy <- function(data_i, draws, log = TRUE)
    apply(
      data_i,
      1,
      function(x, y)
        growth_bertalanffy(
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
  r_eff <- loo::relative_eff(
      llfun_bertalanffy, 
      log = FALSE, # relative_eff wants likelihood not log-likelihood values
      chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/2), 
      data = growth_dt[sampled == 'training', .(dbh, time)], 
      draws = post_dist_lg,
      cores = sim_info$nC
  )

  # Loo using x samples
  loo_obj <-
    loo::loo_subsample(
      llfun_bertalanffy,
      observations = sampleSize/10, # take a subsample of size x
      cores = sim_info$nC,
      r_eff = r_eff, 
      draws = post_dist_lg,
      data = growth_dt[sampled == 'training', .(dbh, time)]
  )

##



## save output

  # # save stan fit
  # md_out$save_object(
  #   file = file.path(
  #     'output',
  #     paste0('stanFit_', sp, '.RDS')
  #   )
  # )

  # save posterior drawss
  saveRDS(
    post_dist,
    file = file.path(
      'output',
      paste0('posterior_', sp, '.RDS')
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
    growth_dt[, .(tree_id, year_measured, sampled)],
      file = file.path(
      'output',
      paste0('trainData_', sp, '.RDS')
    )
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
