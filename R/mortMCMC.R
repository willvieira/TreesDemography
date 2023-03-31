## Script to run mortality bayesian model

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
      which(sim_info$vitalRates == 'mort')
    ]
  )

##



## stratified sampling to define traning and validation data

  # select the species
  mort_dt <- dataSource[species_id == sp]

  # get number of measurements by tree_id
  mort_dt[, nbMeasure := .N, by = tree_id]

  # filter for tree_id with at least two measures
  mort_dt <- mort_dt[nbMeasure > 1]
 
  if(mort_dt[, .N] > sampleSize)
  { 
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaBA = 4

    # Size classes
    mort_dt[!is.na(dbh0), sizeInt := cut(dbh0, breaks = seq(floor(min(dbh0)), ceiling(max(dbh0)), by = deltaS), labels = FALSE, include.lowest = TRUE)]
  	
    # Longitude classes
    mort_dt[!is.na(longitude), lonInt := cut(longitude, breaks = seq(floor(min(longitude)), ceiling(max(longitude)), length.out = nbLonClasses), labels = FALSE, include.lowest = TRUE)]

  	# Latitude classes
    mort_dt[!is.na(latitude), latInt := cut(latitude, breaks = seq(floor(min(latitude)), ceiling(max(latitude)), length.out = nbLatClasses), labels = FALSE, include.lowest = TRUE)]

    # Basal area sp
    mort_dt[!is.na(BA_comp_sp), BAspInt := cut(BA_comp_sp, breaks = seq(floor(min(BA_comp_sp)), ceiling(max(BA_comp_sp)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

    # Basal area intra
    mort_dt[!is.na(BA_comp_intra), BAintraInt := cut(BA_comp_intra, breaks = seq(floor(min(BA_comp_intra)), ceiling(max(BA_comp_intra)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

    # Derive frequencies
    N <- mort_dt[, .N]
    freqDBH = mort_dt[, table(sizeInt)/N]
    freqLon = mort_dt[, table(lonInt)/N]
    freqLat = mort_dt[, table(latInt)/N]
    freqBAsp = mort_dt[, table(BAspInt)/N]
    freqBAintra = mort_dt[, table(BAintraInt)/N]

    ls_sizeInt = mort_dt[, unique(sizeInt)]
    ls_lonInt = mort_dt[, unique(lonInt)]
    ls_latInt = mort_dt[, unique(latInt)]
    ls_BAspInt = mort_dt[, unique(BAspInt)]
    ls_BAintraInt = mort_dt[, unique(BAintraInt)]

    for (s in ls_sizeInt)
      mort_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

    for (lg in ls_lonInt)
      mort_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

    for (lt in ls_latInt)
      mort_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

    for (basp in ls_BAspInt)
      mort_dt[BAspInt == basp, proba_BAsp := freqBAsp[as.character(basp)]]

    for (baintra in ls_BAintraInt)
      mort_dt[BAintraInt == baintra, proba_BAintra := freqBAintra[as.character(baintra)]]

    mort_dt[, proba := proba_s*proba_L*proba_l*proba_BAsp*proba_BAintra]

    # mean inclusison probability over individual trees
    tree_prob <- mort_dt[,
      .(mean_prob = mean(proba, na.rm = TRUE), N = unique(nbMeasure)),
      by = tree_id
    ]

    # sample training data
    selected_trees <- tree_prob[
      !is.na(mean_prob),
      sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
    ]

    mort_dt[, sampled := ifelse(tree_id %in% selected_trees, 'training', NA)]

    # sample validade data
    if(mort_dt[, sum(is.na(sampled))] > sampleSize) {
      selected_trees <- tree_prob[
          !tree_id %in% selected_trees & !is.na(mean_prob),
          sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
        ]

      mort_dt[
        is.na(sampled),
        sampled := ifelse(tree_id %in% unique(selected_trees), 'validation', NA)
      ]
    }else{
      mort_dt[is.na(sampled), sampled := 'validation']
    }

    mort_dt <- mort_dt[!is.na(sampled)]

  }else{
    mort_dt[, sampled := 'training']
  }

##

## define plot_id in sequence to be used in stan
plot_id_uq <- mort_dt[sampled == 'training', unique(plot_id)]
toSub_plot <- data.table(
  plot_id = plot_id_uq,
  plot_id_seq = 1:length(plot_id_uq)
)

# merge
mort_dt[
  toSub_plot,
  plot_id_seq := i.plot_id_seq,
  on = 'plot_id'
]

# scale climate variables
mort_dt[, temp_sc := (bio_01_mean - min(bio_01_mean))/(max(bio_01_mean) - min(bio_01_mean))]
mort_dt[, prec_sc := (bio_12_mean - min(bio_12_mean))/(max(bio_12_mean) - min(bio_12_mean))]


## run the model

  stanModel <- cmdstan_model('stan/mortality.stan')

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = mort_dt[sampled == 'training', .N],
          Np = mort_dt[sampled == 'training', length(unique(plot_id))],
          plot_id = mort_dt[sampled == 'training', plot_id_seq],
          state_t1 = mort_dt[sampled == 'training', mort],
          delta_time = mort_dt[sampled == 'training', deltaYear],
          size_t0 = mort_dt[sampled == 'training', dbh0],
          BA_comp_sp = mort_dt[sampled == 'training', BA_comp_sp],
          BA_comp_inter = mort_dt[sampled == 'training', BA_comp_intra],bio_01_mean = mort_dt[sampled == 'training', temp_sc],
          bio_12_mean = mort_dt[sampled == 'training', prec_sc],
          maxTemp = dataSource[species_id == sp, max(bio_01_mean_scl, na.rm=T)],
          minTemp = dataSource[species_id == sp, min(bio_01_mean_scl, na.rm=T)],
          maxPrec = dataSource[species_id == sp, max(bio_12_mean_scl, na.rm=T)],
          minPrec = dataSource[species_id == sp, min(bio_12_mean_scl, na.rm=T)]
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
      filter(!grepl(pattern = '\\[', par)),
    file = file.path(
      'output',
      paste0('posteriorPop_', sp, '.RDS')
    )
  )

  # posterior of plot_id parameters
  saveRDS(
    post_dist |>
      filter(grepl(pattern = 'psiPlot', par)),
    file = file.path(
      'output',
      paste0('posteriorpsiPlot_', sp, '.RDS')
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
    mort_dt[, .(tree_id, plot_id_seq, year0, sampled)],
      file = file.path(
      'output',
      paste0('trainData_', sp, '.RDS')
    )
  )

##


## Approximate LOO-CV using subsampling

  # Function to compute log-likelihood
  mort_model <- function(dt, post, log)
  {
    # dt is a vector of:
    # [1] status
    # [2] deltaTime
    # [3] plot_id
    # [4] dbh0
    # [5] BA_comp_sp
    # [6] BA_comp_inter
    # [7] bio_01_mean
    # [8] bio_01_mean
    
    # Add plot_id random effects
    psiPlot <- post[, paste0('psiPlot[', dt[3], ']')]

    # fixed effects
    longev_log <- 1/(1 + exp(
        -(
          post[, 'psi'] + 
          psiPlot +
          -(log(dt[4]/post[, 'size_opt'])/post[, 'size_var'])^2 +
          post[, 'Beta'] * (dt[5] + post[, 'theta'] * dt[6]) +
          -post[, 'tau_temp'] * (dt[7] - post[, 'optimal_temp'])^2 +
          -post[, 'tau_prec'] * (dt[8] - post[, 'optimal_prec'])^2
        )
      )
    )

    # time component of the model
    mortality_time <- 1 - (longev_log^dt[2])

    # deal with numerical instability
    if(any(mortality_time == 1))
      mortality_time[which(mortality_time == 1)] = 0.999999

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
      llfun_mort, 
      log = FALSE, # relative_eff wants likelihood not log-likelihood values
      chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/2), 
      data = mort_dt[
        sampled == 'training',
        .(mort, deltaYear, plot_id_seq, dbh0, BA_comp_sp, BA_comp_intra, temp_sc, prec_sc)
      ],
      draws = post_dist_lg,
      cores = sim_info$nC
  )

  # Loo using x samples
  loo_obj <- loo::loo_subsample(
      llfun_mort,
      observations = sampleSize/5, # take a subsample of size x
      cores = sim_info$nC,
      r_eff = r_eff, 
      draws = post_dist_lg,
      data = mort_dt[
        sampled == 'training',
        .(mort, deltaYear, plot_id_seq, dbh0, BA_comp_sp, BA_comp_intra, temp_sc, prec_sc)
      ]
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
