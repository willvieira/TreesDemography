## Script to run size ingrowth bayesian model

##############################
# Steps:
  # get simulation variables and data
  # Subset 
  # Run the model
  # Save model output
##############################


library(data.table)
library(cmdstanr)
library(tidyverse)
library(loo)

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
      which(sim_info$vitalRates == 'sizeIngrowth')
    ]
  )

##




# select the species
sizeIngrowth_dt <- dataSource[species_id == sp]


# ## define plot_id in sequence to be used in stan
# plot_id_uq <- sizeIngrowth_dt[, unique(plot_id)]
# toSub_plot <- data.table(
#   plot_id = plot_id_uq,
#   plot_id_seq = 1:length(plot_id_uq)
# )

# # merge
# sizeIngrowth_dt[
#   toSub_plot,
#   plot_id_seq := i.plot_id_seq,
#   on = 'plot_id'
# ]


## run the model

  stanModel <- cmdstan_model('stan/sizeIngrowth.stan')

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = sizeIngrowth_dt[, .N],
          Lo = 126,
          size_ingrowth = sizeIngrowth_dt[, dbh],
          delta_time = sizeIngrowth_dt[, deltaYear_plot]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2
  )

  # extract posterior distribution
  post_dist <- md_out$draws(format = 'df') |>
    select(!c('lp__', '.chain', '.iteration', '.draw', contains('log_lik'))) |>
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
    rhat = md_out$summary() |> select(variable, rhat),
    time = md_out$time()
  )

  # loo 
  loo_obj <- md_out$loo(cores = 12)

##



## save output

  dir.create(file.path('output', sim_info$simName))

  # posterior of population level parameters
  saveRDS(
    post_dist |>
      filter(!grepl(pattern = '\\[', par)),
    file = file.path(
      'output', sim_info$simName,
      paste0('posteriorPop_', sp, '.RDS')
    )
  )

  # # generated predictions
  # saveRDS(
  #   post_dist |>
  #     filter(grepl(pattern = 'y_rep', par)),
  #   file = file.path(
  #     'output', sim_info$simName,
  #     paste0('y_rep', sp, '.RDS')
  #   )
  # )

  # # save sampling diagnostics
  # saveRDS(
  #   diag_out,
  #   file = file.path(
  #     'output',
  #     paste0('diagnostics_', sp, '.RDS')
  #   )
  # )

  # save Loo
  saveRDS(
    loo_obj,
      file = file.path(
      'output', sim_info$simName,
      paste0('loo_', sp, '.RDS')
    )
  )

##
