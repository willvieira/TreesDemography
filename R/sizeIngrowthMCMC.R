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


# Remove outliers (a tree cannot be first identified with already > 50 mm)
# 99% quantile of size distribution is 415 mm
sizeIngrowth_dt <- sizeIngrowth_dt[dbh < 500]

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

  f_init <- function()
    return( list(
      size_int = runif(1, 127, 200),
      phi_time = rnorm(1, 7, 2),
      sigma_size = rnorm(1, 50, 20)
      )
    )

  dir.create(file.path('output', sp))

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = sizeIngrowth_dt[, .N],
          Lo = 127,
          size_ingrowth = sizeIngrowth_dt[, dbh],
          delta_time = sizeIngrowth_dt[, deltaYear_plot]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2,
      init = list(f_init(), f_init(), f_init(), f_init()),,
      output_dir = file.path('output', sp)
  )

##
