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



## define plot_id and year_id in sequence to be used in stan

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

  # get all possible time intervals
  time_interval <- unique(recruit_dt[, .(year0, year1)])
  time_interval[, time_int := 1:.N]

  # merge
  recruit_dt[
    time_interval,
    time_int := i.time_int,
    on = c('year0' = 'year0', 'year1', 'year1')
  ]

  ## get all possible years between year0 and year1
  all_years <- recruit_dt[, seq(min(year0), max(year1), 1)]
  toSub_year <- data.table(
    all_years = all_years,
    all_years_seq = 1:length(all_years)
  )

  # merge
  time_interval[
    toSub_year,
    year0_seq := i.all_years_seq,
    on = c('year0' = 'all_years')
  ]

  time_interval[
    toSub_year,
    year1_seq := i.all_years_seq,
    on = c('year1' = 'all_years')
  ]

##



## run the model

  stanModel <- cmdstan_model('stan/recruit.stan')

  dir.create(file.path('output', sp))

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
          plot_id = recruit_dt[, plot_id_seq],
          Ny = length(all_years),
          Ni = time_interval[, .N],
          year0_seq = time_interval[, year0_seq],
          year1_seq = time_interval[, year1_seq],
          year_int = recruit_dt[, time_int]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2,
      output_dir = file.path('output', sp)
  )

  # save train and validate data
  saveRDS(
    toSub,
      file = file.path(
      'output',
      paste0('toSub_', sp, '.RDS')
    )
  )

  # save year intervals
  saveRDS(
    time_interval,
      file = file.path(
      'output',
      paste0('timeInterval_', sp, '.RDS')
    )
  )
