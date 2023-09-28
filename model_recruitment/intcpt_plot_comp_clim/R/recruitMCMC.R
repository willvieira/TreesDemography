## Script to run recruitment bayesian model

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

# load database
dataSource <- readRDS(
  sim_info$dataSources[
    which(sim_info$vitalRates == 'recruit')
  ]
)



## stratified sampling to define traning and validation data

# select the species
recruit_dt <- dataSource[species_id == sp]

# remove NA plots
recruit_dt <- recruit_dt[!is.na(plot_id)]

# remove NA Basal area
recruit_dt <- recruit_dt[!is.na(BA_adult)]



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
        bio_01_mean = recruit_dt[, bio_01_mean_scl],
        bio_12_mean = recruit_dt[, bio_12_mean_scl],
        maxTemp = recruit_dt[species_id == sp, max(bio_01_mean_scl, na.rm=T)],
        minTemp = recruit_dt[species_id == sp, min(bio_01_mean_scl, na.rm=T)],
        maxPrec = recruit_dt[species_id == sp, max(bio_12_mean_scl, na.rm=T)],
        minPrec = recruit_dt[species_id == sp, min(bio_12_mean_scl, na.rm=T)]
    ),
    parallel_chains = sim_info$nC,
    iter_warmup = sim_info$maxIter/2,
    iter_sampling = sim_info$maxIter/2,
    output_dir = file.path('output', sp)
)


## save plot_id sequence
saveRDS(
  toSub,
    file = file.path(
    'output',
    paste0('toSub_', sp, '.RDS')
  )
)
