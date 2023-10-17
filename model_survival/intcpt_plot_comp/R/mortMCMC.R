## Script to run recruitment bayesian model

library(data.table)
library(cmdstanr)
library(tidyverse)

## Get simulation info

# array (species)
array_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

set.seed(array_id)

# load info file
sim_info <- yaml::read_yaml('_simulation_info.yml')

# species_ids
sp <- sim_info$spIds[array_id]

# load database
dataSource <- readRDS(
  sim_info$dataSources[
    which(sim_info$vitalRates == 'mort')
  ]
)


# select the species
dataSource <- dataSource[species_id == sp]

# remove observations with dbh lower than 127 mm
dataSource <- dataSource[dbh0 >= 127]

## define plot_id in sequence to be used in stan
plot_id_uq <- dataSource[, unique(plot_id)]
toSub <- data.table(
  plot_id = plot_id_uq,
  plot_id_seq = 1:length(plot_id_uq)
)

# merge
dataSource[
  toSub,
  plot_id_seq := i.plot_id_seq,
  on = 'plot_id'
]

## run the model

stanModel <- cmdstan_model('stan/mort.stan')
dir.create(file.path('output', sp), recursive = TRUE)

# Run
md_out <- stanModel$sample(
    data = list(
        N = dataSource[, .N],
        Np = dataSource[, length(unique(plot_id))],
        plot_id = dataSource[, plot_id_seq],
        state_t1 = dataSource[, mort],
        delta_time = dataSource[, deltaYear],
        BA_comp_sp = dataSource[, BA_comp_sp],
        BA_comp_inter = dataSource[, BA_comp_inter],
        bio_01_mean = dataSource[, bio_01_mean_scl],
        bio_12_mean = dataSource[, bio_12_mean_scl],
        maxTemp = dataSource[, max(bio_01_mean_scl, na.rm=T)],
        minTemp = dataSource[, min(bio_01_mean_scl, na.rm=T)],
        maxPrec = dataSource[, max(bio_12_mean_scl, na.rm=T)],
        minPrec = dataSource[, min(bio_12_mean_scl, na.rm=T)]
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
