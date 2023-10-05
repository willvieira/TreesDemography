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
    which(sim_info$vitalRates == 'growth')
  ]
)


# select the species
dataSource <- dataSource[species_id == sp]

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

stanModel <- cmdstan_model('stan/growth.stan')
dir.create(file.path('output', sp))

# Run
md_out <- stanModel$sample(
    data = list(
        N = dataSource[, .N],
        obs_size_t1 = dataSource[, dbh],
        time = dataSource[, deltaTime],
        obs_size_t0 = dataSource[, dbh0],
        Np = dataSource[, length(unique(plot_id))],
        plot_id = dataSource[, plot_id_seq]
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
