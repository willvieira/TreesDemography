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
    which(sim_info$vitalRates == 'mort')
  ]
)


# select the species
dataSource <- dataSource[species_id == sp]

# remove observations with dbh lower than 127 mm
dataSource <- dataSource[dbh0 >= 127]



## run the model

stanModel <- cmdstan_model('stan/mort.stan')
dir.create(file.path('output', sp))

# Run
md_out <- stanModel$sample(
    data = list(
        N = dataSource[, .N],
        state_t1 = dataSource[, mort],
        delta_time = dataSource[, deltaYear]
    ),
    parallel_chains = sim_info$nC,
    iter_warmup = sim_info$maxIter/2,
    iter_sampling = sim_info$maxIter/2,
    output_dir = file.path('output', sp)
)
