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


## run the model

stanModel <- cmdstan_model('stan/recruit.stan')
dir.create(file.path('output', sp))

# Run
md_out <- stanModel$sample(
    data = list(
        N = recruit_dt[, .N],
        nbRecruit = recruit_dt[, nbRecruit],
        plot_size = recruit_dt[, plot_size],
        deltaTime = recruit_dt[, deltaYear_plot]
    ),
    parallel_chains = sim_info$nC,
    iter_warmup = sim_info$maxIter/2,
    iter_sampling = sim_info$maxIter/2,
    output_dir = file.path('output', sp)
)
