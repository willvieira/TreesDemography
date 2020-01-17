## Script to run mortality bayesian model

##############################
# Steps:
  # get arguments and data
  # Sampling for the given species
  # scale temperature and precipitation and save parameters
  # Run the model
  # Save model output
##############################


library(data.table)
library(parallel)
library(rstan)

set.seed(42)



## Get arguments about simulation and data

  # species_id

  # max interation

  # number of cores and chains

  # load database

##



## begin sp sampling

  # select the species
  mort <- mort[species_id == sp]

  sampleSize = 1.4e4

  # get nb of tree_id
  nbTreeId = uniqueN(mort$tree_id)

  # sample (only if nbTreeId is bigger than sampleSize)
  if(nbTreeId > sampleSize) mort = mort[tree_id %in% sample(unique(tree_id), sampleSize)]

##



## scale TP and PP variables

  scl <- function(x) {
    (x - min(x))/(max(x) - min(x))
  }

  mort[, ("mean_TP_sc"):= scl(mean_temp_period_3_lag)]
  mort[, ("tot_PP_sc"):= scl(tot_annual_pp_lag)]

  # save scaled info to unscale later
  maxTP3 <- max(mort$mean_temp_period_3_lag)
  minTP3 <- min(mort$mean_temp_period_3_lag)
  maxPP3 <- max(mort$tot_annual_pp_lag)
  minPP3 <- min(mort$tot_annual_pp_lag)

  saveRDS(list(mean_TP = list(maxTP3 = maxTP3, minTP3 = minTP3), tot_PP = list(maxPP3 = maxPP3, minPP3 = minPP3)), "output/scaleInfo.RDS")

##



## run the model

  model <- stan_model(file = "../../../stan/mortality.stan")

  ## Data stan
  dataStan <- list(
          N = mort[, .N],
          T_data = mort$mean_TP_sc,
          P_data = mort$tot_PP_sc,
          D_data = mort$dbh0,
          C_data = mort$canopyStatus,
          time_interv = mort$deltaYear,
          Y = mort$mort)

  ## Run

  out <- rstan::sampling(object = model,
                         data = dataStan,
                         chains = nChains,
                         iter = maxIter,
                         refresh = maxIter/10,
                         cores = nCores,
                         init = "random",
                         control = list(adapt_delta = 0.95),
                         include = FALSE,
                         pars = c("lambda", "DexpPhi", "M_d", "mortL"))

##


## save output

  saveRDS(object = out, file = paste0("output/mortMCMC_", sp, "_", format(Sys.time(), "%Y-%m-%d_%T"), ".RDS"))

##
