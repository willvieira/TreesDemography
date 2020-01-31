## Script to run growth bayesian model

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

  # sample size


##



## begin sp sampling

  # select the species
  growth <- growth[species_id == sp]

  # get nb of tree_id
  nbTreeId = uniqueN(growth$tree_id)

  # sample (only if nbTreeId is bigger than sampleSize)
  if(nbTreeId > sampleSize) growth = growth[tree_id %in% sample(unique(tree_id), sampleSize)]

##



## scale TP and PP variables

  scl <- function(x) {
    (x - min(x))/(max(x) - min(x))
  }

  growth_dt[, ("mean_TPperiod3_sc"):= scl(mean_temp_period_3_lag)]
  growth_dt[, ("tot_PPperiod3_sc"):= scl(tot_pp_period3_lag)]

  # save scaled info to unscale later
  maxTP3 <- max(growth_dt$mean_temp_period_3_lag)
  minTP3 <- min(growth_dt$mean_temp_period_3_lag)
  maxPP3 <- max(growth_dt$tot_pp_period3_lag)
  minPP3 <- min(growth_dt$tot_pp_period3_lag)


  saveRDS(list(mean_TP = list(maxTP3 = maxTP3, minTP3 = minTP3), tot_PP = list(maxPP3 = maxPP3, minPP3 = minPP3)), "output/scaleInfo.RDS")

##



## run the model

  model <- stan_model(file = "../../../stan/growth.stan")

  ## Data stan
  dataStan <- list(
          N = growth[, .N],
          T_data = growth$mean_TPperiod3_sc,
          P_data = growth$tot_PPperiod3_sc,
          D_data = growth$dbh0,
          C_data = growth$canopyStatus,
          time_interv = growth$deltaYear,
          Y = growth$growth)

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

  saveRDS(object = out, file = paste0("output/growthMCMC_", sp, "_", format(Sys.time(), "%Y-%m-%d_%T"), ".RDS"))

##
