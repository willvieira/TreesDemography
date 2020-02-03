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

  # sample size


##



##  begin stratified sampling (thanks Amaēl for sharing)

  # select the species
  mort_dt <- mort_dt[species_id == sp]

  if(mort_dt[, .N] > sampleSize) {
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50

    # Size classes
    sizeClass = seq(from = min(mort_dt$dbh0), to = max(mort_dt$dbh0) + deltaS, by = deltaS)
    nbSizeClass = length(sizeClass) - 1

  	for (i in 1:nbSizeClass)
  		mort_dt[ sizeClass[i] <= dbh0 & dbh0 < sizeClass[i + 1], sizeInt := i]

  	# Longitude classes
  	lonClass = seq(from = min(mort_dt$longitude), to = max(mort_dt$longitude) + 1, length.out = nbLonClasses) # +1 to be able to get the extremeties

  	for (i in 1:(nbLonClasses - 1))
  		mort_dt[ lonClass[i] <= longitude & longitude < lonClass[i + 1], lonInt := i]

  	# Species-specific latitude classes
  	latClass = seq(from = min(mort_dt$latitude), to = max(mort_dt$latitude) + 1, length.out = nbLatClasses) # +1 to be able to get the extremeties

  	for (i in 1:(nbLatClasses - 1))
  		mort_dt[ latClass[i] <= latitude & latitude < latClass[i + 1], latInt := i]

  	# Derive frequencies
  	freqDBH = mort_dt[, table(sizeInt)]/mort_dt[, .N]
  	freqLon = mort_dt[, table(lonInt)]/mort_dt[, .N]
  	freqLat = mort_dt[, table(latInt)]/mort_dt[, .N]

  	ls_sizeInt = mort_dt[, unique(sizeInt)]
  	ls_lonInt = mort_dt[, unique(lonInt)]
  	ls_latInt = mort_dt[, unique(latInt)]

  	for (s in ls_sizeInt)
  		mort_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		mort_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		mort_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

  	mort_dt[, proba := proba_s*proba_L*proba_l]

  	sampledIndices = sample(x = 1:mort_dt[,.N], size = sampleSize, replace = FALSE, prob = mort_dt$proba)

    # get samples values
    mort_dt = mort_dt[sampledIndices, ]
  }

##



## scale TP and PP variables

  scl <- function(x) {
    (x - min(x))/(max(x) - min(x))
  }

  mort_dt[, ("mean_TP_sc"):= scl(mean_temp_period_3_lag)]
  mort_dt[, ("tot_PP_sc"):= scl(tot_annual_pp_lag)]

  # save scaled info to unscale later
  maxTP3 <- max(mort_dt$mean_temp_period_3_lag)
  minTP3 <- min(mort_dt$mean_temp_period_3_lag)
  maxPP3 <- max(mort_dt$tot_annual_pp_lag)
  minPP3 <- min(mort_dt$tot_annual_pp_lag)

  saveRDS(list(mean_TP = list(maxTP3 = maxTP3, minTP3 = minTP3),
          tot_PP = list(maxPP3 = maxPP3, minPP3 = minPP3)),
          "output/scaleInfo_mort.RDS")

##



## run the model

  model <- stan_model(file = "../../stan/mortality.stan")

  ## Data stan
  dataStan <- list(
          N = mort_dt[, .N],
          T_data = mort_dt$mean_TP_sc,
          P_data = mort_dt$tot_PP_sc,
          D_data = mort_dt$dbh0,
          C_data = mort_dt$canopyStatus,
          time_interv = mort_dt$deltaYear,
          Y = mort_dt$mort)

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
