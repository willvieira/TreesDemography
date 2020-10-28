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



## begin stratified sampling (thanks Amaēl for sharing)

  # select the species
  growth_dt <- growth_dt[sp_code2 == sp]

  # get latitude and longitude from SHAPE list
  getCoord <- function(SHAPE, coord = 1) {
      n <- length(SHAPE)
      xy <- unique(unlist(SHAPE))
      return (rep(xy[coord], n))
  }
  growth_dt[, longitude := getCoord(SHAPE, coord = 1), by = ID_PE]
  growth_dt[, latitude := getCoord(SHAPE, coord = 2), by = ID_PE]

  if(growth_dt[, .N] > sampleSize) {
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaC = 5

    # Size classes
    sizeClass = seq(from = min(growth_dt$dbh0), to = max(growth_dt$dbh0) + deltaS, by = deltaS)
    nbSizeClass = length(sizeClass) - 1

  	for (i in 1:nbSizeClass)
  		growth_dt[ sizeClass[i] <= dbh0 & dbh0 < sizeClass[i + 1], sizeInt := i]

  	# Longitude classes
  	lonClass = seq(from = min(growth_dt$longitude), to = max(growth_dt$longitude) + 1, length.out = nbLonClasses) # +1 to be able to get the extremeties

  	for (i in 1:(nbLonClasses - 1))
  		growth_dt[ lonClass[i] <= longitude & longitude < lonClass[i + 1], lonInt := i]

  	# Species-specific latitude classes
  	latClass = seq(from = min(growth_dt$latitude), to = max(growth_dt$latitude) + 1, length.out = nbLatClasses) # +1 to be able to get the extremeties

  	for (i in 1:(nbLatClasses - 1))
  		growth_dt[ latClass[i] <= latitude & latitude < latClass[i + 1], latInt := i]

    # BA classes
    BAClass = seq(from = min(growth_dt$BA), to = max(growth_dt$BA) + deltaC, by = deltaC)
    nbBAClass = length(BAClass) - 1

  	for (i in 1:nbBAClass)
  		growth_dt[ BAClass[i] <= BA & BA < BAClass[i + 1], BAInt := i]

  	# Derive frequencies
  	freqDBH = growth_dt[, table(sizeInt)]/growth_dt[, .N]
  	freqLon = growth_dt[, table(lonInt)]/growth_dt[, .N]
  	freqLat = growth_dt[, table(latInt)]/growth_dt[, .N]
    freqBA = growth_dt[, table(BAInt)]/growth_dt[, .N]

  	ls_sizeInt = growth_dt[, unique(sizeInt)]
  	ls_lonInt = growth_dt[, unique(lonInt)]
  	ls_latInt = growth_dt[, unique(latInt)]
    ls_BAInt = growth_dt[, unique(BAInt)]

  	for (s in ls_sizeInt)
  		growth_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		growth_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		growth_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

    for (b in ls_BAInt)
  		growth_dt[BAInt == b, proba_b := freqBA[as.character(b)]]

  	growth_dt[, proba := proba_s*proba_L*proba_l*proba_b]

  	sampledIndices = sample(x = 1:growth_dt[,.N], size = sampleSize, replace = FALSE, prob = growth_dt$proba)

    # get samples values
    growth_dt = growth_dt[sampledIndices, ]

  }

##



## run the model

  model <- stan_model(file = "../../stan/growth_sim1.stan")

  ## Data stan
  dataStan <- list(
          N = growth_dt[, .N],
          T_data = growth_dt$value5_bio60_01,
          P_data = growth_dt$value5_bio60_12,
          D_data = growth_dt$dbh0,
          BA_data = growth_dt$BA,
          CD_data = growth_dt$canopyDistance,
          Y = growth_dt$growth)

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
                         pars = c("mu_d"))

##


## save output

  saveRDS(object = out, file = paste0("output/growthMCMC_sim1", sp, "_", format(Sys.time(), "%Y-%m-%d_%T"), ".RDS"))

##
