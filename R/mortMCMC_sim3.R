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
  mort_dt <- mort_dt[sp_code2 == sp]

  # get latitude and longitude from SHAPE list
  getCoord <- function(SHAPE, coord = 1) {
      n <- length(SHAPE)
      xy <- unique(unlist(SHAPE))
      return (rep(xy[coord], n))
  }
  mort_dt[, longitude := getCoord(SHAPE, coord = 1), by = ID_PE]
  mort_dt[, latitude := getCoord(SHAPE, coord = 2), by = ID_PE]

  if(mort_dt[, .N] > sampleSize) {
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaC = 5

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

    # canopyDistance classes
    canopyClass = seq(from = min(mort_dt$canopyDistance), to = max(mort_dt$canopyDistance) + deltaC, by = deltaC)
    nbCanopyClass = length(canopyClass) - 1

  	for (i in 1:nbCanopyClass)
  		mort_dt[ canopyClass[i] <= canopyDistance & canopyDistance < canopyClass[i + 1], canopyInt := i]

  	# Derive frequencies
  	freqDBH = mort_dt[, table(sizeInt)]/mort_dt[, .N]
  	freqLon = mort_dt[, table(lonInt)]/mort_dt[, .N]
  	freqLat = mort_dt[, table(latInt)]/mort_dt[, .N]
    freqCan = mort_dt[, table(canopyInt)]/mort_dt[, .N]

  	ls_sizeInt = mort_dt[, unique(sizeInt)]
  	ls_lonInt = mort_dt[, unique(lonInt)]
  	ls_latInt = mort_dt[, unique(latInt)]
    ls_canInt = mort_dt[, unique(canopyInt)]

  	for (s in ls_sizeInt)
  		mort_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		mort_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		mort_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

    for (c in ls_canInt)
  		mort_dt[canopyInt == c, proba_c := freqCan[as.character(c)]]

  	mort_dt[, proba := proba_s*proba_L*proba_l*proba_c]

  	sampledIndices = sample(x = 1:mort_dt[,.N], size = sampleSize, replace = FALSE, prob = mort_dt$proba)

    # get samples values
    mort_dt = mort_dt[sampledIndices, ]
  }

##



## run the model

  model <- stan_model(file = "../../stan/mortality_sim3.stan")

  ## Data stan
  dataStan <- list(
          N = mort_dt[, .N],
          T_data = mort_dt$value5_bio60_01,
          P_data = mort_dt$value5_bio60_12,
          D_data = mort_dt$dbh0,
          C_data = mort_dt$BA,
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
                         pars = c("M_d", "mortL", "lgSq", "lgBA"))

##


## save output

  saveRDS(object = out, file = paste0("output/mortMCMC_sim3", sp, "_", format(Sys.time(), "%Y-%m-%d_%T"), ".RDS"))

##
