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
  growth_dt <- growth_dt[species_id == sp]

  # filter to remove plot_id with s_star == 0
  growth_dt <- growth_dt[s_star > 0]

  # # get latitude and longitude from SHAPE list
  # getCoord <- function(SHAPE, coord = 1) {
  #     n <- length(SHAPE)
  #     xy <- unique(unlist(SHAPE))
  #     return (rep(xy[coord], n))
  # }
  # growth_dt[, longitude := getCoord(SHAPE, coord = 1), by = ID_PE]
  # growth_dt[, latitude := getCoord(SHAPE, coord = 2), by = ID_PE]

  if(growth_dt[, .N] > sampleSize) {
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaCD = 2

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

    # canopyDistance
    CDClass = seq(from = min(growth_dt$canopyDistance), to = max(growth_dt$canopyDistance) + deltaCD, by = deltaCD)
    nbCDClass = length(CDClass) - 1
    
    for (i in 1:nbCDClass)
        growth_dt[ CDClass[i] <= canopyDistance & canopyDistance < CDClass[i + 1], CDInt := i]

  	# Derive frequencies
  	freqDBH = growth_dt[, table(sizeInt)]/growth_dt[, .N]
  	freqLon = growth_dt[, table(lonInt)]/growth_dt[, .N]
  	freqLat = growth_dt[, table(latInt)]/growth_dt[, .N]
    freqCD = growth_dt[, table(CDInt)]/growth_dt[, .N]

  	ls_sizeInt = growth_dt[, unique(sizeInt)]
  	ls_lonInt = growth_dt[, unique(lonInt)]
  	ls_latInt = growth_dt[, unique(latInt)]
    ls_CPInt = growth_dt[, unique(CDInt)]

  	for (s in ls_sizeInt)
  		growth_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		growth_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		growth_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

    for (cd in ls_CPInt)
        growth_dt[CDInt == cd, proba_cd := freqCD[as.character(cd)]]

  	growth_dt[, proba := proba_s*proba_L*proba_l*proba_cd]

  	sampledIndices = sample(x = 1:growth_dt[,.N], size = sampleSize, replace = FALSE, prob = growth_dt$proba)

    # get samples values
    growth_dt = growth_dt[sampledIndices, ]
  }

##


## Adjust plot_id to be a evenly sequence from 1 to nbPlot_id

  # Create new data.table with unique ID_PE and a sequence of 1:N to replace ID_PE
  plot_id_uq <- growth_dt[, unique(plot_id)]
  toSub <- data.table(plot_id = plot_id_uq, plot_id_seq = 1:length(plot_id_uq))

  # Create new column with replaced codes
  growth_dt[toSub, on = .(plot_id), plot_id_seq := i.plot_id_seq]

  dir.create('sampleInfo')
  saveRDS(sampledIndices, file = 'sampleInfo/sampledIndices_sim2.RDS')
  saveRDS(toSub, file = 'sampleInfo/toSub_sim2.RDS')

##




## run the model

  model <- stan_model(file = "../../stan/growth_sim2.stan")

  ## Data stan
  dataStan <- list(
          N = growth_dt[, .N],
          Np = growth_dt[, length(unique(plot_id_seq))],
          plot_id = growth_dt[, plot_id_seq],
          T_data = growth_dt$mean_temp_period_3_lag,
          P_data = growth_dt$tot_annual_pp_lag,
          D_data = growth_dt$dbh0,
          C_data = growth_dt$canopyDistance,
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
                         pars = c("mu_d", "pdg_plot"))

##


## save output

  saveRDS(object = out, file = paste0("output/growthMCMC_sim2", sp, "_", format(Sys.time(), "%Y-%m-%d_%T"), ".RDS"))

##
