## Script to run mortality bayesian model

##############################
# Steps:
  # get simulation variables and data
  # Subset 
  # scale temperature and precipitation and save parameters
  # Run the model
  # Save model output
##############################


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
  
  # sample size
  sampleSize <- sim_info$sampleSize

  # load database
  dataSource <- readRDS(
    sim_info$dataSources[
      which(sim_info$vitalRates == 'mort')
    ]
  )

##



## stratified sampling to define traning and validation data

  # select the species
  mort_dt <- dataSource[species_id == sp]

  # remove observations with dbh lower than 127 mm
  mort_dt <- mort_dt[dbh0 >= 127]

  # get number of measurements by tree_id
  mort_dt[, nbMeasure := .N, by = tree_id]
 
  if(mort_dt[, .N] > sampleSize)
  { 
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaBA = 4

    # Size classes
    mort_dt[!is.na(dbh0), sizeInt := cut(dbh0, breaks = seq(floor(min(dbh0)), ceiling(max(dbh0)), by = deltaS), labels = FALSE, include.lowest = TRUE)]
  	
    # Longitude classes
    mort_dt[!is.na(longitude), lonInt := cut(longitude, breaks = seq(floor(min(longitude)), ceiling(max(longitude)), length.out = nbLonClasses), labels = FALSE, include.lowest = TRUE)]

  	# Latitude classes
    mort_dt[!is.na(latitude), latInt := cut(latitude, breaks = seq(floor(min(latitude)), ceiling(max(latitude)), length.out = nbLatClasses), labels = FALSE, include.lowest = TRUE)]

    # Basal area sp
    mort_dt[!is.na(BA_comp_sp), BAspInt := cut(BA_comp_sp, breaks = seq(floor(min(BA_comp_sp)), ceiling(max(BA_comp_sp)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

    # Basal area intra
    mort_dt[!is.na(BA_comp_intra), BAintraInt := cut(BA_comp_intra, breaks = seq(floor(min(BA_comp_intra)), ceiling(max(BA_comp_intra)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

  	# Derive frequencies
    N <- mort_dt[, .N]
   	freqDBH = mort_dt[, table(sizeInt)/N]
  	freqLon = mort_dt[, table(lonInt)/N]
  	freqLat = mort_dt[, table(latInt)/N]
    freqBAsp = mort_dt[, table(BAspInt)/N]
    freqBAintra = mort_dt[, table(BAintraInt)/N]

  	ls_sizeInt = mort_dt[, unique(sizeInt)]
  	ls_lonInt = mort_dt[, unique(lonInt)]
  	ls_latInt = mort_dt[, unique(latInt)]
    ls_BAspInt = mort_dt[, unique(BAspInt)]
    ls_BAintraInt = mort_dt[, unique(BAintraInt)]

  	for (s in ls_sizeInt)
  		mort_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		mort_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		mort_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

   for (basp in ls_BAspInt)
  		mort_dt[BAspInt == basp, proba_BAsp := freqBAsp[as.character(basp)]]

   for (baintra in ls_BAintraInt)
  		mort_dt[BAintraInt == baintra, proba_BAintra := freqBAintra[as.character(baintra)]]

  	mort_dt[, proba := proba_s*proba_L*proba_l*proba_BAsp*proba_BAintra]

    # mean inclusison probability over individual trees
    tree_prob <- mort_dt[,
      .(mean_prob = mean(proba, na.rm = TRUE), N = unique(nbMeasure)),
      by = tree_id
    ]

    # sample training data
    selected_trees <- tree_prob[
      !is.na(mean_prob),
      sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
    ]

    mort_dt[, sampled := ifelse(tree_id %in% selected_trees, 'training', NA)]

    # sample validade data
    if(mort_dt[, sum(is.na(sampled))] > sampleSize) {
      selected_trees <- tree_prob[
          !tree_id %in% selected_trees & !is.na(mean_prob),
          sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
        ]

      mort_dt[
        is.na(sampled),
        sampled := ifelse(tree_id %in% unique(selected_trees), 'validation', NA)
      ]
    }else{
      mort_dt[is.na(sampled), sampled := 'validation']
    }

    mort_dt <- mort_dt[!is.na(sampled)]

  }else{
    mort_dt[, sampled := 'training']
  }

##

## define plot_id in sequence to be used in stan
plot_id_uq <- mort_dt[sampled == 'training', unique(plot_id)]
toSub_plot <- data.table(
  plot_id = plot_id_uq,
  plot_id_seq = 1:length(plot_id_uq)
)

# merge
mort_dt[
  toSub_plot,
  plot_id_seq := i.plot_id_seq,
  on = 'plot_id'
]

# get all possible time intervals
time_interval <- unique(mort_dt[sampled == 'training', .(year0, year1)])
time_interval[, time_int := 1:.N]

# merge
mort_dt[
  time_interval,
  time_int := i.time_int,
  on = c('year0' = 'year0', 'year1', 'year1')
]

## get all possible years between year0 and year1
all_years <- mort_dt[sampled == 'training', seq(min(year0), max(year1), 1)]
toSub_year <- data.table(
  all_years = all_years,
  all_years_seq = 1:length(all_years)
)

# merge
time_interval[
  toSub_year,
  year0_seq := i.all_years_seq,
  on = c('year0' = 'all_years')
]
time_interval[
  toSub_year,
  year1_seq := i.all_years_seq,
  on = c('year1' = 'all_years')
]


## run the model

  stanModel <- cmdstan_model('stan/mortality.stan')

  dir.create(file.path('output', sp))

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = mort_dt[sampled == 'training', .N],
          Np = mort_dt[sampled == 'training', length(unique(plot_id))],
          plot_id = mort_dt[sampled == 'training', plot_id_seq],
          Ny = length(all_years),
          Ni = time_interval[, .N],
          year0_seq = time_interval[, year0_seq],
          year1_seq = time_interval[, year1_seq],
          year_int = mort_dt[sampled == 'training', time_int],
          state_t1 = mort_dt[sampled == 'training', mort],
          delta_time = mort_dt[sampled == 'training', deltaYear],
          size_t0 = mort_dt[sampled == 'training', dbh0],
          BA_comp_sp = mort_dt[sampled == 'training', BA_comp_sp],
          BA_comp_inter = mort_dt[sampled == 'training', BA_comp_intra],
          bio_01_mean = mort_dt[sampled == 'training', bio_01_mean_scl],
          bio_12_mean = mort_dt[sampled == 'training', bio_12_mean_scl],
          maxTemp = dataSource[species_id == sp, max(bio_01_mean_scl, na.rm=T)],
          minTemp = dataSource[species_id == sp, min(bio_01_mean_scl, na.rm=T)],
          maxPrec = dataSource[species_id == sp, max(bio_12_mean_scl, na.rm=T)],
          minPrec = dataSource[species_id == sp, min(bio_12_mean_scl, na.rm=T)]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2,
      output_dir = file.path('output', sp)
  )

  # save train and validate data
  saveRDS(
    mort_dt[, .(tree_id, plot_id, plot_id_seq, year0, sampled)],
      file = file.path(
      'output',
      paste0('trainData_', sp, '.RDS')
    )
  )
  saveRDS(
    time_interval,
      file = file.path(
      'output',
      paste0('timeInterval_', sp, '.RDS')
    )
  )

##
