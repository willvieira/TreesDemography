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

  # create dir to save species raw sampling output
  dir.create(file.path('output', sp))

##



## stratified sampling to define traning and validation data

  # select the species
  mort_dt <- dataSource[species_id == sp]

  # get number of measurements by tree_id
  mort_dt[, nbMeasure := .N, by = tree_id]

  # filter for tree_id with at least two measures
  mort_dt <- mort_dt[nbMeasure > 1]

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

  
## add predicted growth
 
  sim_output <- '../bertalanffy_plotInd_BAspcTempPrecscl/output'
  trainData <- readRDS(paste0(sim_output, '/trainData_', sp, '.RDS'))
  trainData[
    dataSource,
    plot_id := i.plot_id,
    on = 'tree_id'
  ]

  pars_pop <- readRDS(paste0(sim_output, '/posteriorPop_', sp, '.RDS')) |>
    pivot_wider(names_from = 'par', values_from = 'value') |>
    as.data.frame()

  pars_plot <- readRDS(paste0(sim_output, '/posteriorrPlot_', sp, '.RDS')) |>
    mutate(plot_id_seq = parse_number(par)) |>
    left_join(
      trainData[, .(plot_id_seq = head(plot_id_seq, 1)), by = plot_id]
    ) |>
    select(plot_id, value, iter) |>
    pivot_wider(names_from = 'plot_id', values_from = 'value') |>
    as.data.frame()

  pars_tree <- readRDS(paste0(sim_output, '/posteriorrTree_', sp, '.RDS')) |>
    mutate(tree_id_seq = parse_number(par)) |>
    left_join(
      trainData[, .(tree_id_seq = head(tree_id_seq, 1)), by = tree_id]
    ) |>
    select(tree_id, value, iter) |>
    pivot_wider(names_from = 'tree_id', values_from = 'value') |>
    as.data.frame()

  growth_f <- function(
    pars_pop, pars_plot, pars_tree, plot_id, tree_id, deltaTime,
    dbh0, BA_comp_sp, BA_comp_intra, bio_01_mean_scl, bio_12_mean_scl
  ){
    if(plot_id %in% names(pars_plot)) {
      plot_re = pars_plot[, plot_id]
    }else{
      plot_re = rnorm(nrow(pars_pop), 0, pars_pop[, 'sigma_PlotTree'] * pars_pop[, 'p_plotTree'])
    }
    if(tree_id %in% names(pars_tree)) {
      tree_re = pars_tree[, tree_id]
    }else{
      tree_re = rnorm(nrow(pars_pop), 0, pars_pop[, 'sigma_PlotTree'] * (1 - pars_pop[, 'p_plotTree']))
    }

    rPlot = exp(
      pars_pop[, 'r'] +
      plot_re +
      tree_re +
      pars_pop[, 'Beta'] * (BA_comp_sp + pars_pop[, 'theta'] * BA_comp_intra) +
      -pars_pop[, 'tau_temp'] * (bio_01_mean_scl - pars_pop[, 'optimal_temp'])^2 +
      -pars_pop[, 'tau_prec'] * (bio_12_mean_scl - pars_pop[, 'optimal_prec'])^2
    )

    rPlotTime = exp(-rPlot * deltaTime)

    size_t1 = rnorm(
      nrow(pars_pop),
      mean = dbh0 * rPlotTime + pars_pop[, 'Lmax'] * (1 - rPlotTime),
      sd = pars_pop[, 'sigma_obs']
    )

    growth_pred <- (size_t1 - dbh0)/deltaTime
    
    # get mean and sd of predicted growth rate
    list(growth_mean = mean(growth_pred), growth_sd = sd(growth_pred))    
  }

  mort_dt[, rowID := .I]
  mort_dt[,
    c('growth_mean', 'growth_sd') := growth_f(
      pars_pop, pars_plot, pars_tree, plot_id, tree_id, deltaYear, dbh0, BA_comp_sp, BA_comp_intra, bio_01_mean_scl, bio_12_mean_scl
    ),
    by = rowID
  ]

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
          growth_mean = mort_dt[sampled == 'training', growth_mean],
          growth_sd = mort_dt[sampled == 'training', growth_sd]
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2,
      output_dir = file.path('output', sp)
  )
