## Script to run growth bayesian model

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
      which(sim_info$vitalRates == 'growth')
    ]
  )

##



## stratified sampling to define traning and validation data

  # select the species
  growth_dt <- dataSource[species_id == sp]

  # remove dead trees
  growth_dt <- growth_dt[status == 1]
  
  # get number of measurements by tree_id
  growth_dt[, nbMeasure := .N, by = tree_id]

  # filter for tree_id with at least two measures
  growth_dt <- growth_dt[nbMeasure > 1]
 
  if(growth_dt[, .N] > sampleSize)
  { 
    # define the size of (i) size, (ii) longitute and (iii) latitude classes to stratify sampling
    deltaS = 10; nbLonClasses = nbLatClasses = 50; deltaBA = 4

    # Size classes
    growth_dt[!is.na(dbh), sizeInt := cut(dbh, breaks = seq(floor(min(dbh)), ceiling(max(dbh)), by = deltaS), labels = FALSE, include.lowest = TRUE)]
  	
    # Longitude classes
    growth_dt[!is.na(longitude), lonInt := cut(longitude, breaks = seq(floor(min(longitude)), ceiling(max(longitude)), length.out = nbLonClasses), labels = FALSE, include.lowest = TRUE)]

  	# Latitude classes
    growth_dt[!is.na(latitude), latInt := cut(latitude, breaks = seq(floor(min(latitude)), ceiling(max(latitude)), length.out = nbLatClasses), labels = FALSE, include.lowest = TRUE)]

    # Basal area
    growth_dt[!is.na(BA_plot), BAInt := cut(BA_plot, breaks = seq(floor(min(BA_plot)), ceiling(max(BA_plot)), by = deltaBA), labels = FALSE, include.lowest = TRUE)]

  	# Derive frequencies
    N <- growth_dt[, .N]
   	freqDBH = growth_dt[, table(sizeInt)]/N
  	freqLon = growth_dt[, table(lonInt)]/N
  	freqLat = growth_dt[, table(latInt)]/N
    freqCD = growth_dt[, table(BAInt)]/N

  	ls_sizeInt = growth_dt[, unique(sizeInt)]
  	ls_lonInt = growth_dt[, unique(lonInt)]
  	ls_latInt = growth_dt[, unique(latInt)]
    ls_CPInt = growth_dt[, unique(BAInt)]

  	for (s in ls_sizeInt)
  		growth_dt[sizeInt == s, proba_s := freqDBH[as.character(s)]]

  	for (lg in ls_lonInt)
  		growth_dt[lonInt == lg, proba_L := freqLon[as.character(lg)]]

  	for (lt in ls_latInt)
  		growth_dt[latInt == lt, proba_l := freqLat[as.character(lt)]]

   for (cd in ls_CPInt)
  		growth_dt[BAInt == cd, proba_cd := freqCD[as.character(cd)]]

  	growth_dt[, proba := proba_s*proba_L*proba_l*proba_cd]

    # mean inclusison probability over individual trees
    tree_prob <- growth_dt[,
      .(mean_prob = mean(proba, na.rm = TRUE), N = unique(nbMeasure)),
      by = tree_id
    ]

    # sample training data
    selected_trees <- tree_prob[
      !is.na(mean_prob),
      sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
    ]

    growth_dt[, sampled := ifelse(tree_id %in% selected_trees, 'training', NA)]

    # sample validade data
    if(growth_dt[, sum(is.na(sampled))] > sampleSize) {
      selected_trees <- tree_prob[
          !tree_id %in% selected_trees & !is.na(mean_prob),
          sample(tree_id, floor(sampleSize/mean(N)), prob = mean_prob)
        ]

      growth_dt[
        is.na(sampled),
        sampled := ifelse(tree_id %in% unique(selected_trees), 'validation', NA)
      ]
    }else{
      growth_dt[is.na(sampled), sampled := 'validation']
    }

    growth_dt <- growth_dt[!is.na(sampled)]

  }

##


## Filter data based on plot location only

growth_dt[, lat_class := cut(latitude, seq(min(latitude), max(latitude), length.out = 11),  include.lowest = TRUE)]

set.seed(3)
plot_to_keep <- growth_dt[, sample(unique(plot_id), 10, replace = T), by = lat_class]$V1

growth_dt = growth_dt[plot_id %in% plot_to_keep]
growth_dt[, sampled := 'training']

## compute de deltaTime between measures of dbh
growth_dt[,
  deltaTime := year_measured - lag(year_measured, 1),
  by = tree_id
]

## define previous measure
growth_dt[,
  dbh0 := lag(dbh, 1),
  by = tree_id
]

## Fill the NA first measures with their non lag information
growth_dt[is.na(deltaTime), deltaTime := 0]
growth_dt[deltaTime == 0, dbh0 := dbh]


## define plot_id in sequence to be used in stan
plot_id_uq <- growth_dt[sampled == 'training', unique(plot_id)]
toSub_plot <- data.table(
  plot_id = plot_id_uq,
  plot_id_seq = 1:length(plot_id_uq)
)

# add latitude
toSub_plot[
  growth_dt,
  latitude := i.latitude,
  on = 'plot_id'
]

## Do the same for tree_id
tree_id_uq <- growth_dt[sampled == 'training', unique(tree_id)]
toSub_tree <- data.table(
  tree_id = tree_id_uq,
  tree_id_seq = 1:length(tree_id_uq)
)

# merge
growth_dt[
  toSub_plot,
  plot_id_seq := i.plot_id_seq,
  on = 'plot_id'
]

growth_dt[
  toSub_tree,
  tree_id_seq := i.tree_id_seq,
  on = 'tree_id'
]


# distance matrix between plots for gaussian process
toSub_plot[
  dataSource,
  `:=`(
    longitude = i.longitude,
    latitude = i.latitude
  ),
  on = 'plot_id'
]

# generate grid of lat and lon points to predict random effects
xs = toSub_plot[, seq(min(longitude), max(longitude), length.out = 20)]
ys = toSub_plot[, seq(min(latitude), max(latitude), length.out = 20)]

grid_xy <- expand.grid(longitude = xs, latitude = ys)

# Interpolate grid with plot locations so we predict for the regions around the plot only (to avoid too large matrix of distance)
plot_buffer <- toSub_plot |>
  select(longitude, latitude) |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) |>
  st_buffer(dist = 300000)

toKeep <- grid_xy |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) |>
  st_intersects(plot_buffer, sparse = FALSE) |>
  apply(1, any) |> which()

plot_xy <- toSub_plot |>
  select(!plot_id) |>
  bind_rows(
    grid_xy[toKeep, ] |>
      mutate(plot_id_seq = (1:length(toKeep)) + nrow(toSub_plot))
  )

# calculate distance matrix
dist_mat <- plot_xy |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) |>
  st_distance()

# remove units
units(dist_mat) <- NULL

# meters to Km
dist_mat <- dist_mat/1000

# scale
dist_min <- min(dist_mat)
dist_max <- max(dist_mat)
dist_mat_scale <- (dist_mat - dist_min)/(dist_max - dist_min) + 0.00001 # TODO: check if 0 is ok

## run the model

  stanModel <- cmdstan_model('stan/growth.stan')

  # Run
  md_out <- stanModel$sample(
      data = list(
          N = growth_dt[sampled == 'training', .N],
          obs_size_t1 = growth_dt[sampled == 'training', dbh],
          time = growth_dt[sampled == 'training', deltaTime],
          obs_size_t0 = growth_dt[sampled == 'training', dbh0],
          Np = growth_dt[sampled == 'training', length(unique(plot_id))],
          plot_id = growth_dt[sampled == 'training', plot_id_seq],
          Nt = growth_dt[sampled == 'training', length(unique(tree_id))],
          tree_id = growth_dt[sampled == 'training', tree_id_seq],
          BA_comp = growth_dt[sampled == 'training', BA_comp],
          bio_01_mean = growth_dt[sampled == 'training', bio_01_mean],
          bio_12_mean = growth_dt[sampled == 'training', bio_12_mean],
          maxTemp = dataSource[species_id == sp, max(bio_01_mean, na.rm = T)],
          minTemp = dataSource[species_id == sp, min(bio_01_mean, na.rm = T)],
          maxPrec = dataSource[species_id == sp, max(bio_12_mean, na.rm = T)],
          minPrec = dataSource[species_id == sp, min(bio_12_mean, na.rm = T)],
          N_grid = length(toKeep),
          dist = dist_mat_scale
      ),
      parallel_chains = sim_info$nC,
      iter_warmup = sim_info$maxIter/2,
      iter_sampling = sim_info$maxIter/2
  )

  # extract posterior distribution
  post_dist <- md_out$draws(format = 'df') |>
    select(!c('lp__', '.chain', '.iteration', '.draw')) |>
    pivot_longer(
      cols = everything(),
      names_to = 'par',
      values_to = 'value'
    ) |>
    group_by(par) |>
    mutate(iter = row_number()) |>
    ungroup()

  # extract sample diagnostics
  diag_out <- list(
    diag_summary = md_out$diagnostic_summary(),
    rhat = md_out$summary() |> select(variable, rhat),
    time = md_out$time()
  )
##


## Approximate LOO-CV using subsampling

  # Function to compute log-likelihood
  growth_bertalanffy <- function(dt, post, log)
  {
    # dt is a vector of:
    # [1] dbh,
    # [2] time,
    # [3] dbh0,
    # [4] plot_id_seq,
    # [5] tree_id_seq,
    # [6] BA_comp,
    # [7] bio_01_mean,
    # [8] bio_12_mean
    
    # Add plot_id random effects
    rPlot_log <- post[, paste0('rPlot_log[', dt[4], ']')]

    # Add tree_id random effects
    rTree_log <- post[, paste0('rTree_log[', dt[5], ']')]

    # fixed effects
    rPlot_beta <- exp(
      post[, 'r'] + 
      rPlot_log +
      rTree_log +
      dt[6] * post[, 'Beta'] +
      -post[, 'tau_temp'] * (dt[7] - post[, 'optimal_temp'])^2 +
      -post[, 'tau_prec'] * (dt[8] - post[, 'optimal_prec'])^2
    )

    # time component of the model
    rPlotTime <- exp(-rPlot_beta * dt[2])

    # mean
    Mean <- dt[3] * rPlotTime + post[, 'Lmax'] * (1 - rPlotTime)

    # likelihood
    dnorm(
      x = dt[1],
      mean = Mean,
      sd = post[, 'sigma_obs'],
      log = log
    )
  }

  llfun_bertalanffy <- function(data_i, draws, log = TRUE)
    apply(
      data_i,
      1,
      function(x, y)
        growth_bertalanffy(
          x,
          y,
          log = log
        ),
        y = draws
    )

  # Matrix of posterior draws
  post_dist_lg <- post_dist |>
    pivot_wider(
      names_from = par,
      values_from = value
    ) |>
    select(-iter) |>
    as.matrix()

  # compute relative efficiency
  # this is slow and optional but is recommended to allow for adjusting PSIS
  # effective sample size based on MCMC effective sample size)
  keepTrying <- TRUE; nTry = 1
  while(keepTrying & nTry < 10) {
    r_eff <- loo::relative_eff(
        llfun_bertalanffy, 
        log = FALSE, # relative_eff wants likelihood not log-likelihood values
        chain_id = rep(1:sim_info$nC, each = sim_info$maxIter/2), 
        data = growth_dt[
          sampled == 'training',
          .(dbh, deltaTime, dbh0, plot_id_seq, tree_id_seq, BA_comp, bio_01_mean, bio_12_mean)
        ],
        draws = post_dist_lg,
        cores = sim_info$nC
    )

    if(is.numeric(r_eff[1])) {
      keepTrying <- FALSE
    }else{
      nTry <- nTry + 1
    }
  }

  # Loo using x samples
  loo_obj <-
    loo::loo_subsample(
      llfun_bertalanffy,
      observations = sampleSize/10, # take a subsample of size x
      cores = sim_info$nC,
      r_eff = r_eff, 
      draws = post_dist_lg,
      data = growth_dt[
        sampled == 'training',
        .(dbh, deltaTime, dbh0, plot_id_seq, tree_id_seq, BA_comp, bio_01_mean, bio_12_mean)
      ]
  )

##



## save output

  # posterior of population level parameters
  saveRDS(
    post_dist |>
      filter(!grepl(pattern = '\\[', par)),
    file = file.path(
      'output',
      paste0('posteriorPop_', sp, '.RDS')
    )
  )

  # posterior of plot_id parameters
  saveRDS(
    post_dist |>
      filter(grepl(pattern = 'rPlot_log', par)),
    file = file.path(
      'output',
      paste0('posteriorrPlot_', sp, '.RDS')
    )
  )

  # posterior of tree_id parameters
  saveRDS(
    post_dist |>
      filter(grepl(pattern = 'rTree_log', par)),
    file = file.path(
      'output',
      paste0('posteriorrTree_', sp, '.RDS')
    )
  )

  # save sampling diagnostics
  saveRDS(
    diag_out,
    file = file.path(
      'output',
      paste0('diagnostics_', sp, '.RDS')
    )
  )

  # save train and validate data
  saveRDS(
    growth_dt[, .(tree_id, tree_id_seq, plot_id_seq, year_measured, sampled)],
      file = file.path(
      'output',
      paste0('trainData_', sp, '.RDS')
    )
  )

  # save Loo
  saveRDS(
    loo_obj,
      file = file.path(
      'output',
      paste0('loo_', sp, '.RDS')
    )
  )

##




## Fig
gaus_f <- function(sigma_f, lengthscale_f)
  return( function(x) sigma_f * exp(-1/(2 * lengthscale_f^2) * x^2) )

curve(gaus_f(1, 1)(x), 0, 20, add = T, col = 2, ylim = c(0, 1))

post_dist |>
  filter(par %in% c('lengthscale_f', 'sigma_f')) |>
  pivot_wider(
    names_from = par,
    values_from = value
  ) |>
  mutate(sim = 'parameter') |>
  bind_rows(
    tibble(
      iter = 1:4000,
      lengthscale_f = abs(rnorm(4000, 0, 1)),
      sigma_f = abs(rnorm(4000, 0, 1)),
      sim = 'prior'
    )
  ) |>
  rowwise() |>
  mutate(
    gausf = list(gaus_f(sigma_f, lengthscale_f)),
    x = list(seq(0, 5, 0.1)),
    y = list(gausf(x))
  ) |>
  select(iter, sim, x, y)  |>
  unnest(cols = c(x, y)) |>
  ggplot(aes(x, y, color = sim)) +
    stat_ribbon(.width = 0.5) +
    scale_fill_brewer() +
    theme_classic()


post_dist |>
  filter(grepl('rPlot', par)) |>
  mutate(plot_id_seq = parse_number(par)) |>
  left_join(toSub_plot) |>
  ggplot(aes(latitude, value)) +
    stat_ribbon() +
    scale_fill_brewer() +
    theme_minimal() +
    ylab('Plot random effects')



tt = post_dist |>
  filter(grepl('rPlot', par)) |>
  mutate(plot_id_seq = parse_number(par)) |>
  filter(plot_id_seq <= 95) |>
  left_join(toSub_plot) |>
  group_by(latitude) |>
  summarise(value = mean(value))

post_dist |>
  filter(grepl('rPlot', par)) |>
  mutate(plot_id_seq = parse_number(par)) |>
  filter(plot_id_seq > 95) |>
  mutate(latitude = seq(-2, 2, length.out = 100)[plot_id_seq - 95]) |>
  ggplot(aes(x = latitude * 2.6 + 48.3, y = value)) +
    stat_lineribbon() +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_point(data = tt, aes(latitude, value)) +
    scale_fill_brewer() +
    theme_classic() +
    ylab('Plot random effects') + xlab('Latitude')



# Version 2
#############################################################

## Fig
gaus_f <- function(sigma_f, lengthscale_f)
  return( function(x) sigma_f * exp(-lengthscale_f * x^2)  )

post_dist |>
  filter(par %in% c('lengthscale_f', 'sigma_f')) |>
  pivot_wider(
    names_from = par,
    values_from = value
  ) |>
  mutate(sim = 'parameter') |>
  bind_rows(
    tibble(
      iter = 1:8000,
      lengthscale_f = abs(rnorm(8000, 0, 1)),
      sigma_f = abs(rnorm(8000, 0, 1)),
      sim = 'prior'
    )
  ) |>
  rowwise() |>
  mutate(
    gausf = list(gaus_f(sigma_f, lengthscale_f)),
    x = list(seq(0, 2, 0.1)),
    y = list(gausf(x))
  ) |>
  select(iter, sim, x, y)  |>
  unnest(cols = c(x, y)) |>
  ggplot(aes(x, y, color = sim)) +
    stat_ribbon(.width = 0.5) +
    scale_fill_brewer() +
    theme_classic()


# Fig 2 (spatial grid)
toSub_plot_value <- toSub_plot |>
  left_join(
    post_dist |>
      filter(grepl('rPlot', par)) |>
      mutate(plot_id_seq = parse_number(par)) |>
      group_by(plot_id_seq) |>
      summarise(value = mean(value))
  )

post_dist |>
  filter(grepl('rPlot', par)) |>
  mutate(plot_id_seq = parse_number(par)) |>
  filter(plot_id_seq > nrow(toSub_plot)) |>
  group_by(plot_id_seq) |>
  summarise(
    value = mean(value)
  ) |>
  left_join(
    grid_xy[toKeep, ] |>
      bind_cols(plot_id_seq = (1:length(toKeep)) + nrow(toSub_plot))
  ) |>
  #st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) |>
  ggplot(aes(x = longitude, y = latitude, fill = value)) +
    geom_raster(interpolate = TRUE) +
    geom_point(
      data = toSub_plot_value, aes(longitude, latitude, fill = value), size = 3, shape = 21
    ) +
    scale_fill_gradient2() +
    labs(fill = 'Plot offset') +
    theme_minimal() +
    ggtitle('Jack pine')

# Fig 3 (corr between plots)
post_mean <- post_dist |>
  filter(par %in% c('sigma_f', 'lengthscale_f', 'sigman')) |>
  group_by(par) |>
  summarise(value = mean(value))

K <- matrix(0, nrow = nrow(toSub_plot), ncol = nrow(toSub_plot))
for(i in 1:nrow(toSub_plot)){
  for(j in 1:nrow(toSub_plot)){
    K[i,j] <- median(post_mean$value[2]) * exp(-median(post_mean$value[1]) * (dist_mat/1000)[i,j])
  }
}
diag(K) <- median(post$etasq) + 0.01

Rho <- round(cov2cor(K),2)
rownames(Rho) <- seq(1, nrow(toSub_plot), 1)
colnames(Rho) <- rownames(Rho)

plot(
  latitude ~ longitude,
  data = toSub_plot,
  #col = value,
  pch = 16,
  cex = 0.4,
  col = 'blue',
  main = 'Correlation inferred from model'
)

for(i in 1:nrow(toSub_plot)){
  for(j in 1:nrow(toSub_plot)){
    if(i < j){
      lines(
        x = c(toSub_plot$longitude[i], toSub_plot$longitude[j]), 
        y = c(toSub_plot$latitude[i], toSub_plot$latitude[j]),
        lwd = .1,
        col = rgb(0, 0, 0, Rho[i,j]^2)
      )
    }
  }
}
