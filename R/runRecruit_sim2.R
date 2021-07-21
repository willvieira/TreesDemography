# Script to run JAGS model for regeneration

library(jagsUI)
library(data.table)

# load dataset
out_sp <- readRDS('data/RESEF/jags_data.RDS')

# simulation info
simInfo <- yaml::read_yaml('_simulation_info.yml')

simName <- simInfo$simName
simulations <- simInfo$simulations

maxIter <- simInfo$maxIter
nChains <- simInfo$nC


# define species ID
sp = simInfo$spIds[as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))]

# Filter data to specific species
n <- out_sp[[sp]]


# Remove sites in which delta year was different than 5 years (first attempt to keep time interval standardize between sites)
plot_to_rm <- c('301', '804', '904')
n <- n[which(!gsub('\\_.*', '', rownames(n)) %in% plot_to_rm), ,]


nSites = nrow(n) #Total number of locations
nYears = ncol(n) #Total number of survey years
nObsAges = 4               #Number of observable stages



# #Set the intital values to run the JAGS model
# lamNew <- gammaNew <- thetaNew <- omegaNew <- pNew <- NA
# for(i in 1:4)
# {
#     lamNew[i] <- sample(1:50, 1)
#     omegaNew[i] <- runif(1, 0, 1)
# }

# for(i in 1:2)
#     gammaNew[i] <- runif(1, 0, 5)

# for(i in 1:3)
#     thetaNew[i] <- runif(1, 0, 1)


# #Must specify intital values for S, G, T, and N that are consistent with the model
# #NNew and GNew are empty arrays
# #Fix SNew to some value greater than zero
# SNew <- array(
#     NA,
#     dim = c(nSites, nYears, nObsAges),
#     dimnames = list(
#         rownames(n),
#         1:nYears,
#         c('Sap', 'Juv1', 'Juv2', 'Adu')
#     )
# )

# GNew <- array(
#     NA,
#     dim = c(nSites, nYears, nObsAges), 
#     dimnames = list(
#         rownames(n),
#         1:nYears,
#         c('Sap', 'Juv1', 'Juv2', 'Adu')
#     )
# )

# NNew <- array(
#     NA,
#     dim = c(nSites, nYears, nObsAges),
#     dimnames = list(
#         rownames(n),
#         1:nYears,
#         c('Sap', 'Juv1', 'Juv2', 'Adu')
#     )
# )


# # Add a big number to nmax for each stage to fill in the NNew matrix
# NNew[, 1, 1] <- sample(5:20, nSites, replace = TRUE)
# NNew[, 1, 2] <- sample(5:15, nSites, replace = TRUE)
# NNew[, 1, 3] <- sample(1:10, nSites, replace = TRUE)
# NNew[, 1, 4] <- sample(1:10, nSites, replace = TRUE)

# SNew[, 1, 1] <- rbinom(n = nSites, size = NNew[, 1, 1], prob = 0.9)
# SNew[, 1, 2] <- rbinom(n = nSites, size = NNew[, 1, 2], prob = 0.9)
# SNew[, 1, 3] <- rbinom(n = nSites, size = NNew[, 1, 3], prob = 0.9)
# SNew[, 1, 4] <- rbinom(n = nSites, size = NNew[, 1, 4], prob = 0.9)

# #Very important!!
# #Make sure SNew+GNew=NNew or jags will not run!
# GNew = NNew - SNew

# #Set intial values
# initStage <- function()
#     list(
#         gamma = gammaNew,
#         omega = omegaNew,
#         lambda = lamNew,
#         theta = thetaNew,
#         S = SNew,
#         G = GNew,
#         N = NNew
#     )

#Create all the necessary inputs for JAGS
#Bundle data
Dat <- list(
    nSites = nSites, 
    nYears = nYears,
    n = n
)

# Parameters to be monitored
ParsStage <- c(
    'lambda',
    'omega',
    'gamma',
    'theta',
    'Ntotal'
)


# sample
out_sample <- jagsUI::jags(
    data = Dat,
    parameters.to.save = ParsStage,
    model.file = 'jags/recruit_sim2',
    n.chains = nChains,
    n.iter = maxIter,
    n.burnin = maxIter/2,
    n.thin = 10,
    parallel = TRUE
)

dir.create('output')
saveRDS(out_sample, file = paste0('output/out_sim2', sp, '_', simName, '.RDS'))


pdf(file = paste0('output/plot_sim2', sp, '_', simName, '.pdf'), width = 9.3, height = 16.2)
traceplot(
    out_sample,
    parameters = c('lambda', 'omega', 'gamma', 'theta', 'Ntotal'),
    layout = c(11, 4),
    ask = FALSE
)
dev.off()

# #Print out a summary of the parameter estimates
# summary(out_sample)
# summary(out_sample$samples)[[1]]

# #Graph the model results
# plot(out_sample)

# #Check the R-hat statistic to ensure convergence
# library(coda)
# gelman.diag(out_sample$sample)
