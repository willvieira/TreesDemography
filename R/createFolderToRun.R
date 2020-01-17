cat('####### Creating species folder to run MCMC #######\n')
###############################
# Automate the creation of a folder for each species to run MCMC
# Will Vieira
# February 07, 2019
##############################

##############################
# Steps:
  # Set variables to be simulated
  # For each vital rate and species_id:
    # Create the main folder and subfolders
    # Create the vitalrate R script
    # Create the vitalrate sub.sh script to run in the server
##############################




## Set variables to be simulated

  # species id
  spIds <- readRDS('data/spIds.RDS')

  # max interation
  maxIter <- 15000

  # number of cores and chains
  nC <- 5

  # vital rates
  vitalRates <- c('mort', 'growth')

##



## Create allSp directory

  dir <- 'MCMC/allSp/'
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

##



## For each vital rate and species_id

  for(sp in spIds)
  {

    # Create the main folder and subfolders (output)

      dir.create(paste0(dir, sp), showWarnings = FALSE)
      dir.create(paste0(dir, sp, '/output'), showWarnings = FALSE)

    #


    for(vital in vitalRates) {

      ## edit respective vitalRate MCMC scripts and save in the folder

        linesToAdd = c(paste0('   sp <- \'', sp, '\''),
                       paste0('   maxIter <- ', maxIter),
                       paste0('   nCores <- nChains <- ', nC),
                       paste0('   ', vital, ' <- readRDS(\'../../../data/', vital, '_dt.RDS\')'))
        # read file
        MCMCscript <- readLines(paste0('R/', vital, 'MCMC.R'))
        # update lines
        MCMCscript[c(24, 26, 28, 30)] <- linesToAdd
        # save file
        writeLines(MCMCscript, paste0(dir, sp, '/run_', vital, '.R'))

      ##


      # Create the vital sub.sh script to run in the server
        bash <- paste0("#!/bin/bash

#SBATCH --account=def-dgravel
#SBATCH -t 4-00:00:00
#SBATCH --mem-per-cpu=2500M
#SBATCH --ntasks=", nC, "
#SBATCH --job-name=", vital, sp, "
#SBATCH --mail-user=willian.vieira@usherbrooke.ca
#SBATCH --mail-type=FAIL

NCORES=$SLURM_CPUS_PER_TASK R -f ~/TreesDemography/MCMC/allSp/", sp, "/run_", vital, ".R")

      # save bash script as sub.sh
      system(paste0("echo ", "'", bash, "' > ", dir, sp, "/sub.sh"))
    #

    cat('  creating folder for species', sp, '(', which(spIds == sp), 'of', length(spIds), ')', '\r')
  }
}
