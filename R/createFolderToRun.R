cat('####### Creating species folder to run MCMC #######\n')
###############################
# Automate the creation of a folder for each species to run MCMC
# Will Vieira
# February 07, 2019
# Last update: January 17, 2020
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

  simInfo <- yaml::read_yaml('_simulation_info.yml')

  # species id
  if(is.null(simInfo$spIds)) {
    spIds <- readRDS('data/spIds.RDS')
  }else {
    spIds <- simInfo$spIds
  }

  # max interation
  maxIter <- as.numeric(simInfo$maxIter)

  # number of cores and chains
  nC <- as.numeric(simInfo$nC)

  # vital rates
  vitalRates <- simInfo$vitalRates

  # sample size
  sampleSize <- as.numeric(simInfo$sampleSize)

##



## Create allSp directory

  dir <- 'MCMC/'
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
                       paste0('   ', vital, '_dt <- readRDS(\'../../data/', vital, '_dt.RDS\')'),
                       paste0('   sampleSize <- ', sampleSize))

        # read file
        MCMCscript <- readLines(paste0('R/', vital, 'MCMC.R'))
        # update lines
        MCMCscript[c(24, 26, 28, 30, 32)] <- linesToAdd
        # save file
        writeLines(MCMCscript, paste0(dir, sp, '/run_', vital, '.R'))

      ##


      # Create the vital sub.sh script to run in the server
        bash <- paste0("#!/bin/bash

#SBATCH --account=def-dgravel
#SBATCH -t 5-00:00:00
#SBATCH --mem-per-cpu=2500M
#SBATCH --ntasks=", nC, "
#SBATCH --job-name=", vital, sp, "
#SBATCH --mail-user=willian.vieira@usherbrooke.ca
#SBATCH --mail-type=FAIL

NCORES=$SLURM_CPUS_PER_TASK R -f ~/TreesDemography/MCMC/", sp, "/run_", vital, ".R")

      # save bash script as sub.sh
      system(paste0("echo ", "'", bash, "' > ", dir, sp, "/sub_", vital, ".sh"))
    #

    cat('  creating folder for species', sp, '(', which(spIds == sp), 'of', length(spIds), ')', '\r')
  }
}
