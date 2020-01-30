cat('####### Sending batch submission MCMC #######\n')
###############################
# Automate the run of all species in the server
# Will Vieira
# February 07, 2019
# Last update: January 16, 2020
##############################


##############################
# Steps:
  # Load simulation variables
  # For each species_id:
    # Change directory to the sp directory
    # run sbatch command with the sub.sh file
##############################



## Load simulation variables

  # species id
  spIds <- readRDS('data/spIds.RDS')
  # vital rates
  eval(parse(text = readLines('R/createFolderToRun.R')[33]))

##



## checking if I am on the server

  if(system('hash sbatch') == 0) {

    # for each species_id:
    dir <- 'MCMC/'

    for(sp in spIds)
    {
      # Change directory to the sp directory (I'm doing so the slurm is saved inside each sp folder)
      setwd(paste0(dir, sp, '/'))

      # run sub.sh
      for(vital in vitalRates)
        system(paste0('sbatch sub_', vital, '.sh'))

      # return to the main directory
      setwd('../../../')

      cat('  Submitting batch for', sp, '(', which(spIds == sp), 'of', length(spIds), ')', '\r')
    }
  }else {

    print('Sbatch not found, skipping job submission.')

  }

##
