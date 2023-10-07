cat('####### Sending simulations to the server #######\n')
###############################
# Send folder to run simulations on the server
# Will Vieira
# February 8, 2020
# Last edited: Nov, 2022
##############################



##############################
# Steps:
  # Load simulation variables
  # Send simulations
  # create bash submission script
##############################


## Set variables to be simulated

  model_dir <- 'model_survival/intcpt_plot_size'
  serverInfo <- yaml::read_yaml('_serverInfo.yml')
  simInfo <- yaml::read_yaml(file.path(model_dir, '_simulation_info.yml'))

  vitalRates <- simInfo$vitalRates
  simName <- paste0('scratch/', vitalRates, '/', simInfo$simName)

##



## Folders and files

  filesToSend <- '_simulation_info.yml'
  foldersToSend <- c('R', 'stan')

##



## Send simulations

  myPass <- serverInfo$myPass
  myUser <- serverInfo$myUser
  myAddress <- paste0(simInfo$cluster, '.', serverInfo$myAddress)
  
  # first create same folder on the server side
  sapply(
    paste0(
      'sshpass -f ', myPass,
      ' ssh -t ',
      myUser, '@', myAddress,
      ' "mkdir -p ',
      simName, c('/output"')
    ),
    system
  )

  # now send only files and folders necessary to the run the simulations
  
  for(File in filesToSend)
  {
    script <- paste0('sshpass -f ',
                    myPass,
                    ' scp ',
                    getwd(),
                    '/',
                    model_dir,
                    '/',
                    File,
                    ' ',
                    myUser,
                    '@',
                    myAddress,
                    ':/home/',
                    myUser,
                    '/',
                    simName, '/',
                    File)

    system(script)
  }
  
  for(Folder in foldersToSend)
  {
    script <- paste0('sshpass -f ',
                myPass,
                ' scp -r ',
                getwd(),
                '/',
                model_dir,
                '/',
                Folder,
                ' ',
                myUser,
                '@',
                myAddress,
                ':/home/',
                myUser,
                '/',
                simName, '/',
                Folder)

    system(script)
  }

##



## Create bash submission script R wrapper

  for(VT in vitalRates) {
  
    # Create bash file
    bash_file <- paste0('#!/bin/bash
#SBATCH --account=def-dgravel
#SBATCH -t 1-23:40:00
#SBATCH --mem-per-cpu=5000M
#SBATCH --ntasks=', simInfo$nC, '
#SBATCH --job-name=', simInfo$simName, '
#SBATCH --mail-user=', serverInfo$email, '
#SBATCH --mail-type=ALL
#SBATCH --array=1-', length(simInfo$spIds), '

module load StdEnv/2020 r/4.1.0

NCORES=\\$SLURM_CPUS_PER_TASK R -f R/', VT, 'MCMC.R
')

    # send bash file to the server
    system(
      paste0(
        'sshpass -f ', myPass,
        ' ssh -t ',
        myUser, '@', myAddress,
        ' "echo \'', bash_file, '\' > ', simName, '/sub_', VT, '.sh"'
      )
    )

    # Create R script wrapper
    Rscript_file <- paste0('# Wrapper script to compile model and send simulation batch
library(cmdstanr)
stanModel <- cmdstan_model(\\"stan/', VT, '.stan\\")
system(\\"sbatch sub_', VT, '.sh\\")
')
    
    # send Rscript file to the server
    system(
      paste0(
        'sshpass -f ', myPass,
        ' ssh -t ',
        myUser, '@', myAddress,
        ' "echo \'', Rscript_file, '\' > ', simName, '/run_', VT, '.R"'
      )
    )
  }

##
