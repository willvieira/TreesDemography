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

  serverInfo <- yaml::read_yaml('_serverInfo.yml')
  simInfo <- yaml::read_yaml('_simulation_info.yml')
  
  simName <- simInfo$simName
  vitalRates <- simInfo$vitalRates
  dataSources <- simInfo$dataSources

##



## Folders and files

  filesToSend <- c('_simulation_info.yml', '_rawDataLink', dataSources)
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
      simName, c('/data"', '/output"')
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



## Create bash submission script

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

NCORES=\\$SLURM_CPUS_PER_TASK R -f R/recruitMCMC.R
')

  # write in the server
  system(
    paste0(
      'echo "',
      bash_file,
      '" | ssh ',
      serverInfo$myUser,
      '@',
      serverInfo$myAddress,
      '-T "cat > ',
      simName,
      '/sub.sh'
    )
  )

  system(
    paste0(
      'sshpass -f ', myPass,
      ' ssh -t ',
      myUser, '@', myAddress,
      ' "echo \'', bash_file, '\' > ', simName, '/sub.sh"'
    )
  )

##