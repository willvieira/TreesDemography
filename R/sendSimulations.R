cat('####### Sending simulations to the server #######\n')
###############################
# Send folder to run simulations on the server
# Will Vieira
# February 8, 2020
##############################



##############################
# Steps:
  # Load simulation variables
  # Send simulations
##############################


## Set variables to be simulated

  serverInfo <- yaml::read_yaml('_serverInfo.yml')
  simName <- yaml::read_yaml('_simulation_info.yml')$simName
  vitalRate <- yaml::read_yaml('_simulation_info.yml')$vitalRates

##



## Folders and files

  filesToSend <- c('_simulation_info.yml', '_rawDataLink', paste0('data/', vitalRate, '_dt.RDS'))
  foldersToSend <- c('R', 'stan')
##



## Send simulations

  myPass <- serverInfo$myPass
  myUser <- serverInfo$myUser
  myAddress <- serverInfo$myAddress
  
  # first create same folder on the server side
  system(paste0('sshpass -f ', myPass, ' ssh -t ', myUser, '@', myAddress, ' "mkdir -p ', simName, '/data/quebec"'))

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
