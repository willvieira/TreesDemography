cat('####### Loading simulations from the server #######\n')
###############################
# Load simulations from the server and save in the output folder
# Will Vieira
# January 16, 2020
# last modified: nov 2022
##############################



##############################
# Steps:
  # Load simulation variables
  # check if simulation exists for each species
  # If not, load simulations from the server
##############################



## load simulation and server info
  
  model_dir <- 'model_recruitment/intcpt_plot_comp'
  serverInfo <- yaml::read_yaml('_serverInfo.yml')
  simInfo <- yaml::read_yaml(file.path(model_dir, '_simulation_info.yml'))
  
  simName <- simInfo$simName
  vitalRates <- simInfo$vitalRates
  spIds <- simInfo$spIds

  serverDir <- paste0('/scratch/', vitalRates, '/', simName, '/output/*')
  mainDir <- paste0('output_sim/', vitalRates, '/', simName)

  myPass <- serverInfo$myPass
  myUser <- serverInfo$myUser
  myAddress <- paste0(simInfo$cluster, '.', serverInfo$myAddress)

##



## check for species folder and RDS file in the output folder

  if(dir.exists(mainDir) & length(dir(mainDir)) > 0) {

    # Test if all species already have their folder
    localFiles <- dir(mainDir)

    # check if all spIds have their files
    spPresent = sapply(spIds, function(x) length(grep(x, localFiles)))

    # define missing species
    if(any(spPresent == 0)) {
      toLoad = TRUE
    }else{
      toLoad = FALSE
    }
  }else {
    dir.create(mainDir, recursive = TRUE)
    toLoad = TRUE
  }

##



## Load simulations from the server

  if(toLoad)
  {
    path <- getwd()
    script <- paste0(
       'sshpass -f ',
        myPass,
        ' scp -r ',
        myUser,
        '@',
        myAddress,
        ':/home/',
        myUser,
        serverDir,
        ' ',
        path,
        '/',
        mainDir,
        '/'
    )
    system(script)
  }else{
    print('Simulations are already loaded')
  }

##