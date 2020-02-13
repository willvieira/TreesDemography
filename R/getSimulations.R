cat('####### Loading simulations from the server #######\n')
###############################
# Load simulations from the server and save in the output folder
# Will Vieira
# January 16, 2020
##############################



##############################
# Steps:
  # Load simulation variables
  # check if simulation exists for each species
  # If not, load simulations from the server
##############################



## Load simulation variables

## Set variables to be simulated

  simInfo <- yaml::read_yaml('_simulation_info.yml')

  # species id
  if(is.null(simInfo$spIds)) {
    spIds <- readRDS('data/spIds.RDS')
  }else {
    spIds <- simInfo$spIds
  }

  # vital rates
  vitalRates <- simInfo$vitalRates

  # simulation name
  simName <- simInfo$simName
  serverDir <- paste0('/', simName, '/MCMC/')
  mainDir <- paste0('output/', simName)

##



## check for species folder and RDS file in the output folder

  if(dir.exists(mainDir) & length(dir(mainDir)) > 0) {

    # Test if all species already have their folder
    localFiles <- dir(mainDir)

    if(length(localFiles) > 0) {

      if(all(spIds %in% localFiles)) {
        foldersToLoad <- NULL
      }else {
        foldersToLoad <- spIds[!(spIds %in% localFiles)]
      }
    }

    # Test if MCMC output file is present in the species folder
    if(length(localFiles) > 0) {

      checkFiles <- expand.grid(localFiles, vitalRates, NA)

      for(i in 1:nrow(checkFiles))
        checkFiles[i, 3] <- ifelse(length(grep(checkFiles[i, 2], dir(paste0(mainDir, checkFiles[i, 1])))) == 0, T, F)
    }
  }else {
    dir.create(mainDir, recursive = TRUE)
    foldersToLoad <- spIds
  }

##



## Load simulations from the server

  # if there was any missing folder, create it and add species to the checkFiles
  if(length(foldersToLoad) > 0)
    invisible(sapply(foldersToLoad, function(x) dir.create(paste0(mainDir, '/', x))))

  # If checkFile exists, get TRUE for those species that need to be loaded and append to foldersToLoad
  # If any species missing, I will import all vital rates outputs
  if(exists('checkFiles'))
    speciesToLoad <- checkFiles$Var1[which(checkFiles$Var3 == TRUE)]

  # Merge folders and species to load in one single species_id vector
  if(length(foldersToLoad > 0)) {
    if(exists('speciesToLoad')) {
      sp_load <- c(foldersToLoad, speciesToLoad)
    }else{
      sp_load <- foldersToLoad
    }
  }else{
    if(exists('speciesToLoad')) {
      sp_load <- speciesToLoad
    }else {
      sp_load <- NULL
    }
  }


  if(!is.null(sp_load)) {

    # load from the server
    serverInfo <- yaml::read_yaml(file = '_serverInfo.yml')
    myPass <- serverInfo$myPass
    myUser <- serverInfo$myUser
    myAddress <- serverInfo$myAddress

    path <- getwd()
    for(sp in sp_load) {
      script <- paste0('sshpass -f ',
                       myPass,
                       ' scp -r ',
                       myUser,
                       '@',
                       myAddress,
                       ':/home/',
                       myUser,
                       '/',
                       serverDir,
                       sp,
                       '/output/. ',
                       path,
                       '/',
                       mainDir,
                       '/',
                       sp,
                       '/')
      system(script)
      cat('   loading sp', which(sp == sp_load), 'of', length(sp_load), '\n')
    }

  }else{
    print('Simulations are already loaded')
  }

##
