cat('####### Loading simulations from the server #######\n')
###############################
# Load simulations from the server and save in the output folder
# Will Vieira
# January 16, 2020
##############################



##############################
# Steps:
  # load config file
  # Load simulation variables
  # check if simulation exists for each species
  # If not, load simulations from the server
##############################



## Load conf file with server info

  server_info <- readLines('_server.yml')

##



## Load simulation variables

  # species_id
  spIds <- readRDS('data/spIds.RDS')

  # vitalRates
  eval(parse(text = readLines('R/createFolderToRun.R')[33]))

  serverDir <- '/TreesDemography/MCMC/'
  mainDir <- 'output/'
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
    dir.create(mainDir)
    foldersToLoad <- spIds
  }

##



## Load simulations from the server

  # if there was any missing folder, create it and add species to the checkFiles
  if(length(foldersToLoad) > 0)
    invisible(sapply(foldersToLoad, function(x) dir.create(paste0(mainDir, x))))

  # If checkFile exists, get TRUE for those species that need to be loaded and append to foldersToLoad
  # If any species missing, I will import all vital rates outputs
  if(exists('checkFiles'))
    speciesToLoad <- checkFiles$Var1[which(checkFiles$Var3 == TRUE)]


  if(length(foldersToLoad) > 0 & exists('speciesToLoad')) {

    # merge all needed species and load
    speciesToLoad <- c(foldersToLoad, speciesToLoad)

    # load from the server
    myPass <- Sys.getenv('MYPASS')
    myUser <- Sys.getenv('MYUSER')
    myAddress <- Sys.getenv('MYADDRESS')

    path <- getwd()

    for(sp in speciesToLoad) {
      script <- paste0('sshpass -p ',
                       myPass,
                       ' scp ',
                       myUser,
                       '@',
                       myAddress,
                       serverDir,
                       sp,
                       '/.
                       ',
                       path,
                       '/',
                       mainDir,
                       sp,
                       '/')
      system(script)
    }

  }else{
    print('Simulations are already loaded')
  }

##
