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

##



## Send simulations

  myPass <- serverInfo$myPass
  myUser <- serverInfo$myUser
  myAddress <- serverInfo$myAddress

  script <- paste0('sshpass -f ',
                   myPass,
                   ' scp -r ',
                   getwd(),
                   ' ',
                   myUser,
                   '@',
                   myAddress,
                   ':/home/',
                   myUser,
                   '/',
                   simName)

  system(script)

##
