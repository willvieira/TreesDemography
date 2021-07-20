##############################
# Extract ANUSPLIN bioclimatic variables 2km2 resolution
# Nov 6, 2020
##############################



##############################
# Steps
# - Download data
# - Check if geom_ref is the same of all variables
# - Stack over year
# - Feed missing years (2019 only) with information from year-1
# - Extract climate data for plot locations
# - Merge lists of year into one data.frame
# - Correction for temperature variables
# - Long format for rolling average
# - Rolling average
# - Spreed climate data and merge all three variables into one single df with all info for each plot_id
# - Remove correlated variables (> .75)
# - Merge climate data with tree and reg dt
##############################


library(sf)
library(raster)


# Bioclimatic raster data were obtained directly from McKinney. 
# This script extract climate for all Qc forest plots
# Return the mean of the bioclimatic variables for the year of the plot survey as well as over the last 5 and 10 years before the year of the plot survey.

# Download climate data

  allYear = 1972:2018 # Can be from 1900 to 2018
  #years = allYear[array_id]
  years = allYear

  # url address Dan McKenney (ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids)
  basurl = "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles300/"

  # Variable to download, among "bio", "cmi", "mint", "maxt", "pcp", "sg"
  infos = c("bio", "cmi")

  # Length of .tiff files expected for each `info` variable
  infoSize = c(bio = 19, cmi = 13)

  # Resolution either "_300arcsec.zip" or "_60arcsec.zip", 300 = 10km², 60 = 2km²
  end = "_300arcsec.zip"

  # Where to save data
  mainFolder = "./rawData/climateData/"

  if(!dir.exists(mainFolder))
      dir.create(mainFolder)


  ## Download data for years and infos
  for(info in infos)
  {
      # Create info folder
      if (!dir.exists(paste0(mainFolder, info)))
        dir.create(paste0(mainFolder, info))

      for(year in years)
      {
          # File name
          zout = paste0(mainFolder, info, '/', year, ".zip")

          if (!dir.exists(paste0(mainFolder, info, '/', year)))
          {
              
              myTry = 0
              while(myTry < 10)
              {
                  x = try(download.file(paste0(basurl, info, year, end), destfile = zout, method = "auto", quiet = TRUE))
                  if (class(x) == "try-error") {
                      # print(paste("ERROR1: ", x, "\n"))
                      Sys.sleep(120)
                      # print("reconnecting...")
                      myTry <- myTry + 1
                      print(paste('Try number', myTry))
                  } else {
                      break
                  } 
              }

              ## Unzip in the folder climateData
              unzip(zout, exdir = paste0(mainFolder, info, '/'))
              
              ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

              if (length(ls_tif) != infoSize[info])
              {
                  print(paste0("WARNING: unzip function had a problem, year = ", year, "; info = ", info))
              }else{
                  # remove zip
                  file.remove(zout)
              }

          } else {
              
              ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

              if (length(ls_tif) != infoSize[info])
              {
                  ## Unzip in the folder climateData
                  unzip(zout, exdir = paste0(mainFolder, info, '/'))

                  ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

                  if (length(ls_tif) != infoSize[info])
                  {
                      print(paste0("WARNING: unzip function had a problem, year = ", year, "; info = ", info))
                  }else{
                      # remove zip
                      file.remove(zout)
                  }
              }
          }
      }
  }

#



# Check if climate ref are the same

  mainDir <- './rawData/climateData/'
  allYear = 1972:2018
  nb_years = length(allYear)

  # Raster reference to check all years
  ref_coordinates_bio = raster::coordinates(raster::raster(paste0(mainDir, 'bio/', allYear[1], "/bio_01.asc")))
  ref_coordinates_cmi = raster::coordinates(raster::raster(paste0(mainDir, 'cmi/', allYear[1], "/cmi_01.asc")))
  #ref_coordinates_pcp = raster::coordinates(raster::raster(paste0(mainDir, 'pcp/', allYear[1], "/pcp_01.asc")))

  ## Check the two references are equal
  # comparison2Ref = all.equal(ref_coordinates_bio, ref_coordinates_pcp, ref_coordinates_cmi)
  # if (!isTRUE(comparison2Ref))
  # {
  #   print(comparison2Ref)
  #   print("Need to choose the bioclim reference for pcp")
  #   print(paste0("InfNorm(diff coordinates) = ", max(abs(ref_coordinates_bio - ref_coordinates_pcp))))
  # }

  for (i in 2:nb_years)
  {
    print(paste0("year: ", allYear[i]))

    # Check within bio
    current_raster = raster::raster(paste0(mainDir, 'bio/', allYear[i], "/bio_01.asc"))
    coords = raster::coordinates(current_raster)
    print(all.equal(ref_coordinates_bio, coords))

  # Check within cmi
    current_raster = raster::raster(paste0(mainDir, 'cmi/', allYear[i], "/cmi_01.asc"))
    coords = raster::coordinates(current_raster)
    print(all.equal(ref_coordinates_cmi, coords))
  
    # # Check within pcp
    # current_raster = raster::raster(paste0(mainDir, 'pcp/', allYear[i], "/pcp_01.asc"))
    # coords = raster::coordinates(current_raster)
    # print(all.equal(ref_coordinates_pcp, coords))
  }

#





# Stack over years for each variable (bio, cmi, pcp)

  # For each climate variable:
  # Open climate raster files in each folder (year),
  # create a list of climate files,
  # stack them,
  # then create a list of rasterstacks
  mainFolder <- 'rawData/climateData/'

  for(var in dir(mainFolder))
  {

    cat('Stacking for variable:', var, '\n')

    year_folder <- list.files(paste0(mainFolder, var))
    climate_files <- list.files(paste0(mainFolder, var, '/', year_folder[1]), pattern = '.asc')
    
    varList <- list()
    yearList <- list()
    
    # progress bar
    nTotal <- length(year_folder) * length(climate_files)
    count = 0
    
    for(year in year_folder)
    {
      for(File in climate_files)
      {
        bname <- strsplit(File, ".", fixed = TRUE)[[1]][1]
        tmp <- raster::raster(paste0(mainFolder, var, "/", year, "/", File))
        yearList[[bname]] <- tmp

        # progress
        cat('   In progress...', round(count/nTotal * 100, 1), '%\r'); count = count + 1
      }

      yname <- paste(var, year, sep = "_")
      varList[[yname]] <- raster::stack(yearList)
    }

    ### Save the climate raster for each variable in a list
    assign(paste0('stack', var), varList)

  }
 
#




# Feed missing years with information from year-1
  
  # We have inventory data for 2019 but climate data ends on 2018
  # I will feed the year 2019 with data from 2018
  stackbio[['bio_2019']] <- stackbio[['bio_2018']]
  stackcmi[['cmi_2019']] <- stackcmi[['cmi_2018']]

#



# Extract climate data for plot locations

  plot_xy1 <- plot_location %>%
    dplyr::select(PLACE, longitude, latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    as("Spatial")

  # list to save all variables
  # for each variable, a list for all years with
  # a data.frame plot_xy_id and the variables
  var_pts <- list()
  
  for(var in dir('rawData/climateData/'))
  {

    cat('Extracting climate data for variable:', var, '\n')

    # get stack with all years and variables for the var
    stackVar <- get(paste0('stack', var))

    ynames <- names(stackVar)
    year_pts <- list()
    
    count = 0
    for(y in ynames)
    {
      tmp <- raster::extract(stackVar[[y]], plot_xy1)
      year_pts[[y]] <- cbind(plot_location[, c('PLACE', 'longitude', 'latitude'), with = FALSE], tmp)

      # progress
      cat('   In progress...', round(count/length(ynames) * 100, 1), '%\r'); count = count + 1      
    }

    var_pts[[var]] <- year_pts

  }

#




# Merge lists of year into one data.frame
  # There will be still one main list with the three variables

  vars_df <- list()
  for(var in dir('rawData/climateData'))
    vars_df[[var]] <- data.table::rbindlist(var_pts[[var]], idcol = '.id')

#




# Correction for temperature variables

  # # All temperature variables must be divided by 10 
  T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

  # # Temperature seasonality (bio_04) must be divided by 100
  Tseason <- "bio_04"

  vars_df[[1]] <- vars_df[[1]] %>% 
    dplyr::mutate_at(T.var, funs(./10)) %>%
    dplyr::mutate_at(Tseason, funs(./100))

  vars_df[[1]] <- as.data.table(vars_df[[1]])

#




# Long format for rolling average

  IDVAR <- c('.id', 'PLACE', 'longitude', 'latitude')
  vars_df_long <- list()
  
  for(var in dir('rawData/climateData'))
  {
    
    varNames <- names(vars_df[[var]])[!names(vars_df[[var]]) %in% IDVAR]
    vars_df_long[[var]] <- data.table::melt(vars_df[[var]], id.vars = IDVAR,
                                            measure.vars = varNames,
                                            variable.name = paste0(var, '_var'),
                                            value.name = var)
  }

#




# Rolling average
  # for 5 years before and 10 years before

  vars_df_roll <- list()
  
  # bio
  vars_df_roll[[1]] <- vars_df_long[[1]][,list(year = .id,
                                              bio = bio,
                                              bio_mean5 = frollmean(bio, n = 5, na.rm = TRUE, align = "right", fill = NA)),
                                              by = list(PLACE, bio_var)]

  vars_df_roll[[1]][, year := as.integer(gsub('bio_', '', year))]
  
  # cmi
  vars_df_roll[[2]] <- vars_df_long[[2]][,list(year = .id,
                                              cmi = cmi,
                                              cmi_mean5 = frollmean(cmi, n = 5, na.rm = TRUE, align = "right", fill = NA)),
                                              by = list(PLACE, cmi_var)]

  vars_df_roll[[2]][, year := as.integer(gsub('cmi_', '', year))]



  # fix class of PLACE column
  vars_df_roll[[1]][, PLACE := as.numeric(PLACE)]
  vars_df_roll[[2]][, PLACE := as.numeric(PLACE)]

#




# Spreed climate data and merge all three variables into one single df with all info for each plot_id

  # bio
  bio <- tree_data %>%
      dplyr::select(plot_id, year) %>%
      dplyr::distinct() %>%
      dplyr::left_join(vars_df_roll[[1]], by = c("plot_id" = "PLACE", "year" = "year"))

  # cmi
  cmi <- tree_data %>%
      dplyr::select(plot_id, year) %>%
      dplyr::distinct() %>%
      dplyr::left_join(vars_df_roll[[2]], by = c("plot_id" = "PLACE", "year" = "year"))


  # spreed for each value (mean0, mean5, mean10)
  names(bio) <- c(names(bio)[1:3], paste0('value', c(0, 5)))
  bio_s <- bio %>%
      tidyr::pivot_wider(names_from = bio_var, values_from = c(value0, value5))
  
  names(cmi) <- c(names(cmi)[1:3], paste0('value', c(0, 5)))
  cmi_s <- cmi %>%
      tidyr::pivot_wider(names_from = cmi_var, values_from = c(value0, value5))
  
  
  # merge all variables
  allVar_df <- bio_s %>%
      dplyr::full_join(cmi_s, by = c("plot_id", "year"))

#



# Remove correlated variables (> .75)
  
  # corVar <- cor(allVar_df[, setdiff(names(allVar_df), c('plot_id', 'year', grep('value0', names(allVar_df), value = T)))])
  # corVar2 <- corVar
  # corVar2[which(abs(corVar2) < 0.75)] <- NA
  # corrplot::corrplot(corVar2, type = 'upper')

  # # Also remove value0 and keep only 5 years lag
  # varsToRm <- c(grep('value0', names(allVar_df), value = T),
  #               'value5_bio_05', 'value5_bio_06', 'value5_bio_07', 'value5_bio_10', 'value5_bio_11', 'value5_bio_12', 'value5_bio_13', 'value5_bio_16', 'value5_bio_17', 'value5_bio_19')

  # allVar_df <- allVar_df[, setdiff(names(allVar_df), varsToRm), with = FALSE]

#



# Merge climate data with tree and reg dt

  tree_data <- merge(tree_data, allVar_df, by = c('plot_id', 'year'))
  reg_data <- merge(reg_data, allVar_df, by = c('plot_id', 'year'))

#


# Unload raster package
detach("package:raster", unload=TRUE)
