##############################
# Extract ANUSPLIN bioclimatic variables 2km2 resolution
# May 29, 2020
##############################


##############################
# Steps
#   - load variables from db
#   - reclassify variables
##############################


# Bioclimatic raster data were obtained directly from McKinney. 
# This script extract climate for all Qc forest plots
# Return the mean of the bioclimatic variables for the year of the plot survey as well as over the last 5 and 10 years before the year of the plot survey.

# Download climate data

  # allYear = 1958:2018 # Can be from 1900 to 2018
  # #years = allYear[array_id]
  # years = allYear

  # # url address Dan McKenney (ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids)
  # basurl = "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles60/"

  # # Variable to download, among "bio", "cmi", "mint", "maxt", "pcp", "sg"
  # infos = c("bio", "pcp", "cmi")

  # # Length of .tiff files expected for each `info` variable
  # infoSize = c(bio = 19, pcp = 12, cmi = 13)

  # # Resolution either "_300arcsec.zip" or "_60arcsec.zip", 300 = 10km², 60 = 2km²
  # end = "_60arcsec.zip"

  # # Where to save data
  # mainFolder = "./rawData/climateData/"

  # if(!dir.exists(mainFolder))
  #     dir.create(mainFolder)


  # ## Download data for years and infos
  # for(info in infos)
  # {
  #     # Create info folder
  #     if (!dir.exists(paste0(mainFolder, info)))
  #       dir.create(paste0(mainFolder, info))

  #     for(year in years)
  #     {
  #         # File name
  #         zout = paste0(mainFolder, info, '/', year, ".zip")

  #         if (!dir.exists(paste0(mainFolder, info, '/', year)))
  #         {
              
  #             myTry = 0
  #             while(myTry < 10)
  #             {
  #                 x = try(download.file(paste0(basurl, info, year, end), destfile = zout, method = "auto", quiet = TRUE))
  #                 if (class(x) == "try-error") {
  #                     # print(paste("ERROR1: ", x, "\n"))
  #                     Sys.sleep(120)
  #                     # print("reconnecting...")
  #                     myTry <- myTry + 1
  #                     print(paste('Try number', myTry))
  #                 } else {
  #                     break
  #                 } 
  #             }

  #             ## Unzip in the folder climateData
  #             unzip(zout, exdir = paste0(mainFolder, info, '/'))
              
  #             ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

  #             if (length(ls_tif) != infoSize[info])
  #             {
  #                 print(paste0("WARNING: unzip function had a problem, year = ", year, "; info = ", info))
  #             }else{
  #                 # remove zip
  #                 file.remove(zout)
  #             }

  #         } else {
              
  #             ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

  #             if (length(ls_tif) != infoSize[info])
  #             {
  #                 ## Unzip in the folder climateData
  #                 unzip(zout, exdir = paste0(mainFolder, info, '/'))

  #                 ls_tif = list.files(path = paste0(mainFolder, info, '/', year), pattern = ".tif")

  #                 if (length(ls_tif) != infoSize[info])
  #                 {
  #                     print(paste0("WARNING: unzip function had a problem, year = ", year, "; info = ", info))
  #                 }else{
  #                     # remove zip
  #                     file.remove(zout)
  #                 }
  #             }
  #         }
  #     }
  # }

#



# Check if climate ref are the same

  # mainDir <- './rawData/climateData/'
  # allYear = 1958:2018
  # nb_years = length(allYear)

  # # Raster reference to check all years
  # ref_coordinates_bio = raster::coordinates(raster::raster(paste0(mainDir, 'bio/', allYear[1], "/bio60_01.tif")))
  # ref_coordinates_cmi = raster::coordinates(raster::raster(paste0(mainDir, 'cmi/', allYear[1], "/cmi60_01.tif")))
  # ref_coordinates_pcp = raster::coordinates(raster::raster(paste0(mainDir, 'pcp/', allYear[1], "/pcp60_01.tif")))

  # ## Check the two references are equal
  # comparison2Ref = all.equal(ref_coordinates_bio, ref_coordinates_pcp, ref_coordinates_cmi)
  # if (!isTRUE(comparison2Ref))
  # {
  #   print(comparison2Ref)
  #   print("Need to choose the bioclim reference for pcp")
  #   print(paste0("InfNorm(diff coordinates) = ", max(abs(ref_coordinates_bio - ref_coordinates_pcp))))
  # }

  # for (i in 2:nb_years)
  # {
  #   print(paste0("year: ", allYear[i]))

  #   # Check within bio
  #   current_raster = raster::raster(paste0(mainDir, 'bio/', allYear[i], "/bio60_01.tif"))
  #   coords = raster::coordinates(current_raster)
  #   print(all.equal(ref_coordinates_bio, coords))

  # # Check within pcp
  #   current_raster = raster::raster(paste0(mainDir, 'cmi/', allYear[i], "/cmi60_01.tif"))
  #   coords = raster::coordinates(current_raster)
  #   print(all.equal(ref_coordinates_cmi, coords))
  
  #   # Check within pcp
  #   current_raster = raster::raster(paste0(mainDir, 'pcp/', allYear[i], "/pcp60_01.tif"))
  #   coords = raster::coordinates(current_raster)
  #   print(all.equal(ref_coordinates_pcp, coords))
  # }

#


# Stack over years for each variable (bio, cmi, pcp)

  ### xy coordinates
  plot_xy_id <- cbind.data.frame(ID_PE = plot_xy$ID_PE, sf::st_coordinates(plot_xy))

  plot_xy1 <- sf::st_as_sf(plot_xy) %>% 
      sf::st_transform(4326) %>% # change projection to match climate raster
      as("Spatial") # require sp object to use raster::extract

  # For each climate variable:
  # Open climate raster files in each folder (year),
  # create a list of climate files,
  # stack them,
  # then create a list of rasterstacks
  mainFolder <- 'rawData/climateData/'

  for(var in dir(mainFolder))
  {

    cat('Stacking for variable: ', var, '\n')

    year_folder <- list.files(paste0(mainFolder, var))
    climate_files <- list.files(paste0(mainFolder, var, '/', year_folder[1]), pattern = '.tif')
    
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



# Extract climate data for plot locations

  # list to save all variables
  # for each variable, a list for all years with
  # a data.frame plot_xy_id and the variables
  var_pts <- list()
  
  for(var in dir('rawData/climateData/'))
  {

    cat('Extracting climate data for variable: ', var, '\n')

    # get stack with all years and variables for the var
    stackVar <- get(paste0('stack', var))

    ynames <- names(stackVar)
    year_pts <- list()
    
    count = 0
    for(y in ynames)
    {
      tmp <- raster::extract(stackVar[[y]], plot_xy1)
      year_pts[[y]] <- cbind(plot_xy_id, tmp)

      # progress
      cat('   In progress...', round(count/length(ynames) * 100, 1), '%\r'); count = count + 1      
    }

    var_pts[[var]] <- year_pts

  }

#



# Correct crazy values

  # get adjacent cells for each NA cell
  crazy_plots <- list()
  var_newCells <- list()
  for(var in dir('rawData/climateData'))
  {

    cat('Getting adjacent cells for NA values: ', var, '\n')

    # get lat-lon of NA values (from plots that are located at the margin of the climate raster)
    varDf <- subset(var_pts[[var]][[1]], is.na(get(paste0(var, '60_01'))), select = ID_PE)

    # add coordinates of missing plots
    varDf <- cbind(varDf, raster::coordinates(plot_xy1[plot_xy$ID_PE %in% varDf$ID_PE, ]))

    # extract cell number on the raster
    varDf[, 'cellNumber'] <- raster::extract(get(paste0('stack', var))[[1]][[1]], varDf[, c('coords.x1', 'coords.x2')], cellnumbers = TRUE)[, 1L]

    # find the nearest adjacent cells with non-NA values
    newCells <- list()
    count = 0
    for (i in 1:nrow(varDf))
    {
      nr <- 3; l <- 1
      while (l)
      {
        mid <- floor(nr/2) + 1
        mat <- matrix(1, nr, nr)
        mat[mid, mid] <- 0
        tmp_cells <- raster::adjacent(get(paste0('stack', var))[[1]][[1]], varDf$cellNumber[i], directions = mat)[, 2L]
        tmp_val <- raster::values(get(paste0('stack', var))[[1]][[1]])[tmp_cells]
        if (!all(is.na(tmp_val))) {
          newCells[[i]] <- tmp_cells[!is.na(tmp_val)]
          l <- 0
        } else nr <- nr + 2
      }
      
      # progress
      cat('   In progress...', round(count/nrow(varDf) * 100, 1), '%\r'); count = count + 1  
    }
    crazy_plots[[var]] <- varDf
    var_newCells[[var]] <- newCells
  }
  
  

  # Replace the NA values by the mean value of adjacent cells
  for(var in dir('rawData/climateData')) 
  {

    cat('Replacing the NA values: ', var, '\n')

    # cell to be fixed
    plotsToFix <- crazy_plots[[var]][, 'ID_PE']
    
    # adjacent cells
    adjCells <- var_newCells[[var]]

    # variable stack
    varStack <- get(paste0('stack', var))
    
    count = 0
    for(plot in 1:length(plotsToFix))
    {
      # adjacent cells
      adjCell <- var_newCells[[var]][[plot]]

      # get mean climate for adjacent cells
      for(yr in 1:length(varStack))
      {
        extVars <- colMeans(raster::extract(varStack[[yr]], adjCell))

        var_pts[[var]][[yr]][which(var_pts[[var]][[yr]]$ID_PE == plotsToFix[plot]), names(extVars)] <- extVars

        # progress
        cat('   In progress...', round(count/(length(plotsToFix) * length(varStack)) * 100, 1), '%\r'); count = count + 1
      }
    }
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
  T.var <- c("bio60_01", "bio60_02", "bio60_05", "bio60_06", "bio60_07", "bio60_08", "bio60_09", "bio60_10", "bio60_11")

  # # Temperature seasonality (bio_04) must be divided by 100
  Tseason <- "bio60_04"

  vars_df[[1]] <- vars_df[[1]] %>% 
    dplyr::mutate_at(T.var, funs(./10)) %>%
    dplyr::mutate_at(Tseason, funs(./100))

  vars_df[[1]] <- as.data.table(vars_df[[1]])

#



# Get cellID
  plot_coord <- plot_xy1 %>%
    st_transform(4326) %>%
    st_coordinates()

  plot_xy1$cellID <- raster::extract(
    stackbio[[1]][[1]],
    plot_coord,
    cellnumbers = TRUE
  )[, 1L] 


#



# Long format for rolling average

  IDVAR <- c('.id', 'ID_PE', 'X', 'Y')
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
                                              bio_mean5 = frollmean(bio, n = 5, na.rm = TRUE, align = "right", fill = NA), 
                                              bio_mean10 = frollmean(bio, n = 10, na.rm = TRUE, align = "right", fill = NA)),
                                              by = list(ID_PE, bio_var)]

  vars_df_roll[[1]][, year := as.integer(gsub('bio_', '', year))]
  
  # cmi
  vars_df_roll[[2]] <- vars_df_long[[2]][,list(year = .id,
                                              cmi = cmi,
                                              cmi_mean5 = frollmean(cmi, n = 5, na.rm = TRUE, align = "right", fill = NA), 
                                              cmi_mean10 = frollmean(cmi, n = 10, na.rm = TRUE, align = "right", fill = NA)),
                                              by = list(ID_PE, cmi_var)]

  vars_df_roll[[2]][, year := as.integer(gsub('cmi_', '', year))]

  # pcp
  vars_df_roll[[3]] <- vars_df_long[[3]][,list(year = .id,
                                              pcp = pcp,
                                              pcp_mean5 = frollmean(pcp, n = 5, na.rm = TRUE, align = "right", fill = NA), 
                                              pcp_mean10 = frollmean(pcp, n = 10, na.rm = TRUE, align = "right", fill = NA)),
                                              by = list(ID_PE, pcp_var)]

  vars_df_roll[[3]][, year := as.integer(gsub('pcp_', '', year))]


#



# Spreed climate data and merge all three variables into one single df with all info for each plot_id

  # bio
  bio <- tree_data %>%
      dplyr::select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
      dplyr::distinct() %>%
      dplyr::left_join(vars_df_roll[[1]], by = c("ID_PE" = "ID_PE", "year_measured" = "year"))

  # cmi
  cmi <- tree_data %>%
      dplyr::select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
      dplyr::distinct() %>%
      dplyr::left_join(vars_df_roll[[2]], by = c("ID_PE" = "ID_PE", "year_measured" = "year"))

  # cmi
  pcp <- tree_data %>%
      dplyr::select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
      dplyr::distinct() %>%
      dplyr::left_join(vars_df_roll[[3]], by = c("ID_PE" = "ID_PE", "year_measured" = "year"))

  
  # spreed for each value (mean0, mean5, mean10)
  names(bio) <- c(names(bio)[1:5], paste0('value', c(0, 5, 10)))
  bio_s <- bio %>%
      tidyr::pivot_wider(names_from = bio_var, values_from = c(value0, value5, value10))
  
  names(cmi) <- c(names(cmi)[1:5], paste0('value', c(0, 5, 10)))
  cmi_s <- cmi %>%
      tidyr::pivot_wider(names_from = cmi_var, values_from = c(value0, value5, value10))
  
  names(pcp) <- c(names(pcp)[1:5], paste0('value', c(0, 5, 10)))
  pcp_s <- pcp %>%
      tidyr::pivot_wider(names_from = pcp_var, values_from = c(value0, value5, value10))
  
  
  # merge all variables
  allVar_df <- bio_s %>%
      dplyr::full_join(cmi_s, by = c("ID_PE", "ID_PE_MES", "plot_id", "year_measured")) %>%
      dplyr::full_join(pcp_s, by = c("ID_PE", "ID_PE_MES", "plot_id", "year_measured"))

#
