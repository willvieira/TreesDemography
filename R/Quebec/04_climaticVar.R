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

  # allYear = 1955:2018 # Can be from 1900 to 2018
  # #years = allYear[array_id]
  # years = allYear

  # # url address Dan McKenney (ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids)
  # basurl = "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles60/"

  # # Variable to download, among "bio", "cmi", "mint", "maxt", "pcp", "sg"
  # infos = c("bio", "pcp", "cmi")

  # # Length of .tiff files expected for each `info` variable
  # infoSize = c(bio = 19, pcp = 12, cmi = 13)

  # # Resolution either "_300arcsec.zip" or "_60arcsec.zip", 300 = 10km², 60 = 2km²
  # end = "_300arcsec.zip"

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
  # ref_coordinates_bio = raster::coordinates(raster::raster(paste0(mainDir, 'bio/', allYear[1], "/bio_01.tif")))
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
  #   current_raster = raster::raster(paste0(mainDir, 'bio/', allYear[i], "/bio_01.tif"))
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


# Define climate variables
# Options are: `bio`, `cmi`, and `pcp`
clim_variables <- 'bio'

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

  for(var in clim_variables)
  {

    cat('Stacking for variable: ', var, '\n')

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



# Extract climate data for plot locations

  # list to save all variables
  # for each variable, a list for all years with
  # a data.frame plot_xy_id and the variables
  var_pts <- list()
  
  for(var in clim_variables)
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
  for(var in clim_variables)
  {

    cat('Getting adjacent cells for NA values: ', var, '\n')

    # get lat-lon of NA values (from plots that are located at the margin of the climate raster)
    varDf <- subset(var_pts[[var]][[1]], is.na(get(paste0(var, '_01'))), select = ID_PE)

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
  for(var in clim_variables) 
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

  vars_df <- Reduce(
    merge,
    lapply(
      var_pts,
      function(x)
        data.table::rbindlist(x, idcol = '.id')
    )
  )

  vars_df[, year_measured := gsub('.*_', '', .id)]

#



# Correction for temperature variables

  # # All temperature variables must be divided by 10 
  T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

  vars_df[ , (T.var) := lapply(.SD, function(x) x/10), .SDcols = T.var]

  # # Temperature seasonality (bio_04) must be divided by 100
  Tseason <- "bio_04"

  vars_df[ , (Tseason) := lapply(.SD, function(x) x/100), .SDcols = Tseason]

#



# Get cellID
  plot_coord <- plot_xy1 %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    st_coordinates()

  plot_xy1$cellID <- raster::extract(
    stackbio[[1]][[1]],
    plot_coord,
    cellnumbers = TRUE
  )[, 1L] 

#



# Prepare object to average climate data for the years within a deltaYear

  # function to return unique measurement years and the number of years between measurements per plot_id
  diff_yMeasured <- function(years)
  {  
    uq_year <- sort(unique(years))
    yr_diff <- diff(uq_year) - 1

    # as first year won't be used, add zero in the first place
    yr_diff <- c(0, yr_diff)

    return( list(year_measured = uq_year, dif = yr_diff) )
  }
  
  plot_climVars <- as.data.table(tree_data)[, 
    diff_yMeasured(year_measured),
    by = ID_PE
  ]

  # add cellID
  plot_climVars[
    as.data.table(plot_xy1),
    cellID := i.cellID,
    on = 'ID_PE'
  ]

# 



# Rolling average between years within the deltaYear

  roll_average <- function(clim_dt, plotId, year, dif, vars)
  {
    # define the years to mean
    years <- (year - dif):year

    # subset clim data
    dat <- clim_dt[
      ID_PE == plotId &
      year_measured %in% years,
      vars, with = FALSE
    ]
    
    # calculate mean for desirable columns "vars"
    Mean <- apply(dat, 2, mean, na.rm = TRUE)
    SD <- apply(dat, 2, sd, na.rm = TRUE)

    # add vars name and convert all to list (needed by data.table)
    return( 
      as.list(
        c(
          setNames(Mean, paste0(vars, '_mean')),
          setNames(SD, paste0(vars, '_sd'))
        )
      )
    )
  }

  plot_climVars[, rowID := 1:.N]

  out <- plot_climVars[,
    roll_average(
      clim_dt = vars_df,
      plotId = ID_PE,
      year = year_measured,
      dif = dif,
      vars = c('bio_01', 'bio_12')
    ),
    by = rowID
  ]

  plot_climVars <- merge(plot_climVars, out)
  
  plot_climVars[, c('rowID', 'dif') := NULL]

#
