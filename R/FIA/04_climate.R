#------------------------------------------------------
# import ANUSPLIN bioclimatic variables
# Will Vieira
# July 16, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Stack over years for each variable (bio, cmi)
#   - Extract climate data for plot locations
#   - Fill NA plots with adjacent information
#   - Fix climate units
#   - Rolling average of 5 years
#------------------------------------------------------


library(data.table)
library(dplyr)
library(sf)
library(raster)

treeData <- readRDS('data/FIA/treeData_sStar.RDS')





# Bioclimatic raster data were obtained directly from McKinney. 
# Download, extraction, and checks were already performed in the R/Quebec/climate.R script



#------------------------------------------------------
#------------------------------------------------------

# Stack over years for each variable (bio, cmi)

#------------------------------------------------------
#------------------------------------------------------

# For each climate variable:
# Open climate raster files in each folder (year),
# create a list of climate files,
# stack them,
# then create a list of rasterstacks
mainFolder <- file.path('rawData', 'climateData')


# define first year to avoid loading unnecessary years
# -6 because of rolling avarange from past 5 years
minYear <- treeData[, min(year_measured) - 6]


# Define climate variables
# Options are: `bio`, `cmi`, and `pcp`
clim_variables <- 'bio'


for(var in clim_variables)
{

    cat('Stacking for variable:', var, '\n')

    # get all years
    year_folder <- list.files(
        file.path(
            mainFolder,
            var
        )
    )

    # filter for necessary years only
    year_folder <- year_folder[
        as.numeric(year_folder) %in% minYear:max(as.numeric(year_folder))
    ]

    #get all variables
    climate_files <- list.files(
        file.path(
            mainFolder,
            var,
            year_folder[1]
        ),
        pattern = '.asc'
    )

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
            tmp <- raster::raster(
                file.path(
                    mainFolder,
                    var,
                    year,
                    File
                )
            )
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





#------------------------------------------------------
#------------------------------------------------------

# Extract climate data for plot locations

#------------------------------------------------------
#------------------------------------------------------

plot_location <- treeData[,
    head(.SD, 1),
    by = plot_id,
    .SDcols = c('longitude', 'latitude')
]

plot_xy <- plot_location %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  as("Spatial")

# list to save all variables
# for each variable, a list for all years with
# a data.frame plot_xy_id and the variables
var_pts <- list()

for(var in clim_variables)
{

  cat('Extracting climate data for variable:', var, '\n')

  ynames <- names(get(paste0('stack', var)))
  year_pts <- list()
  
  count = 0
  for(y in ynames)
  {
    tmp <- raster::extract(
      get(paste0('stack', var))[[y]],
      plot_xy
    )
    
    year_pts[[y]] <- cbind(
      plot_location,
      tmp
    )

    # progress
    cat('   In progress...', round(count/length(ynames) * 100, 1), '%\r'); count = count + 1      
  }

  var_pts[[var]] <- year_pts

}




#------------------------------------------------------
#------------------------------------------------------

# Some plots have NA climate cells
# Fill it with the mean of the adjacent cells

#------------------------------------------------------
#------------------------------------------------------

na_plots <- list()
var_newCells <- list()

for(var in clim_variables)
{

  cat('Getting adjacent cells for NA values: ', var, '\n')

  # Get plots and their location with climate NA
  missing_clim <- var_pts[[var]][[1]][is.na(get(paste0(var, '_01'))), .(plot_id, longitude, latitude)]

  # extract cell number on the raster
  missing_clim[, 'cellNumber'] <- raster::extract(
    get(paste0('stack', var))[[1]][[1]],
    missing_clim[, c('longitude', 'latitude')],
    cellnumbers = TRUE
  )[, 1L]

  # find the nearest adjacent cells with non-NA values
  newCells <- list()
  count = 0
  for (i in which(!is.na(missing_clim$cellNumber)))
  {
    nr <- 3; l <- 1
    while (l)
    {
      mid <- floor(nr/2) + 1
      mat <- matrix(1, nr, nr)
      mat[mid, mid] <- 0
      
      tmp_cells <- raster::adjacent(
        get(paste0('stack', var))[[1]][[1]],
        missing_clim$cellNumber[i], directions = mat
      )[, 2L]

      tmp_val <- raster::values(
        get(paste0('stack', var))[[1]][[1]]
      )[tmp_cells]

      if (!all(is.na(tmp_val))) {
        newCells[[i]] <- tmp_cells[!is.na(tmp_val)]
        l <- 0
      } else nr <- nr + 2
    }
    
    # progress
    cat('   In progress...', round(count/length(which(!is.na(missing_clim$cellNumber))) * 100, 1), '%\r'); count = count + 1  
  }
  na_plots[[var]] <- missing_clim
  var_newCells[[var]] <- newCells

}



# Replace the NA values by the mean value of adjacent cells
for(var in clim_variables) 
{
  cat('Replacing the NA values: ', var, '\n')

  # cell to be fixed
  plotsToFix <- na_plots[[var]][, 'plot_id']
  
  # adjacent cells
  adjCells <- var_newCells[[var]]

  # variable stack
  varStack <- get(paste0('stack', var))

  count = 0
  for(plot in which(!unlist(lapply(adjCells, is.null))))
  {
    # adjacent cells
    adjCell <- var_newCells[[var]][[plot]]

    # get mean climate for adjacent cells
    for(yr in 1:length(varStack))
    {
      # get adj values
      extVars <- colMeans(
        raster::extract(
          varStack[[yr]],
          adjCell
        )
      )

      # fill NAs
      var_pts[[var]][[yr]][
        plot_id %in% plotsToFix$plot_id[plot],
        names(extVars) := as.list(extVars)
      ]

      # progress
      cat('   In progress...', round(count/(length(which(!unlist(lapply(adjCells, is.null)))) * length(varStack)) * 100, 1), '%\r'); count = count + 1
    }
  }
}


# Merge lists of year into one data.frame
vars_df <- Reduce(
  merge,
  lapply(
    var_pts,
    function(x)
      data.table::rbindlist(x, idcol = '.id')
  )
)



#------------------------------------------------------
#------------------------------------------------------

# Fix climate units

#------------------------------------------------------
#------------------------------------------------------

# All temperature variables must be divided by 10 
T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

vars_df[ , (T.var) := lapply(.SD, function(x) x/10), .SDcols = T.var]

# # Temperature seasonality (bio_04) must be divided by 100
Tseason <- "bio_04"

vars_df[ , (Tseason) := lapply(.SD, function(x) x/100), .SDcols = Tseason]





#------------------------------------------------------
#------------------------------------------------------

# Assign the climate cell ID to the plot so I can trace
# Which plots are using the same source of information

#------------------------------------------------------
#------------------------------------------------------

plot_location$cellID <- raster::extract(
  stackbio[[1]][[1]],
  plot_location[, c('longitude', 'latitude')],
  cellnumbers = TRUE
)[, 1L] 





#------------------------------------------------------
#------------------------------------------------------

# Rolling average

#------------------------------------------------------
#------------------------------------------------------


# Prepare object to average climate data for the years within a deltaYear

# function to return unique measurement years and the number of years between measurements per plot_id
diff_yMeasured <- function(years)
{  
  uq_year <- sort(unique(years))
  yr_diff <- diff(uq_year) - 1

  # as first year won't be used, add zero in the first place
  yr_diff <- c(0, yr_diff)

  return( list(year_measured = uq_year, diff = yr_diff) )
}

plot_climVars <- treeData[, 
  diff_yMeasured(year_measured),
  by = plot_id
]

# add cellID
plot_climVars[
  plot_location,
  cellID := i.cellID,
  on = 'plot_id'
]



# Rolling average between years within the deltaYear
roll_average <- function(clim_dt, plotId, year, diff, vars)
{
  # define the years to mean
  years <- (year - diff):year

  # subset clim data
  dat <- clim_dt[plot_id == plotId & year %in% years, vars, with = FALSE]
  
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
    plotId = plot_id,
    year = year_measured,
    diff = diff,
    vars = c('bio_01', 'bio_12')
  ),
  by = rowID
]

plot_climVars <- merge(plot_climVars, out, by = 'rowID')

plot_climVars[, c('rowID', 'diff') := NULL]

# save
saveRDS(plot_climVars, 'data/FIA/clim_dt.RDS')





#------------------------------------------------------
#------------------------------------------------------

# Merge with treeData

#------------------------------------------------------
#------------------------------------------------------

treeData[
  plot_climVars,
  `:=`(
    bio_01_mean = i.bio_01_mean,
    bio_01_sd = i.bio_01_sd,
    bio_12_mean = i.bio_12_mean,
    bio_12_sd = i.bio_12_sd,
    climate_cellID = i.cellID
  ),
  on = c(plot_id = 'plot_id', year_measured = 'year_measured')
]

saveRDS(treeData, 'data/FIA/treeData_sStar_clim.RDS')
