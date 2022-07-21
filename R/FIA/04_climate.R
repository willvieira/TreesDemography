#------------------------------------------------------
# import ANUSPLIN bioclimatic variables
# Will Vieira
# July 16, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Stack over years for each variable (bio, cmi, pcp)
#   - 

#------------------------------------------------------


library(data.table)
library(dplyrqq)
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


for(var in dir(mainFolder))
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

for(var in dir('rawData/climateData/'))
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


# Merge lists of year into one data.frame
# There will be still one main list with the main variables (bio, cmi)

vars_df <- list()
for(var in dir('rawData/climateData'))
  vars_df[[var]] <- data.table::rbindlist(var_pts[[var]], idcol = '.id')






#------------------------------------------------------
#------------------------------------------------------

# Correction for temperature variables

#------------------------------------------------------
#------------------------------------------------------

# # All temperature variables must be divided by 10 
T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

# # Temperature seasonality (bio_04) must be divided by 100
Tseason <- "bio_04"

vars_df[[1]] <- vars_df[[1]] %>% 
  dplyr::mutate_at(T.var, funs(./10)) %>%
  dplyr::mutate_at(Tseason, funs(./100))






#------------------------------------------------------
#------------------------------------------------------

# Rolling average

#------------------------------------------------------
#------------------------------------------------------

# Long format
IDVAR <- c('.id', 'plot_id', 'longitude', 'latitude')
vars_df_long <- list()

for(var in dir('rawData/climateData'))
{  
  varNames <- names(vars_df[[var]])[!names(vars_df[[var]]) %in% IDVAR]
  vars_df_long[[var]] <- data.table::melt(
    vars_df[[var]],
    id.vars = IDVAR,
    measure.vars = varNames,
    variable.name = paste0(var, '_var'),
    value.name = var
  )
}

# Rolling average for the last 5 years
vars_df_roll <- list()
  
# bio
vars_df_roll[[1]] <- vars_df_long[[1]][,
  list(
    year = .id,
    bio = bio,
    bio_mean5 = frollmean(
      bio,
      n = 5,
      na.rm = TRUE,
      align = "right",
      fill = NA
    )
  ),
  by = .(plot_id, bio_var)
]
vars_df_roll[[1]][, year := as.integer(gsub('bio_', '', year))]
  
# cmi
vars_df_roll[[2]] <- vars_df_long[[2]][,
  list(
    year = .id,
    cmi = cmi,
    cmi_mean5 = frollmean(
      cmi,
      n = 5,
      na.rm = TRUE,
      align = "right",
      fill = NA
    )
  ),
  by = .(plot_id, cmi_var)
]
vars_df_roll[[2]][, year := as.integer(gsub('cmi_', '', year))]


# back to wide format
bio_wide = dcast(
  vars_df_roll[[1]][, .(plot_id, bio_var, year, bio_mean5)],
  plot_id + year ~ bio_var, value.var = 'bio_mean5'
)

cmi_wide = dcast(
  vars_df_roll[[2]][, .(plot_id, cmi_var, year, cmi_mean5)],
  plot_id + year ~ cmi_var, value.var = 'cmi_mean5'
)

clim_dt <- merge(
  bio_wide,
  cmi_wide,
  by = c('plot_id', 'year')
)

# save
saveRDS(clim_dt, 'data/FIA/clim_dt.RDS')


#------------------------------------------------------
#------------------------------------------------------

# Merge with treeData

#------------------------------------------------------
#------------------------------------------------------

treeData <- merge(
  treeData,
  clim_dt[, .(plot_id, year, bio_01, bio_12)],
  by.x = c('plot_id', 'year_measured'),
  by.y = c('plot_id', 'year')
)


saveRDS(treeData, 'data/FIA/treeData_sStar_clim.RDS')
