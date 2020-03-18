cat('####### cleaning raw data #######\n')
###############################
# Clean raw data
# Will Vieira (with lots of code kindly shared by Amaēl)
# February 05, 2019
# Last update: January 16, 2020
##############################


##############################
# Steps:
  # Load data
  # Calculate basal area (plot and species level)
  # Remove some problematic species
  # Remove individuals with climate NA
  # Remove character `-` in species_id's
  # Separete between Growth, mortality and fecundity data

    # For growth
      # Rewrite data table in a transition way
      # Keep species with more than 1e4 individuals
      # Calculate canopyStatus and canopyDistance

    # For mortality
      # Keep the same species as in the growth filter above
      # Remove trees already died in first measurement
      # Keep only the first death record
      # Fix height problem
      # Rewrite data table in a transition way
      # Remove negative growth
      # Remove extreme deltaYear (2 < deltaYear < 15)
      # Check if above cleaning worked
      # Calculate canopyStatus and canopyDistance

    # For fecundity
      # Keep the same species as in the growth filter above

  # Filter species_id to keep species with at least 2e4 measurements
  # save growth, mortality and fecundity databases
##############################



suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))



# load data

  # prepare directories
  dir.create('rawData')
  dir.create('data')

  # download data (unfortunately not shareable)
  rawDataLink <- readLines('_rawDataLink')
  rawData = 'rawData/tree_clim_sStar.RDS'

  # check if data is already dowloaded, and if not download it
  if(!file.exists(rawData))
    download.file(rawDataLink, rawData, method = 'auto', quiet = TRUE)

  # load
  treeData = readRDS(rawData)

#




# calculate basal area total and by species_id
# remove plots with basal area larger than 400 m2/ha


# calculate basal area of plot for all species (BA) and by species_id (BA_sp)

  # First remove plot_ids with more than one measures of plot_size
  treeData[, ln_plotSize := length(unique(plot_size)), by = plot_id]
  treeData <- treeData[ln_plotSize == 1]
  treeData[, ln_plotSize := NULL]

  # individual basal area
  treeData[, indBA := pi * (dbh^2) / 4000000]

  # plot basal area
  # calculate plot basal area (BA in m2/ha)
  treeData[is_dead == 'f', BA := sum(indBA) * 1e4/plot_size, by = list(year_measured, plot_id)]

  # fill NAs of BA (due to dead trees) with the value from the plot
  # NAs will still persist in plots where all individuals are dead in a specific year
  treeData[, BA := nafill(BA, "locf"), by = list(year_measured, plot_id)]
  treeData[, BA := nafill(BA, "nocb"), by = list(year_measured, plot_id)]
  # For persistent NA where all individuals of the plot are dead in a year)
  # That means that there are not competing individuals, so BA is iqual 0
  treeData[, BA := nafill(BA, fill = 0), by = list(year_measured, plot_id)]

  # remove plot_id in which basal area was higher than 400 m2/ha
  # because there are NA in the BA column, just filter BA < 400 removes the NA rows which I wanna keep
  # Quick & dirty: get all NA and all BA < 400 separeted
  # Check with treeData[tree_id == 8762761]
  naBA <- which(is.na(treeData$BA))
  less400 <- which(treeData$BA < 400)
  treeData <- treeData[sort(c(naBA, less400))]

  # species basal area per plot (BA_sp) as a proxy of seed source
  treeData[is_dead == 'f', BA_sp := sum(indBA) * 1e4/plot_size, by = list(year_measured, plot_id, species_id)]
  # fill NAs the same as for BA
  treeData[, BA_sp := nafill(BA_sp, "locf"), by = list(year_measured, plot_id, species_id)]
  treeData[, BA_sp := nafill(BA_sp, "nocb"), by = list(year_measured, plot_id, species_id)]
  treeData[, BA_sp := nafill(BA_sp, fill = 0), by = list(year_measured, plot_id, species_id)]

  # Species relative basal area to overcome the potential opposite response of
  # regeneration in function of BA (i.e. competition) and BA_sp (i.e. seed source)
  treeData[, relativeBA_sp := BA_sp/BA, by = list(year_measured, plot_id, species_id)]

#



# remove some species_id

  # first three species has been measured only once and has therefore no transition
  # fourth one has too few transitions so glm did not converge
  # next five species did not have any mortality event (should I remove?)
  species_idToRemove = c("181824-ABI-AMA",
                         "NA-CHA-NOO",
                         "183309-PIC-SIT",
                         "194855-JUN-OCC",
                         "18044-THU-PLI",
                         "183400-TSU-HET",
                         "183402-TSU-MER",
                         "183417-LAR-OCC",
                         "26879-PRO-GLA")
  treeData = treeData[!(species_id %in% species_idToRemove)]

#



# remove trees with NA climate variables

  # it means if one tree has 2 mensures but one climate NA, I will remove all the 2 mensures because I am interested in the transition
  tree_idToRemove = unique(treeData[is.na(tot_pp_period3_lag), tree_id])
  treeData = treeData[!(tree_id %in% tree_idToRemove)]

#



# Remove character `-` in species_id's

  treeData[, species_id := gsub("-", "", species_id)]

#



# Separete between Growth and mortality data

  # Growth and mortality (nb > 1 to obtain transition)
  mort_dt = treeData[nbMeasure > 1]

  growth_dt = mort_dt[is_dead == 'f']

#



########################
# Growth
########################



# Remove single measure

  # Trees that have been measured twice, alive and then dead cannot be in growth_dt
  growth_dt[, nbMeasure := .N, by = tree_id]
  growth_dt = growth_dt[nbMeasure > 1]

#



# Rewrite data table in a transition way

  names(growth_dt)[c(4, 5)] <- c('year1', 'dbh1')

  growth_dt[, dbh0 := dbh1 - dbhIncr]
  growth_dt[, year0 := year1 - deltaYear]

  # Remove first line of each tree_id as its information is passed for the next line
  growth_dt = growth_dt[!is.na(deltaYear), ]

#



# Calculate canopyStatus and canopyDistance

  growth_dt[, canopyStatus := ifelse(height > s_star, 1, 0)]
  growth_dt[, canopyDistance := (height - s_star)]

#



########################
# Mortality
########################



# Remove trees already died in first measurement

  # check if there is any tree that is already dead when firt measured
  # if so, remove it because I am interested in the transition Alive => Dead and not Dead => Dead
  mort_dt[mort == 0, nbAlive := .N, by = tree_id]
  mort_dt[, notRemove := any(nbAlive), by = tree_id]
  tree_idToRemove = unique(mort_dt[is.na(notRemove), tree_id])
  mort_dt = mort_dt[!(tree_id %in% tree_idToRemove)]
  mort_dt[, nbAlive := NULL]
  mort_dt[, notRemove := NULL]

#



# Keep only the first death record (Remove more than one dead state by individual trees)

  # Sort data by increasing year
  setorderv(mort_dt, cols = "year_measured", order = +1)

  # Change is_dead by TRUE/FALSE rather than characters "t", "f"
  mort_dt[, is_dead := ifelse(is_dead == "t", TRUE, FALSE)]

  # Keep only the first death record, i.e. F[...]FT[...]T becomes F[...]FT
  #		/!\ database sorted by increasing years /!\
  trackFirstDeath = function(vec_lifeState)
  {
      ind_alive = which(vec_lifeState == FALSE)
      ind_dead = which(vec_lifeState == TRUE)

      toKeep = rep(TRUE, length(vec_lifeState))

      if (length(ind_dead) > 1)
          toKeep[ind_dead[2:length(ind_dead)]] = FALSE

      return(toKeep)
  }

  # Keep only first mortality record event, /!\ database sorted by increasing years /!\
  mort_dt[, keepTree := trackFirstDeath(is_dead), by = tree_id]

  # clean work columns
  mort_dt = mort_dt[keepTree == TRUE]
  mort_dt[, keepTree := NULL]

#



# Fix height problem

  # all dead trees had height = 0, it is now fixed)
  # Get the last height value
  fixHeight = function(vec) {
    vec[length(vec)] = vec[length(vec) - 1]
    return (vec)
  }

  treeIdToFix = mort_dt[is_dead == T & height == 0, unique(tree_id)]
  mort_dt[tree_id %in% treeIdToFix, height := fixHeight(height), by = tree_id]

#



# Rewrite data table in a transition way (year0, year1, state0, state1)

  names(mort_dt)[c(4, 5)] <- c('year1', 'dbh1')

  mort_dt[, dbh0 := dbh1 - dbhIncr]
  mort_dt[, year0 := year1 - deltaYear]

  # Remove first line of each tree_id as its information is passed for the next line
  mort_dt = mort_dt[!is.na(deltaYear), ]

#



# Remove negative growth

  mort_dt = mort_dt[growth >= 0]
  # also remove extreme positive growth (more than 3.5 cm/year)
  mort_dt = mort_dt[growth <= 35]

#



# Remove extreme deltaYear (2 < deltaYear < 15)

  mort_dt = mort_dt[(2 < deltaYear) & (deltaYear < 15)]

#



# Checks (here two tree_ids that should not be in the data base anymore following the above cleaning)

  # this tree_id must have only two lines
  ifelse(dim(mort_dt[tree_id == 8762761])[1] == 2, NA, stop('CKECK FAIL - There was a problem cleaning the data: trees must have only one dead event'))

  # tree with more than one dead events
  ifelse(dim(mort_dt[tree_id == 8718506])[1] == 0, NA, stop('CHECK FAIL - There was a problem cleaning the data: trees with only dead events must be removed from data base')) # tree with only dead events

#



# Calculate canopyStatus and canopyDistance

  mort_dt[, canopyStatus := ifelse(height > s_star, 1, 0)]
  mort_dt[, canopyDistance := (height - s_star)]

#



########################
# Fecundity
########################



# fecundity (all individuals entering the population)

  # Remove plot_id with only one year_measured
  # Number of measures by plot_id
  treeData[, nbYear_measured := length(unique(year_measured)), by = plot_id]
  # keep plots with more than one measures (so I can quantify recruitment number)
  treeData <- treeData[nbYear_measured > 1]

  # Number of measures by tree_id
  treeData[, nbMeasure_treeId := length(unique(year_measured)), by = tree_id]

  # function to define if measurement is a recruit or not
  getRecruitment <- function(year_measured, tree_id) {

    # get nb years and unique
    uqYear = unique(year_measured)
    nbYear = length(uqYear)

    # vector for nb recruitment (values are for plot & year_measured level)
    nbRecruit <- rep(0, sum(uqYear[1] == year_measured))
    # vector specifying if is a recruitment for each measure
    isRecruit <- rep(FALSE, sum(uqYear[1] == year_measured))

    for(year in 1:(nbYear -1))
    {
      # which tree_ids are recruit for this year?
      newRecruit <- !(tree_id[which(uqYear[year + 1] == year_measured)] %in%
                     tree_id[which(uqYear[year] == year_measured)])


      isRecruit <- append(isRecruit, newRecruit)
      # how many?
      nbRecruit <- append(nbRecruit, rep(sum(newRecruit), sum(uqYear[year + 1] == year_measured)))
    }

    return( list(isRecruit, nbRecruit) )
  }

  treeData[, c('isRecruit', 'nbRecruit') := getRecruitment(year_measured, tree_id), by = plot_id]

  ## some plots
    # hist(treeData[nbRecruit < 50, unique(nbRecruit), by = plot_id]$V1, breaks = 30)
    # par(mfrow = c(1, 2))
    # treeData[dbh < 500, hist(dbh, breaks = 30, col = 'grey')]
    # treeData[dbh < 500 & isRecruit == T, hist(dbh, breaks = 30, col = 'grey')]
    #
    # Does nbRecruit increase with deltaYear?
    # x = treeData[!is.na(deltaYear)]
    # xx = x[, list(unique(deltaYear), unique(nbRecruit)), by = list(year_measured, plot_id)]
    # plot(xx$V1, xx$V2, xlab = 'deltaYear', ylab = 'nbRecruit')
  ##

  # calculate recruitment in basal area/plot/year
  treeData[, BARecruit := sum(indBA[isRecruit]) * 1e4/plot_size, by = list(year_measured, plot_id)]


    }

    return(recruit)
  }

  treeData[, nbRecruit := isRecruit(year_measured, tree_id), by = plot_id]

  firstMeasure <- function(year_measured) {

    ln <- length(year_measured)

    # check if year is sorted
    if(identical(year_measured, sort(year_measured))) {

      # return vector of 1 for first measure and 0 for the rest of measures
      if
      return ( c(1, rep(0, ln - 1)) )

    }else {
      stop('year_measured is not sorted')
    }
  }

  treeData[, isRecruit := firstMeasure(year_measured), by = tree_id]



  # calculate number of recruitments/plot/year
  treeData[, nbRecruit := sum(nbMeasure == 1), by = list(year_measured, plot_id)]

  # calculate recruitment in basal area/plot/year
  fec_dt <- treeData[, BARecruit := sum(indBA[nbMeasure == 1]) * 1e4/plot_size, by = list(year_measured, plot_id)]

  # remove NAs of BA
  fec_dt <- fec_dt[!is.na(BA)]

#


 # TODO
 # - nbMeasure == 1 for regeneration is really not appropriated. Think in a new column to assign the first measure of an individual
  # - By tree_id, quantify the number of measuremnts times. Like three times, 1, 2 and 3 for each measurement
 # - Figure out if it's a good idea to remove NA of BA now
 # - I believe it it's a good idea to put here the part of the code preparing for the random forest
 # - organize everything in a transition way, so I can add a delayed measure of BA and BA_sp
 # - Think about the Dom's problem. Negative correlation between competition and seed source
 # - Separete random forest by shade torance. I believe it won't be the same predictors driving regeneration

treeData[plot_id == 685863]
x=treeData[plot_id == 686405]
treeData[plot_id == 674895]

uY <- unique(x$year_measured)
x[year_measured == uY[1], isRecruit := 0]

for(count in 2:length(uY))
{
  tId <- x[year_measured == uY[count], unique(tree_id)][!x[year_measured == uY[count], unique(tree_id)] %in% x[year_measured == uY[count - 1], unique(tree_id)]]
  x[year_measured == uY[count] & tree_id %in% tId]$isRecruit <- 1
  x[year_measured == uY[count] & is.na(isRecruit)] <- 0
}

########################
# For all vital rates
########################



# Filter species_id to keep species with at least 2e4 measurements

  # get total N by species_id
  growth_dt[, nb := .N, by = species_id]
  mort_dt[, nb := .N, by = species_id]
  fec_dt[, nb := .N, by = species_id]

  # filter species_id with more than 20k measures
  spUnique_growth = unique(growth_dt[nb > 1e4]$species_id)
  spUnique_mort = unique(mort_dt[nb > 1e4]$species_id)
  spUnique_fec = unique(fec_dt[nb > 1e4]$species_id)

  ls_species <- Reduce(intersect, list(spUnique_growth, spUnique_mort, spUnique_fec))

  # keep same species id for all three vital rates
  growth_dt = growth_dt[species_id %in% ls_species]
  mort_dt = mort_dt[species_id %in% ls_species]
  fec_dt = fec_dt[species_id %in% ls_species]

  # remove nb column
  growth_dt[, nb := NULL]
  mort_dt[, nb := NULL]
  fec_dt[, nb := NULL]

  # save species_id
  saveRDS(ls_species, file = 'data/spIds.RDS')

#



# Save cleaned data

  cat('####### Saving cleaned growth_dt, mort_dt and fec_dt in the data folder" #######\n')
  saveRDS(growth_dt, 'data/growth_dt.RDS')
  saveRDS(mort_dt, 'data/mort_dt.RDS')
  saveRDS(fec_dt, 'data/fec_dt.RDS')

#
