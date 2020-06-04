##############################
# Calculate plot, species and individual variables
# May 29, 2020
##############################


##############################
# Steps
  # - Merge all data from above into one single data.table object
  # - Calculate individual height using Purves et al. 2008 allometries
  # - Calculate competition s_star, canopyStatus and canopyDistance
  # - Calculate plot and species basal area
##############################


# Merge all in one single dt

  final_df <- tree_data %>%
      dplyr::select(-c('ID_PE_MES', 'NO_ARBRE', 'ID_ARBRE', 'ID_ARB_MES', 'ESSENCE', 'DHP_NC', 'plot_id')) %>%
      dplyr::right_join(ecoreg_df[, c('ID_PE', 'ecoreg3')], by = 'ID_PE') %>%
      dplyr::right_join(env_data[, names(env_data)[-c(2, 3)]], by = c('ID_PE', 'year_measured')) %>%
      dplyr::right_join(allVar_df[, c('ID_PE', 'year_measured', names(allVar_df)[grep('value5_', names(allVar_df))])], by = c('ID_PE', 'year_measured'))
      
  final_dt <- as.data.table(final_df)

#



# Calculate individual height

  # First remove all species without sp_code2 (all of them also do not have DHP)
  final_dt <- final_dt[!is.na(sp_code2)]
  
  # source parameters and functions from Amaël's GitHub repo
  devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/createData/parametersAllometries.R')
  devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/toolFunctions.R')

  # get parametrised species
  tmp <- tempfile()
  download.file('https://github.com/amael-ls/code_R0niche/raw/master/createData/ls_speciesParametrised.rds', tmp)
  parametrisedSpecies <- readRDS(tmp)
  parametrisedSpecies <- sps_code$spCode[sps_code$CODE %in% parametrisedSpecies[, 1]]

  # species not parametrised
  final_dt[, unique(sp_code)][!final_dt[, unique(sp_code)] %in% parametrisedSpecies]

  ## Calculate height
  final_dt[sp_code2 %in% parametrisedSpecies, height := dbhToHeight(DHP, purves2007_allometries[species == sps_code$CODE[which(sps_code$spCode == sp_code2)], a],
	  	purves2007_allometries[species == sps_code$CODE[which(sps_code$spCode == sp_code2)], b], mm = TRUE), by = sp_code2]


  final_dt[is.na(height), .N]/nrow(final_dt) * 100
  # 12% of data does not have height (NA)
  # But most of this is due dead trees, which ecologicaly do not compete for light
  final_dt[!is.na(DHP) & is.na(height), .N]/final_dt[!is.na(DHP), .N] * 100
  # less than 1% of these NA for height is due to lack of species specific parameters
  # So for those species I get the heigth based in their DHP according to a regression line for the correlation between DHP and heigth
  # For those individuals already dead, I will keep height = 0 as they should not compete (for the S* calculation)
  srg <- summary(lm(height ~ poly(DHP, 2 , raw = TRUE), final_dt))
  final_dt[!is.na(DHP) & is.na(height), height := (DHP^2 * rnorm(1, srg$coefficients[3, 1], srg$coefficients[3, 2])) + (DHP * rnorm(1, srg$coefficients[2, 1], srg$coefficients[2, 2])) + rnorm(1, srg$coefficients[1, 1], srg$coefficients[1, 2])]

#



# Calculate S*
  
  # set data.table threads for internal parallel
  setDTthreads(threads = 16)

  # First, let's fix the lack of parameters for some species
  # Again, all these species togethere represents 0.8% of the data, so the methods here won't change much the outcome
  # I will use fuzzy match to find a species name close to the ones lacking parameters
  # It's the easier methods to fill missing parameters as closer names means they are close in classification (e.g. genre, family, etc)
  missSp <- unique(final_dt$sp_code2)[!unique(final_dt$sp_code2) %in% gsub('-',
     '', purves2007_allometries$sp)]

  # 0.84% of db is missing allometric parameters
  final_dt[sp_code2 %in% missSp, .N]/final_dt[, .N] * 100

  # Find the closest sp name
  missingSp <- data.frame(missSp = missSp, closeSp = rep(NA, length(missSp)))
  for(i in 1: length(missSp))
  {

    #cat('Looking for sp:', missingSp$missSp[i], '(', i, 'of', length(missSp), ')')
    
    Max_dist <- seq(0.1, 0.9, 0.001)
    test = 1
    
    while(test <= length(Max_dist))
    {
      #cat('Test', test, 'of', length(Max_dist), '\r')

      out <- agrep(missingSp$missSp[i], gsub('-', '', purves2007_allometries$sp), max.distance = Max_dist[test], value = TRUE)
      
      if(length(out) > 0)
      {

        missingSp[i, 'closeSp'] <- out[1] # if more than one, get the first one only
        break

      }else{
        test <- test + 1
      }
    }
  }

  # create new columns changing the name of the missing sp to their closer sp
  # it will be used only to calculate s*
  changeName = function(sp) if(unique(sp) %in% missingSp$missSp) {return (rep(missingSp$closeSp[unique(sp) == missingSp$missSp], length(sp))) }else {return (sp)}
  final_dt[, sp_code3 := changeName(sp_code2), by = sp_code2]


  # get plot and year combination to calculate S*
  final_dt[, plot_year := paste0(ID_PE, "_", year_measured)]

  #### Calculate competition
  ## Function to calculate s*
  # dbh and species_id are vectors; plotArea a scalar
  # Trees are sorted by decreasing height
  
  # Function to get allometric parameters
  purves2007_allometries_modif <- purves2007_allometries[, sp := gsub('-', '', sp)]
  getAllometries <- function(species_id, allometries = purves2007_allometries_modif)
  {
    n = length(species_id)
    ls_allometries = list(a = numeric(length = n), b = numeric(length = n), T_param = numeric(length = n))

    DF = allometries[sp %in% species_id]

    for(i in 1:nrow(DF))
    {   
        pos <- species_id %in% DF$sp[i]
        ls_allometries$a[pos] <- DF$a[i]
        ls_allometries$b[pos] <- DF$b[i]
        ls_allometries$T_param[pos] <- DF$T[i]
    }   

    return (ls_allometries)
  }
  
  canopyHeight_mid = function(height, sp_code, plot_size, C0_C1, plot_year)
  {
    sumArea = 0
    n = length(height)
    Min = 0
    Max = n + 1
    possib = (Max - Min) - 1
    mid = floor(possib/2) + Min

    # check when only 1 measure
    if(mid == 0)
      possib = 0; mid = 2

    while (possib > 0)
    {
      distToTop = height[1:mid] - height[mid]
      paramsAllometries = getAllometries(sp_code[1:mid])
      sumArea = sum(heightToCrownArea(height[1:mid], distToTop, paramsAllometries$a, paramsAllometries$b,
        paramsAllometries$T_param, C0_C1))
      
      if(sumArea < plot_size)
      {
        Min = mid
        possib = (Max - Min) - 1
        mid = floor(possib/2) + Min + 1
      }else {
        Max = mid
        possib = (Max - Min) - 1
        mid = floor(possib/2) + Min + 1
      }
    }

    if (mid == (n + 1)) # i.e., gap in the canopy
      return (0)

    return (height[mid])
  }


  # Sort by plot id, year (within plot_id), and then decreasing height (within plot AND years)
  setorderv(final_dt, cols = c("ID_PE", "year_measured", "height"), order = c(1, 1, -1))

  # get s_star
  final_dt[!is.na(height), s_star :=
          canopyHeight_mid(height, sp_code3, plot_size = 399.7312, C0_C1, plot_year),
          by = plot_year]

  # Remove provisory columns
  final_dt[ ,`:=`(sp_code3 = NULL, plot_year = NULL)]

  # Calculate canopyStatus and canopyDistance
  final_dt[, canopyDistance := (height - s_star)]
  final_dt[, canopyStatus := ifelse(height > s_star, 1, 0)]

  # library(ggplot2)
  # ggplot(final_dt[!is.na(ETAGE_ARB)], aes(x = canopyDistance)) +
  #   geom_histogram(position = "identity", colour = "grey40", bins = 50) +
  #   facet_grid(. ~ ETAGE_ARB, labeller = 
  #              labeller(ETAGE_ARB = c(V = 'Veteran', D = 'Dominant', C = 'Codominant', I = 'Intermediate', O = 'Oppressed')))

  # ggplot(final_dt[!is.na(ENSOLEIL) & s_star != 0], aes(x = canopyDistance)) +
  #   geom_histogram(position = "identity", colour = "grey40", bins = 50) +
  #   facet_grid(. ~ ENSOLEIL, labeller = 
  #              labeller(ENSOLEIL = c('1' = 'Très ensoleillé', '2' = 'Moyennement ensoleillé', '3' = 'Peu ensoleillé', '4' = 'Non ensoleillé')))

#



#   - Calculate plot and species basal area per year

  # some fixes
  final_dt[, DHP := as.numeric(DHP)] # DHP numeric
  treesToRemove <- final_dt[state == 'alive' & is.na(DHP), tree_id] # Remove alive trees without DHP
  final_dt <- final_dt[!(tree_id %in% treesToRemove)]
  final_dt <- final_dt[!(state %in% c('unknown', 'AllDead'))]

  # plot basal area
  # calculate plot basal area (BA in m2/ha)
  final_dt[state == 'alive', BA := sum(indBA) * 1e4/399.7312, by = list(ID_PE, year_measured)]

  # fill NAs of BA (due to dead trees) with the value from the plot
  final_dt[, BA := nafill(BA, "locf"), by = list(ID_PE, year_measured)]
  final_dt[, BA := nafill(BA, "nocb"), by = list(ID_PE, year_measured)]

  # species basal area per plot (BA_sp) as a proxy of seed source
  final_dt[state == 'alive', BA_sp := sum(indBA) * 1e4/399.7312, by = list(ID_PE, year_measured, sp_code2)]
  # fill NAs the same as for BA
  final_dt[, BA_sp := nafill(BA_sp, "locf"), by = list(ID_PE, year_measured, sp_code2)]
  final_dt[, BA_sp := nafill(BA_sp, "nocb"), by = list(ID_PE, year_measured, sp_code2)]
  
  # Species relative basal area to overcome the potential opposite response of
  # regeneration in function of BA (i.e. competition) and BA_sp (i.e. seed source)
  final_dt[, relativeBA_sp := BA_sp/BA, by = list(ID_PE, year_measured, sp_code2)]
  # 0/0 = NA
  final_dt[is.na(relativeBA_sp), relativeBA_sp := 0]

  # plot(test[, unique(s_star), by = .(ID_PE, year0)]$V1, test[, unique(BA),
  #    by = .(ID_PE, year0)]$V1, pch = 19, col = rgb(0,0,0, 0.1), xlab = 's_star (m)', ylab = c('Basal area (m2/ha)'))

#
