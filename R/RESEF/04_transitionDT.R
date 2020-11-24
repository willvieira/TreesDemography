##############################
# Prepare data in a transition way
# Will Vieira
# Nov 29, 2020
##############################


##############################
# Steps
#   - Remove plots with only one measure
#   - Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0), and state (state after deltaYear)
#   - Transform db in a transition way
#   - Separate between growth and mortality df
#   - Filter growth_dt to nbMeasure > 1
#   - Remove extreme growth rate (0 < growth < 35 mm/year)
#   - Filter for natual dead only
#   - Remove first dead measurement for mortality
#   - Create a mort column [0 - 1]
##############################





##########################################################################################
# Remove plots with only one measure
##########################################################################################

# unique years of each plot_id
yearsPlot_id <- tree_data[, unique(year), by = plot_id]

# Which plot_id have only 1 measure?
plot_idToRm <- yearsPlot_id[, names(which(table(plot_id) == 1))]

# Keep plots with more than one measure
tree_data <- tree_data[!plot_id %in% plot_idToRm]





##########################################################################################
# Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0) and state (state after deltaYear)
##########################################################################################

# Number of measurement by tree_id (interested only in nbMeasure > 1 to get transition)
tree_data[, nbMeasure := .N, by = tree_id]

# first it is important data is sorted by year_measured
setorderv(tree_data, cols = "year", order = -1)

# year
tree_data[, year1 := shift(year, 1, type = 'lag'), by = tree_id]
tree_data[, year0 := year]
tree_data[, year := NULL]

# DHP
tree_data[, dbh1 := shift(dbh, 1, type = 'lag'), by = tree_id]
tree_data[, dbh0 := dbh]
tree_data[, dbh := NULL]

# Growth and deltYear
tree_data[, deltaYear := year1 - year0]
tree_data[, growth := (dbh1 - dbh0)/deltaYear]

# State
tree_data[, state_original := state]
tree_data[, state1 := shift(state2, 1, type = 'lag'), by = tree_id]
tree_data[, state0 := state2]
tree_data[, state := NULL]
tree_data[, state2 := NULL]

# Remove first measure with NA due to transition transformation
tree_data <- tree_data[!is.na(deltaYear)]




##########################################################################################
# Separate between growth and mortsality df
##########################################################################################

# Growth and mortality
mort_dt = tree_data[nbMeasure > 1]
growth_dt = mort_dt[state1 == 'alive']




###############
# Growth
###############


# Remove extreme growth rate

  # Remove negative growth rate and > 35 mm/year
  growth_dt <- growth_dt[growth >= 0 & growth < 35]

#



########################
# Mortality
########################


# Remove trees already died in first measurement

  # check if there is any tree that is already dead when firt measured
  # if so, remove it because I am interested in the transition Alive => Dead and not Dead => Dead
  mort_dt <- mort_dt[state0 == 'alive']

#



# Change name of states columuns and create one mort with TRUE and FALSE

  # Change state1 to state
  mort_dt[, state := state1]
  mort_dt[ ,`:=`(state1 = NULL, state0 = NULL)]

  # Mort column
  mort_dt[, mort := ifelse(state == 'alive', 0, 1)]

#



# Add delayed growth rate as possible proxy of mortality risk
# i.e. the growth rate of the measure before
  
  # 'leading' shift as year is in descreasing order
  mort_dt[, growth_lag := shift(growth, 1L, type = 'lead'), by = tree_id]

#





##########################################################################################
# Save all
##########################################################################################

if(!dir.exists('data/RESEF/')) dir.create('data/RESEF')

saveRDS(tree_data, "data/RESEF/tree_data.RDS")
saveRDS(growth_dt, "data/RESEF/growth_dt.RDS")
saveRDS(mort_dt, "data/RESEF/mort_dt.RDS")
