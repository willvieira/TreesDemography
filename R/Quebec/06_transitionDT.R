##############################
# Prepare data in a transition way
# May 29, 2020
##############################


##############################
# Steps
#   - Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0), and state (state after deltaYear)
#   - Transform db in a transition way
#   - Separate between growth and mortality df
#   - Filter growth_dt to nbMeasure > 1
#   - Remove extreme growth rate (0 < growth < 35 mm/year)
#   - Filter for natual dead only
#   - Remove first dead measurement for mortality
#   - Create a mort column [0 - 1]
##############################


final_transition <- copy(final_dt)

# Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0) and state (state after deltaYear)

  # Number of measurement by tree_id (interested only in nbMeasure > 1 to get transition)
  final_transition[, nbMeasure := .N, by = tree_id]

  # first it is important data is sorted by year_measured
  setorderv(final_transition, cols = "year_measured", order = -1)

  # year
  final_transition[, year1 := shift(year_measured, 1, type = 'lag'), by = tree_id]
  final_transition[, year0 := year_measured]
  final_transition[, year_measured := NULL]

  # DHP
  final_transition[, dbh1 := shift(DHP, 1, type = 'lag'), by = tree_id]
  final_transition[, dbh0 := DHP]
  final_transition[, DHP := NULL]

  # Growth and deltYear
  final_transition[, deltaYear := year1 - year0]
  final_transition[, growth := (dbh1 - dbh0)/deltaYear]

  # State
  final_transition[, state1 := shift(state, 1, type = 'lag'), by = tree_id]
  final_transition[, state0 := state]
  final_transition[, state := NULL]

  # Remove first measure with NA due to transition transformation
  final_transition <- final_transition[!is.na(deltaYear)]

#



# Separate between growth and mortality df

  # Growth and mortality (nb > 1 to obtain transition)
  mort_dt = final_transition[nbMeasure > 1]
  growth_dt = mort_dt[state1 == 'alive']

#



###############
# Growth
###############

# Remove single measure and 

  # Trees that have been measured twice, alive and then dead cannot be in growth_dt
  growth_dt[, nbMeasure := .N, by = tree_id]
  growth_dt <- growth_dt[nbMeasure > 1]

#


# Remove extreme growth rate

  # Remove negative growth rate and > 35 mm/year
  growth_dt <- growth_dt[growth > 0 & growth < 35]

  # Change state1 to state
  growth_dt[, state := state1]
  growth_dt[ ,`:=`(state1 = NULL, state0 = NULL)]

#



########################
# Mortality
########################


# keep natural dead only

  mort_dt <- mort_dt[state1 != 'harvested']

#



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
