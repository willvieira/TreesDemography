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



# Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0) and state (state after deltaYear)

  # Number of measurement by tree_id (interested only in nbMeasure > 1 to get transition)
  final_dt[, nbMeasure := .N, by = tree_id]

  # first it is important data is sorted by year_measured
  setorderv(final_dt, cols = "year_measured", order = -1)

  # year
  final_dt[, year1 := shift(year_measured, 1, type = 'lag'), by = tree_id]
  final_dt[, year0 := year_measured]
  final_dt[, year_measured := NULL]

  # DHP
  final_dt[, dbh1 := shift(DHP, 1, type = 'lag'), by = tree_id]
  final_dt[, dbh0 := DHP]
  final_dt[, DHP := NULL]

  # Growth and deltYear
  final_dt[, deltaYear := year1 - year0]
  final_dt[, growth := (dbh1 - dbh0)/deltaYear]

  # State
  final_dt[, state1 := shift(state, 1, type = 'lag'), by = tree_id]
  final_dt[, state0 := state]
  final_dt[, state := NULL]

  # Remove first measure with NA due to transition transformation
  final_dt <- final_dt[!is.na(deltaYear)]

#



# Separate between growth and mortality df

  # Growth and mortality (nb > 1 to obtain transition)
  mort_dt = final_dt[nbMeasure > 1]
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



# Save all

  if(!dir.exists('data/quebec/')) dir.create('data/quebec')

  saveRDS(tree_data, "data/quebec/tree_data_nov2019.RDS")
  st_write(plot_xy, "data/quebec/plot_xy32198_nov2019.gpkg", append = FALSE)
  saveRDS(ecoreg_df, "data/quebec/ecoreg_df_nov19.RDS")
  st_write(bound_Qc, "data/quebec/bound_Qc.gpkg")
  saveRDS(env_data, "data/quebec/env_data_nov2019.RDS")
  saveRDS(allVar_df, "data/quebec/bio-cmi-pcp.RDS")
  saveRDS(final_dt, "data/quebec/treeDataQuebec_all.RDS")
  saveRDS(growth_dt, "data/quebec/growth_dt.RDS")
  saveRDS(mort_dt, "data/quebec/mort_dt.RDS")

#