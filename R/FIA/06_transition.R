#------------------------------------------------------
# Transition growth and mortality
# Will Vieira
# July 28, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0), and state (state after deltaYear)
#   - Transform db in a transition way
#   - Remove extreme growth rate (0 < growth < 35 mm/year)
#   - Keep natural dead events for mortality
#   - remove already indivuduals
#------------------------------------------------------


source('R/FIA/05_cleanData.R')


#------------------------------------------------------
#------------------------------------------------------

# Calculate growth and split between growth and mort db

#------------------------------------------------------
#------------------------------------------------------

# Number of measurement by tree_id (interested only in nbMeasure > 1 to get transition)
treeData[, nbMeasure := .N, by = tree_id]

# year
treeData[, year1 := shift(year_measured, 1, type = 'lead'), by = tree_id]
treeData[, year0 := year_measured]
treeData[, deltaYear := year1 - year0]
treeData[, year_measured := NULL]

# dbh
treeData[, dbh1 := shift(dbh, 1, type = 'lead'), by = tree_id]
treeData[, dbh0 := dbh]
treeData[, deltaDbh := dbh1 - dbh0]
treeData[, dbh := NULL]

# Growth
treeData[, growth := deltaDbh/deltaYear]

# State
treeData[, status1 := shift(status, 1, type = 'lead'), by = tree_id]
treeData[, status0 := status]
treeData[, status := NULL]

# Remove first measure with NA due to transition transformation
treeData <- treeData[!is.na(deltaYear)]



# Separate between growth and mortality df
mort_dt = treeData
growth_dt = treeData[status1 == 1]




#------------------------------------------------------
#------------------------------------------------------

# Growth

#------------------------------------------------------
#------------------------------------------------------

# Remove extreme growth rates
growth_dt <- growth_dt[growth > 0 & growth < 35]


# simplify status
growth_dt[, status := status1]
growth_dt[, c('status1', 'status0') := NULL]





#------------------------------------------------------
#------------------------------------------------------

# Mort

#------------------------------------------------------
#------------------------------------------------------

# keep natural dead only
mort_dt <- mort_dt[plot_treatment1 == 0]

# Remove trees already dead in first measurement
mort_dt <- mort_dt[status0 == 1]

# add mortality column event
mort_dt[, mort := ifelse(status1 == 1, 0, 1)]




# Save
#------------------------------------------------------

saveRDS(growth_dt, 'data/FIA/growth_dt.R')
saveRDS(mort_dt, 'data/FIA/mort_dt.R')
