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


# shift climate data
treeData[, nbMeasure := length(unique(year0)), by = plot_id]
setorder(treeData, cols = 'year0')

shift_cols <- function(year0, Col)
{
    # define yearID
    uqYears <- unique(year0)
    
    # output list
    newCol <- rep(NA, length(year0))

    for(yr in 1:(length(uqYears) -1))
        newCol[which(year0 == uqYears[yr])] <-
            unique(Col[which(year0 == uqYears[yr + 1])])

    return( newCol )
}

# shift enviromental variables
treeData[
    nbMeasure > 1,
    c('bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd') := lapply(
        .SD,
        function(x)
            shift_cols(year0 = year0, Col = x)
    ),
    by = plot_id,
    .SDcols = c('bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd')
]

# return to previous ordered dt
setorderv(
    treeData,
    cols = c('plot_id', 'subplot_id', 'year0'),
    order = c(1, 1, 1)
)


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

saveRDS(growth_dt, 'data/FIA/growth_transition_dt.RDS')
saveRDS(mort_dt, 'data/FIA/mort_transition_dt.RDS')
