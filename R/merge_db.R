#------------------------------------------------------
# Script to merge FIA and Quebec database before MCMC
# Will Vieira
# July 31, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - 
#------------------------------------------------------


library(data.table)

growth_fia <- readRDS('data/FIA/growth_dt.RDS')
growth_quebec <- readRDS('data/quebec/growth_dt.RDS')

mort_fia <- readRDS('data/FIA/mort_dt.RDS')
mort_quebec <- readRDS('data/quebec/mort_dt.RDS')





# Plot level information
#------------------------------------------------------

# Rename plot column in quebec
growth_quebec[, plot_id := ID_PE]
mort_quebec[, plot_id := ID_PE]

# plot size
growth_quebec[, plot_size := 399.7312]
mort_quebec[, plot_size := 399.7312]

growth_fia[, plot_size := subPlot_size]
mort_fia[, plot_size := subPlot_size]

# Plot geo location
plot_xy <- growth_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
growth_quebec$longitude <- plot_xy[, 'X']
growth_quebec$latitude <- plot_xy[, 'Y']

plot_xy <- mort_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
mort_quebec$longitude <- plot_xy[, 'X']
mort_quebec$latitude <- plot_xy[, 'Y']

# Climate variables and cell ID
growth_quebec[, bio_01 := value5_bio60_01]
growth_quebec[, bio_12 := value5_bio60_12]
growth_quebec[, climate_cellID := cellID]

mort_quebec[, bio_01 := value5_bio60_01]
mort_quebec[, bio_12 := value5_bio60_12]
mort_quebec[, climate_cellID := cellID]




# Individual level information
#------------------------------------------------------

# Keep same species code id
sp_ref <- data.table()
for(i in growth_quebec[, unique(sp_code2)])
{
    sp_close <- grep(i, growth_fia[, unique(species_id)], value = TRUE)
    
    if(length(sp_close) == 1) {
        DF <- data.frame(
            sp_qc = i,
            sp_us = sp_close
        )
        sp_ref <- rbind(
            DF,
            sp_ref
        )
    }else{
        print(i)
        print(sp_close)
    }
}

sp_ref <- rbind(
    sp_ref,
    data.table(
        sp_qc = c('ACESAC', 'ACERIN', 'CARCAR', 'SALSP', 'ALNRUG', 'AMESP'),
        sp_us = c('28731ACESAC', '28757ACESAC', '19504CARCAR', '22484SALNIG', NA, NA)
    )
)

growth_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]
mort_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]


# Use estimated height when no measure of height is available
growth_fia[, height := actual_height]
mort_fia[, height := actual_height]

growth_fia[is.na(height), height := total_height]
mort_fia[is.na(height), height := total_height]

growth_fia[is.na(height), height := est_height]
mort_fia[is.na(height), height := est_height]

# Delta size for quebec
growth_quebec[, deltaDbh := dbh1 - dbh0]
mort_quebec[, deltaDbh := dbh1 - dbh0]

# status
growth_quebec[, status := ifelse(state == 'alive', 1, 2)]




# Merge
#------------------------------------------------------

# add source unique ID
growth_fia[, db_origin := 'fia']
mort_fia[, db_origin := 'fia']

growth_quebec[, db_origin := 'qc']
mort_quebec[, db_origin := 'qc']


# Name of columns to keep
colsToKeep_growth <- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA', 's_star',
    'db_origin', 'bio_01', 'bio_12', 'climate_cellID',
    'tree_id', 'species_id', 'status', 'year0', 'year1', 'deltaYear',
    'dbh0', 'dbh1', 'deltaDbh', 'height', 'canopyDistance',
    'BA_sp', 'relativeBA_sp'
)

# Merge growth
growth <- rbind(
    growth_fia[, colsToKeep_growth, with = FALSE],
    growth_quebec[, colsToKeep_growth, with = FALSE]
)

# Name of columns to keep
colsToKeep_mort<- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA', 's_star',
    'db_origin', 'bio_01', 'bio_12', 'climate_cellID',
    'tree_id', 'species_id', 'mort', 'year0', 'year1', 'deltaYear',
    'dbh0', 'dbh1', 'deltaDbh', 'height', 'canopyDistance',
    'BA_sp', 'relativeBA_sp'
)

# Merge growth
mort <- rbind(
    mort_fia[, colsToKeep_mort, with = FALSE],
    mort_quebec[, colsToKeep_mort, with = FALSE]
)




# Save
#------------------------------------------------------

saveRDS(growth, 'data/growth_dt.RDS')
saveRDS(mort, 'data/mort_dt.RDS')
