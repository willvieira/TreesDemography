#------------------------------------------------------
# Script to merge FIA and Quebec database before MCMC
# Will Vieira
# July 31, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Rename plot columns in Quebec
#   - Add plot size
#   - Geo location in Quebec plots
#   - Climate variables and cell ID for Quebec
#   - Keep same species_id in both sources
#   - Use correct height measure or estimation in FIA
#   - Delta size and status for Quebec
#   - Merge both Quebec and FIA
#   - Filter NAs
#------------------------------------------------------


library(data.table)

# Non transition db
source(file.path('R', 'FIA', '05_cleanData.R'))
treeData_fia <- treeData; rm(treeData)
treeData_quebec <- readRDS(file.path('data', 'quebec', 'treeDataQuebec_beforeTransition.RDS'))

# Transition db
growth_fia <- readRDS(file.path('data', 'FIA', 'growth_transition_dt.RDS'))
growth_quebec <- readRDS(file.path('data', 'quebec', 'growth_transition_dt.RDS'))

mort_fia <- readRDS(file.path('data', 'FIA', 'mort_dt.RDS'))
mort_quebec <- readRDS(file.path('data', 'quebec', 'mort_dt.RDS'))

fec_fia <- readRDS(file.path('data', 'FIA', 'fec_dt.RDS'))
fec_quebec <- readRDS(file.path('data','quebec', 'fec_dt.RDS'))



# Plot level information
#------------------------------------------------------

# Rename plot column in quebec
treeData_quebec[, plot_id := ID_PE]
growth_quebec[, plot_id := ID_PE]
mort_quebec[, plot_id := ID_PE]
fec_quebec[, plot_id := ID_PE]

# plot size
treeData_quebec[, plot_size := 399.7312]
growth_quebec[, plot_size := 399.7312]
mort_quebec[, plot_size := 399.7312]
fec_quebec[, plot_size := 399.7312]

treeData_fia[, plot_size := subPlot_size]
growth_fia[, plot_size := subPlot_size]
mort_fia[, plot_size := subPlot_size]

# Plot geo location
plot_xy <- treeData_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
treeData_quebec$longitude <- plot_xy[, 'X']
treeData_quebec$latitude <- plot_xy[, 'Y']

plot_xy <- growth_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
growth_quebec$longitude <- plot_xy[, 'X']
growth_quebec$latitude <- plot_xy[, 'Y']

plot_xy <- mort_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
mort_quebec$longitude <- plot_xy[, 'X']
mort_quebec$latitude <- plot_xy[, 'Y']

fec_quebec[
    treeData_quebec,
    `:=`(
        'longitude' = i.longitude,
        'latitude' = i.latitude
    ),
    on = 'plot_id'
]

# Climate variables and cell ID
treeData_quebec[, climate_cellID := cellID]
growth_quebec[, climate_cellID := cellID]
mort_quebec[, climate_cellID := cellID]
fec_quebec[, climate_cellID := cellID]


# Individual level information
#------------------------------------------------------

# Keep same species code id
sp_ref <- data.table()
for(i in treeData_quebec[, unique(sp_code2)])
{
    sp_close <- grep(i, treeData_fia[, unique(species_id)], value = TRUE)
    
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

treeData_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]
growth_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]
mort_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]
fec_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]

# Use estimated height when no measure of height is available
treeData_fia[, height := actual_height]
growth_fia[, height := actual_height]
mort_fia[, height := actual_height]

treeData_fia[is.na(height), height := total_height]
growth_fia[is.na(height), height := total_height]
mort_fia[is.na(height), height := total_height]

treeData_fia[is.na(height), height := est_height]
growth_fia[is.na(height), height := est_height]
mort_fia[is.na(height), height := est_height]

# Delta size for quebec
treeData_quebec[, dbh := DHP]
growth_quebec[, deltaDbh := dbh1 - dbh0]
mort_quebec[, deltaDbh := dbh1 - dbh0]

# status (remove harvested for quebec)
treeData_quebec <- treeData_quebec[state != 'harvested']
treeData_quebec[, status := ifelse(state == 'alive', 1, 2)]
growth_quebec[, status := ifelse(state == 'alive', 1, 2)]



# Seedling and sapling info
#------------------------------------------------------

# Make sure the same threshold is used by both sources
fec_quebec[, nbRecruit := nbRecruit_127]

# Ignore classes of sapling in quebec
fec_quebec[, nbSapling := nbSapling_total]

# add size of microplots
fec_quebec[, plotSize_seedling := 16.05]
fec_quebec[, plotSize_sapling := 40.04]

fec_fia[, plotSize_seedling := 13.5]
fec_fia[, plotSize_sapling := 13.5]


# Merge
#------------------------------------------------------

# add source unique ID
treeData_fia[, db_origin := 'fia']
growth_fia[, db_origin := 'fia']
mort_fia[, db_origin := 'fia']
fec_fia[, db_origin := 'fia']

treeData_quebec[, db_origin := 'qc']
growth_quebec[, db_origin := 'qc']
mort_quebec[, db_origin := 'qc']
fec_quebec[, db_origin := 'qc']

# Name of columns to keep
colsToKeep_treeData <- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA_plot', 'BA_comp', 'relativeBA_comp', 's_star', 'db_origin', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 'climate_cellID', 'tree_id', 'species_id', 'status', 'year_measured', 'dbh', 'height', 'canopyDistance', 'BA_sp', 'BA_inter', 'BA_comp_sp', 'BA_comp_intra', 'relativeBA_sp'
)

# Merge treeData
treeData <- rbind(
    treeData_fia[, colsToKeep_treeData, with = FALSE],
    treeData_quebec[, colsToKeep_treeData, with = FALSE]
)


# Name of columns to keep
colsToKeep_growth <- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA_plot', 'BA_comp', 'relativeBA_comp', 's_star', 'db_origin', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 'climate_cellID', 'tree_id', 'species_id', 'status', 'year0', 'year1', 'deltaYear', 'dbh0', 'dbh1', 'deltaDbh', 'growth', 'height', 'canopyDistance', 'BA_sp', 'BA_inter', 'BA_comp_sp', 'BA_comp_intra', 'relativeBA_sp'
)

# Merge growth
growth <- rbind(
    growth_fia[, colsToKeep_growth, with = FALSE],
    growth_quebec[, colsToKeep_growth, with = FALSE]
)

# Name of columns to keep
colsToKeep_mort<- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA_plot', 'BA_comp', 'relativeBA_comp', 's_star', 'db_origin', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 'climate_cellID', 'tree_id', 'species_id', 'mort', 'year0', 'year1', 'deltaYear', 'dbh0', 'dbh1', 'deltaDbh', 'height', 'canopyDistance', 'BA_sp', 'relativeBA_sp'
)

# Merge mort
mort <- rbind(
    mort_fia[, colsToKeep_mort, with = FALSE],
    mort_quebec[, colsToKeep_mort, with = FALSE]
)

colsToKeep_fec <- c(
    'plot_id', 'species_id', 'nbRecruit', 'nbSapling', 'nbSeedling', 'deltaYear_plot', 'plot_size', 'plotSize_seedling', 'plotSize_sapling',
    'latitude', 'longitude', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 's_star', 'BA_plot', 'BA_adult', 'BA_adult_sp', 'relativeBA_adult_sp', 'year1', 'year0', 'db_origin'
)

fec <- rbind(
    fec_fia[, colsToKeep_fec, with = FALSE],
    fec_quebec[, colsToKeep_fec, with = FALSE]
)



# Remove NAs
#------------------------------------------------------

treeData <- treeData[!is.na(species_id)]
growth <- growth[!is.na(species_id)]
mort <- mort[!is.na(species_id)]
fec <-  fec[!is.na(species_id)]



# Save
#------------------------------------------------------

saveRDS(treeData, file.path('data', 'treeData.RDS'))
saveRDS(growth, file.path('data', 'growth_dt.RDS'))
saveRDS(mort, file.path('data', 'mort_dt.RDS'))
saveRDS(fec, file.path('data', 'fec_dt.RDS'))
