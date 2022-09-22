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

growth_fia <- readRDS(file.path('data', 'FIA', 'growth_dt.RDS'))
growth_quebec <- readRDS(file.path('data', 'quebec', 'growth_dt.RDS'))

mort_fia <- readRDS(file.path('data', 'FIA', 'mort_dt.RDS'))
mort_quebec <- readRDS(file.path('data', 'quebec', 'mort_dt.RDS'))

fec_fia <- readRDS(file.path('data', 'FIA', 'fec_dt.RDS'))
fec_quebec <- readRDS(file.path('data','quebec', 'fec_dt.RDS'))



# Plot level information
#------------------------------------------------------

# Rename plot column in quebec
growth_quebec[, plot_id := ID_PE]
mort_quebec[, plot_id := ID_PE]
fec_quebec[, plot_id := ID_PE]

# plot size
growth_quebec[, plot_size := 399.7312]
mort_quebec[, plot_size := 399.7312]
fec_quebec[, plot_size := 399.7312]

growth_fia[, plot_size := subPlot_size]
mort_fia[, plot_size := subPlot_size]

# Plot geo location
plot_xy <- growth_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
growth_quebec$longitude <- plot_xy[, 'X']
growth_quebec$latitude <- plot_xy[, 'Y']

plot_xy <- mort_quebec[, sf::st_coordinates(sf::st_transform(SHAPE, 4326))]
mort_quebec$longitude <- plot_xy[, 'X']
mort_quebec$latitude <- plot_xy[, 'Y']

fec_quebec[
    growth_quebec,
    `:=`(
        'longitude' = i.longitude,
        'latitude' = i.latitude
    ),
    on = 'plot_id'
]

# for the remaining NA, extract lat and lon from SHAPE (slow)
get_xy <- function(SHAPE, coord = 1) {
    sf::st_coordinates(
        sf::st_transform(
            sf::st_sfc(
                sf::st_point(
                    unlist(SHAPE)
                ),
                crs = growth_quebec[1, sf::st_crs(SHAPE)]
            ),
            4326
        )
    )[coord]
}

fec_quebec[is.na(latitude), latitude := sapply(SHAPE, get_xy, coord = 1)]
fec_quebec[is.na(longitude), longitude := sapply(SHAPE, get_xy, coord = 2)]
 

# Climate variables and cell ID
growth_quebec[, climate_cellID := cellID]
mort_quebec[, climate_cellID := cellID]
fec_quebec[, climate_cellID := cellID]


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
fec_quebec[, species_id := sp_ref$sp_us[match(sp_code2, sp_ref$sp_qc)]]

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
growth_fia[, db_origin := 'fia']
mort_fia[, db_origin := 'fia']
fec_fia[, db_origin := 'fia']

growth_quebec[, db_origin := 'qc']
mort_quebec[, db_origin := 'qc']
fec_quebec[, db_origin := 'qc']

# Name of columns to keep
colsToKeep_growth <- c(
    'plot_id', 'longitude', 'latitude', 'plot_size', 'BA_plot', 'BA_comp', 'relativeBA_comp', 's_star', 'db_origin', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 'climate_cellID', 'tree_id', 'species_id', 'status', 'year0', 'year1', 'deltaYear', 'dbh0', 'dbh1', 'deltaDbh', 'growth', 'height', 'canopyDistance', 'BA_sp', 'relativeBA_sp'
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
    'latitude', 'longitude', 'bio_01_mean', 'bio_12_mean', 'bio_01_sd', 'bio_12_sd', 's_star', 'BA_plot', 'BA_adult', 'BA_adult_sp', 'relativeBA_adult_sp', 'year1', 'year0', 'nbSeedling', 'db_origin'
)

fec <- rbind(
    fec_fia[, colsToKeep_fec, with = FALSE],
    fec_quebec[, colsToKeep_fec, with = FALSE]
)



# Remove NAs
#------------------------------------------------------

growth <- growth[!(is.na(species_id) | is.na(BA_plot) | is.na(BA_sp) | is.na(s_star))]
mort <- mort[!(is.na(species_id) | is.na(BA_plot) | is.na(BA_sp) | is.na(s_star))]
fec <-  fec[!(is.na(species_id) | is.na(BA_plot) | is.na(s_star))]



# Save
#------------------------------------------------------

saveRDS(growth, file.path('data', 'growth_dt.RDS'))
saveRDS(mort, file.path('data', 'mort_dt.RDS'))
saveRDS(fec, file.path('data', 'fec_dt.RDS'))
