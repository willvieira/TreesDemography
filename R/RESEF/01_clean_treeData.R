##############################################
# Prepare RESEF db
# Will Vieira
# Sept 25, 2020
# Last updated: Nov 4, 2020
##############################################


# METADATA
# 49 Plots of 50 x 100 m for hardwood stand and 100 x 100 for conifer stand
# Each plot is subdivided in 10 x 10 subplots


##############################
# Steps
#   - Load packages and database
#   - Extract useful layers
#   - Keep only necessary columns
#   - change species code
#   - Merge with plot info
#   - Remove and group some states
#   - basal area
##############################





#######################################################################
# Load packages and database
#######################################################################

library(data.table)
library(tidyverse)

# db link
db <- 'rawData/RESEF_Dendro2019.accdb'

# Show tables
Hmisc::mdb.get(db, tables = TRUE)





#######################################################################
# Extract useful layers
#######################################################################

# Trees DBH > 1 cm
tree_data <- setDT(Hmisc::mdb.get(db, tables = 't_Dendrometrie'))
# Regeneration
reg_data <- setDT(Hmisc::mdb.get(db, tables = 't_Regeneration'))
# Plot_id year of measurement
plot_measure <- setDT(Hmisc::mdb.get(db, tables = 't_SerieAnnee'))
# Plot_id GPS location
plot_location <- setDT(Hmisc::mdb.get(db, tables = 't_Azimuts_XY'))
# plot_id info
plot_info <- setDT(Hmisc::mdb.get(db, tables = 't_Reseau'))
# Epidemic info 
epidemic <- setDT(Hmisc::mdb.get(db, tables = 't_EpidemieTBE'))
# species list
spList <- setDT(Hmisc::mdb.get(db, tables = 't_ListeEss'))
# Species code
spCodeFile <- "rawData/ref_spCode.csv"
if(!file.exists(spCodeFile))
    download.file('https://raw.githubusercontent.com/mhBrice/talk_thesis/master/data/ref_spCode.csv', spCodeFile, method = 'auto', quiet = TRUE)
sps_code <- setDT(read.csv2(spCodeFile))





#######################################################################
# Keep only necessary columns
#######################################################################

# Remove columns with too many NA (> 90%) and keep only necessary columns
colsToRm <- names(which(apply(tree_data, 2, function(x) length(which(complete.cases(x)))/nrow(tree_data)) < 0.9))
tree_data <- tree_data[, setdiff(names(tree_data), c(colsToRm, 'SERIE', 'RANG', 'ORDRE', 'CLE', 'PPA', 'DATE', 'BORNA', 'BORNB')), with = FALSE]

# Rename remaining columns to plain English
tree_data <- tree_data[, .(plot_id = PLACE,
                            subplot_id = PEP,
                            tree_id = paste0(PLACE, '_', PEP, '_', ARBRE),
                            year = ANNEE,
                            sp_id = ESS,
                            dbh = DHP,
                            state = ETAT,
                            canopyState = ETAG,
                            heightMeasured = H,
                            height = HTOT,
                            x = AXEX,
                            y = AXEY,
                            obs = REMARQUE)]

# add year of measure to reg_data and remove SERIE column
reg_data <- merge(reg_data, plot_measure[, 1:3], by = c('SERIE', 'PLACE'))
reg_data[, SERIE := NULL]

# Rename remaining columns to plain English
reg_data <- reg_data[, .(plot_id = PLACE,
                            subplot_id = PEP,
                            year = ANNEE,
                            sp_id = ESS,
                            nbStems = NBTIGES,
                            regCover = RECOUV)]





#######################################################################
# Change species code
#######################################################################

uqSp <- unique(c(tree_data[, unique(sp_id)], reg_data[, unique(sp_id)]))

# species in the list that are neither in the adults nor in the regeneration database
spList$Code[!spList$Code %in% uqSp]
# species in the aduts/regneration database that are not listed in the spList
uqSp[!uqSp %in% spList$Code]
tree_data[, .N, by = sp_id][sp_id %in% c('AUR', 'AUC')]
reg_data[, .N, by = sp_id][sp_id %in% c('AUR', 'AUC')]

# Assign new sp code
tree_data <- merge(tree_data, sps_code[, c('spCode', 'qc_code'), with = FALSE], by.x = 'sp_id', by.y = 'qc_code')
reg_data <- merge(reg_data, sps_code[, c('spCode', 'qc_code'), with = FALSE], by.x = 'sp_id', by.y = 'qc_code')

# Remove old sp_id
tree_data[, sp_id := NULL]
reg_data[, sp_id := NULL]





#######################################################################
# Merge with plot info
#######################################################################

# GPS location of plot
tree_data <- merge(tree_data, plot_location[, c('PLACE', 'LONGCALGPS', 'LATCALGPS'), with = FALSE], by.x = 'plot_id', by.y = 'PLACE')
reg_data <- merge(reg_data, plot_location[, c('PLACE', 'LONGCALGPS', 'LATCALGPS'), with = FALSE], by.x = 'plot_id', by.y = 'PLACE')

# plot info
plot_info <- plot_info[, c('PLACE', 'ZONE', 'PENTE', 'CL.PENTE', 'DRAINAGE', 'HUMUS', 'SOL', 'DENSITE'), with = FALSE]
plot_info <- plot_info[, .(plot_id = PLACE,
                           ecoregion = ZONE,
                           slope = PENTE,
                           slope_cl = CL.PENTE,
                           drai = DRAINAGE,
                           humus = HUMUS,
                           soil = SOL,
                           densityCover = DENSITE)]

tree_data <- merge(tree_data, plot_info)
reg_data <- merge(reg_data, plot_info)





#######################################################################
# Remove and group some states
#######################################################################

# Etat de l'arbre:
# 10=vivant, 14=mort, 40=recrue commercial (>91mm), 60=recrue gaulis (>1.1cm),
# 24=mort tombé, 26=coupé
tree_data[, knitr::kable(sort(table(state)))]

dead_states <- c(14, 24)
live_states <- c(10, 40, 60)
rm_states <- c(12, 26)

# Remove and group tree states
tree_data[state %in% dead_states, state2 := 'dead']
tree_data[state %in% live_states, state2 := 'alive']
tree_data <- tree_data[!state %in% rm_states]

# Check for resurection
dead_id <- tree_data[state %in% dead_states, tree_id]
dead <- tree_data %>%
    filter(tree_id %in% dead_id) %>%
    group_by(tree_id) %>%
    arrange(year) %>%
    slice(-(1:min(which(state %in% dead_states)))) # remove all rows before dead state by tree id

resurected_id <- dead %>%
                    subset(state %in% live_states) %>%
                    distinct(tree_id)

# Only one case of resurected tree (chaning its dead ID to alive)
tree_data[tree_id == resurected_id & state2 == 'dead', state2 := 'alive']
rm(dead_id, dead, resurected_id)

# Are all tree_id associated with only one species?
if(tree_data[, length(unique(spCode)), by = tree_id][, unique(V1)] != 1)
    stop('Some tree_id has more than one species_id')





#######################################################################
# Basal are
#######################################################################

tree_data[, indBA := pi * (dbh/(2 * 1000))^2]
