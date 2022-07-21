###############################
# Merge FIA files into single RDS
# Will Vieira
# July 6, 2022
##############################



##############################
# Steps
#   - Load db tables while merging all states into single obj
#   - Merge plot, condition and subplot condition into one obj
#   - Filter plot/survey to have permanent plots (remeasured) and assure all states follow the same protocol
#   - load tree data for plots of interest
#   - Assign plot size, tree_id, and species name
#   - rename columns
##############################



library(data.table)


# Define states
states <- c(
    'ME', 'MA', 'RI', 'CT', 'NJ', 'DE', 'MD',
    'NH', 'VT', 'NY', 'PA', 'VA', 'NC', 'SC', 'GA', 'FL',
    'MI', 'OH', 'IN', 'WV', 'KY', 'TN', 'AL' , 'MS',
    'WI', 'IL', 'MN', 'IA', 'MO', 'AR', 'LA',
    'ND', 'SD', 'NE', 'KS', 'OK', 'TX'
)



# Load table files from states into a single RDS object
############################################################

# load SURVEY TABLE
surv_tb <- fread(
    cmd = "awk 'NR==1||FNR!=1' rawData/FIA/csv/*_SURVEY.csv",
    header = T
)

# before load subplot CONDITION TABLE, first their files so it does not get confused with plot CONDITION TABLE
file.rename(
    from = list.files('rawData/FIA/csv', pattern = 'SUBP_COND', full.names = TRUE),
    to = gsub(
        'SUBP_COND',
        'COND_SUBP',
        list.files('rawData/FIA/csv', pattern = 'SUBP_COND', full.names = TRUE)
    )
)

# load SUBPLOT CONDITION TABLE
condSub_tb <- fread(
    cmd = "awk 'NR==1||FNR!=1' rawData/FIA/csv/*_COND_SUBP.csv",
    header = T,
    select = c(
        'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'SUBP', 'INVYR',
        'CONDID', 'MICRCOND_PROP', 'SUBPCOND_PROP', 'MACRCOND_PROP'
    )
)

# load CONDITION TABLE
cond_tb <- fread(
    cmd = "awk 'NR==1||FNR!=1' rawData/FIA/csv/*_COND.csv",
    header = T,
    select = c(
        'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'INVYR', 'CONDID',
        'STDORGCD', 'COND_STATUS_CD', 'STDAGE', 'STDSZCD', 'SLOPE',
        'ASPECT', 'DSTRBCD1', 'DSTRBCD2', 'DSTRBCD3', 'TRTCD1', 'TRTCD2', 'TRTCD3', 'BALIVE', 'LIVE_CANOPY_CVR_PCT'
    )
)

# load PLOT TABLE
plot_tb <- fread(
    cmd = "awk 'NR==1||FNR!=1' rawData/FIA/csv/*_PLOT.csv",
    header = T,
    select = c(
        'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'LAT', 'LON',
        'INVYR', 'DESIGNCD', 'MANUAL', 'PLOT_STATUS_CD', 'KINDCD',
        'QA_STATUS', 'SAMP_METHOD_CD'
    )
)



# Prepare plot level information
############################################################

# merge plot_tb and surv_tb to get ANN_INVENTORY var
plot_tb[surv_tb, ANN_INVENTORY := i.ANN_INVENTORY, on = c('STATECD', 'INVYR')]

# Remove all subplots with code different than 1-4
condSub_tb <- condSub_tb[SUBP <= 4]

# Some subplots at a certain survey have more than one condition (~ 3%)
# If so, get the condition in which represents the highest proportion of the subplot (SUBPCOND_PROP)
condSub_tb[,
    nbCond := .N,
    by = .(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, INVYR)
]

maxProp <- function(x) {
    maxVec <- x == max(x, na.rm = TRUE)
     # If there are two max values, keep the first one
     if(sum(maxVec, na.rm = TRUE) > 1)
        maxVec[max(which(maxVec))] <- FALSE
    return( maxVec )
}

condSub_tb[nbCond > 1,
    maxProp := maxProp(SUBPCOND_PROP),
    by = .(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, INVYR)
]
condSub_tb[nbCond == 1, maxProp := TRUE]

# filter to keep only the condition with higher proportion in the subplot
condSub_tb <- condSub_tb[maxProp == TRUE]
condSub_tb[, c('nbCond', 'maxProp') := NULL]


# merge condSub and cond tables so every plot/subplot/year has its own condition
condPlotSub_tb <- merge(
    condSub_tb, cond_tb,
    by = c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'INVYR', 'CONDID'),
     all.x = TRUE
)


# merge plot_tb and cond_tb to get all plot/year conditions
plot_tb <- merge(
    plot_tb, condPlotSub_tb,
    by = c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'INVYR')
)


# create plot_id
plot_tb[, plot_id := paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '.')]
plot_tb[, c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT') := NULL]



# I can't load the TREE table from all states because of memory issues
# So here I first filter for plot_id that I'm interested in keeping, then
# load the TREE table for each state individually to filter TREE measures within plots of intereset

# PLOT_ID FILTERS (from: https://ecologicaldata.org/wiki/forest-inventory-and-analysis-database)
    # - SURVEY.ANN_INVENTORY = "Y" - selects only those surveys done using the modern, standardized methodology
    # - PLOT.PLOT_STATUS_CD = 1: Sampled - at least one accessible forest land condition present
    # - PLOT.KINDCD > 0 AND < 4: Standard National Design plots - basically NOT 0 (Periodic inventory plot - pre 1990s, non standardized) AND NOT 4 (Modeled periodic inventory plot - conducted only in Northeat and North central regions)
    # - PLOT.DESIGNCD  = 1 or 220 or 240 or 311 - 314 or 328 or 502-505 - all basically sampled the same way (see Appendix B of Phase 2 documentation for explanations)
    # - PLOT.QA_STATUS = 1: Standard production plot
    # - PLOT.MANUAL >= 1: limits data to those collected using the standardized methodology described in the National FIA Field Guide
    # - PLOT.SAMP_METHOD_CD = 1: Field visited (as opposed to 2 = remote sensed)
    # - PLOT.INVR < 9999 (INVYR is set to 9999 to distinguish those Western Phase 3 plots that are “off subpanel”. This is due to differences in measurement intervals between Phase 3 (measurement interval=5 years) and Phase 2 (measurement interval=10 years) plots. Only users interested in performing certain Phase 3 data analyses should access plots with this anomalous value in INVYR.)
    # - COND.TRTCD1, TRTCD2, and TRTCD3 = 00: No observable treatment (i.e., no cutting, girdling, herbicides, etc.), but most records are actually NULL in these fields
    # - COND.STDORGCD - Stand origin code - 0 = Natural stand (vs. 1 = clear evidence of artificial regeneration)
plot_tb <- plot_tb[,
    toKeep :=   ANN_INVENTORY == 'Y' &
                PLOT_STATUS_CD == 1 &
                KINDCD > 0 & KINDCD < 4 &
                DESIGNCD %in% c(1, 220, 240, 311:314, 328, 502:505) &
                QA_STATUS == 1 &
                MANUAL > 1 &
                SAMP_METHOD_CD == 1 &
                INVYR < 9999 &
                TRTCD1 == 0 & TRTCD2 == 0 & TRTCD3 == 0 &
                STDORGCD == 0
]

# assure that all surveys for a plot respect the filter above
plot_tb[, toKeep_plot := all(toKeep), by = plot_id]

plot_tb <- plot_tb[toKeep_plot == TRUE]
plot_tb[, c('toKeep', 'toKeep_plot') := NULL]


# Some plots have different DESIGNCD depending on INVYR
# I manually checked multiple plots with different DESIGNCD and everything looked fine
# 5575 plots (33%) have two DESIGNCD
# Adding a note to these plots in case I find a problem in the future
plot_tb[, NOTE := as.character()]
plot_tb[, NOTE := ifelse(length(unique(DESIGNCD)) > 1, 'Plot with more than one DESIGNCD', NA), by = plot_id]




# Load TREE table filtering for plot_id year of interest
# And merge with plot_tb
############################################################

plot_year_id <- plot_tb[, unique(paste0(plot_id, INVYR))]

tree_tb <- data.table()
for(st in states)
{
    # load TREE TABLE for specific state
    tree_tb_st <- fread(
        file = paste0('rawData/FIA/csv/', st, '_TREE.csv'),
        header = T,
        select = c(
            'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'SUBP', 'INVYR', 'TREE',
            'STATUSCD', 'SPCD', 'SPGRPCD', 'DIA', 'DIAHTCD', 'HT', 'HTCD', 'ACTUALHT', 'CR', 'CCLCD', 'AGENTCD', 'DAMLOC1', 'DAMTYP1',
            'DAMSEV1', 'DIACHECK', 'RECONCILECD'
        )
    )

    # assign plot_id
    tree_tb_st[, plot_id := paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '.')]
    tree_tb_st[, plot_id_year := paste0(plot_id, INVYR)]

    # select measures within plot_id / INVYR
    tree_tb_st <- tree_tb_st[plot_id_year %in% plot_year_id]
    
    # clean columns
    tree_tb_st[, c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT', 'plot_id_year') := NULL]

    tree_tb <- rbind(
        tree_tb,
        tree_tb_st
    )
}

# merge with plot_tb
tree_tb <- merge(
    tree_tb,
    plot_tb,
    by = c('plot_id', 'SUBP', 'INVYR')
)


# Assing plot size
tree_tb$subPlot_size <- pi * (24/3.281)^2

# assing tree_id
tree_tb[, tree_id := paste(plot_id, SUBP, TREE, sep = '.')]
tree_tb[, TREE := NULL]


# Species name
############################################################

# load csv with spCode reference
spRef <- fread('https://raw.githubusercontent.com/QUICC-FOR/QUICCSQL/1740964a2ec96eaed1f35fa8908a5426c4fa70e5/ref_species_R/final_ref_table.csv')[, c('us_code', 'tsn', 'id_spe'), with = FALSE]
spRef[, SPCD := us_code]
spRef[, us_code := NULL]

# table with codes to fix (duplicated code)
fix_spCode <- fread('rawData/FIA/FIA_spFixCode.csv')

for(i in 1:nrow(fix_spCode))
    tree_tb[SPCD == fix_spCode$us_code[i], SPCD := fix_spCode$new_code[i]]


# Check if all species code are present in reference table
missingSp <- setdiff(unique(tree_tb$SPCD), spRef$SPCD) # 9 spIds have no reference

# load FIA species reference code
FIA_sp <- fread('rawData/FIA/REF_SPECIES.csv')[SPCD %in% missingSp, c('SPCD', 'GENUS', 'SPECIES')]
FIA_sp[, spName := paste(GENUS, SPECIES)]


# Use taxize to find TSN for the missing species 
# TODO: taxize not working...
FIA_sp$tsn <- c(195176, 195108, 19420, 23580, 27807, 19385, 28573, 504254, 24791)
FIA_sp[, id_spe := paste0(tsn, '-', substr(toupper(GENUS), 1, 3), '-', substr(toupper(SPECIES), 1, 3))]

spRef <- rbind(
    spRef,
    FIA_sp[, c('SPCD', 'tsn', 'id_spe')]
)

spRef[, spIds := gsub('-', '', id_spe)]

# merge with tree_tb
tree_tb[spRef, species_id := i.spIds, on = 'SPCD']
tree_tb[, SPCD := NULL]

# save
write.csv(spRef, file.path('data', 'FIA', 'sp_ref.csv'))



# Rename columns
############################################################

setnames(
    tree_tb,
    old = c(
        'SUBP', 'INVYR', 'STATUSCD', 'SPGRPCD', 'DIA', 'DIAHTCD', 'HT', 'HTCD', 'ACTUALHT', 'CR', 'CCLCD', 'AGENTCD', 'DAMLOC1', 'DAMTYP1', 'DAMSEV1', 'DIACHECK', 'RECONCILECD', 'LAT', 'LON', 'DESIGNCD', 'MANUAL', 'KINDCD', 'COND_STATUS_CD', 'STDAGE', 'STDSZCD', 'SLOPE', 'ASPECT', 'DSTRBCD1', 'DSTRBCD2', 'DSTRBCD3', 'TRTCD1', 'TRTCD2', 'TRTCD3', 
        'BALIVE', 'LIVE_CANOPY_CVR_PCT', 'NOTE'
    ),
    new = c(
        'subplot_id', 'year_measured', 'status', 'species_group', 'dbh', 'dbh_code', 'total_height', 'height_code', 'actual_height', 'compacted_crown', 'crown_status', 'cause_of_death', 'damage_loc1', 'damage_type1', 'damage_serverity1', 'diameter_check_code', 'reconcile_code', 'latitude', 'longitude', 'design_id', 'manual', 'plotSample_code', 'plotCondition_code', 'stand_age', 'standSize_code', 'plot_slope', 'slope_aspect', 'plot_disturbance1', 'plot_disturbance2', 'plot_disturbance3', 'plot_treatment1', 'plot_treatment2', 'plot_treatment3', 'plot_basalArea', 'plot_canopyCover', 'note'
    )
)

tree_tb[, c('QA_STATUS', 'SAMP_METHOD_CD', 'ANN_INVENTORY', 'CONDID', 'MICRCOND_PROP', 'SUBPCOND_PROP', 'MACRCOND_PROP', 'STDORGCD') := NULL]


# save
saveRDS(tree_tb, file.path('data', 'FIA', 'tree_tb.RDS'))
