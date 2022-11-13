#------------------------------------------------------
# Clean data
# Will Vieira
# July 28, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Remove individuals from micro plot
#   - Keep only alive and dead status
#   - Fix resurrected
#   - Remove already dead and multiple dead events
#   - Fix indivudals with multiple species_id
#   - Remove plots with missing climate
#------------------------------------------------------


    
library(data.table)

treeData <- readRDS('data/FIA/treeData_sStar_clim.RDS')




# Sort data by year_measured (within plot and subplot)a
#------------------------------------------------------
setorderv(
    treeData,
    cols = c('plot_id', 'subplot_id', 'year_measured'),
    order = c(1, 1, 1)
)



# Remove individuals from micro plot (subsection within subplot)
# Represents 5% of observation
#------------------------------------------------------

treeData <- treeData[dbh >= 127]



# Keep only alive and dead status
# Removing 0.03% with other status
#------------------------------------------------------

treeData <- treeData[status %in% c(1, 2)]



# Resurrected
# In this case, I will change the status of the observation
# assuming the latest is the correct one
# Only 465 tree_ids ( 0.06%)
#------------------------------------------------------

check_resur <- function(st)
{
    # get position from dead and alive events
    dead_pos <- which(st == 2)
    alive_pos <- which(st == 1)

    if(length(dead_pos) > 0)
    {
        # check if all alive events are before dead ones
        t1_minus <- sapply(
            alive_pos,
            function(x)
                x < dead_pos
        )

        return( all(t1_minus) )
    }else{
        return( TRUE )
    }
}
treeData[, isGood := check_resur(status), by = tree_id]

# Function to fix wrong status
fix_resur <- function(x)
{
    # get last measure
    last_status <- x[length(x)]

    # if last measure is alive, all other must be alive
    if(last_status == 1) {
        x[x != 1] <- 1
        return( x )
    }else{
        # if last measure is dead, then fix between first and last dead event
        x[min(which(x == 2)):length(x)] <- 2
        return( x )
    }
}
treeData[isGood == FALSE, status := fix_resur(status), by = tree_id]

# For those plots/subplots that had status changed,
# recalculate all competition metrics
plots_to_calculate <- unique(
    treeData[is.na(indBA) & isGood == FALSE & status == 1, .(plot_id, subplot_id)]
)

# Individual BA
treeData[
    is.na(indBA) & status == 1,
    indBA := pi * (dbh/(2 * 1000))^2
]

for(i in 1:nrow(plots_to_calculate))
{
    pt_i <- plots_to_calculate$plot_id[i]
    spt_i <- plots_to_calculate$subplot_id[i]

    # clean cols to be recalculated
    treeData[
        plot_id == pt_i & subplot_id == spt_i,
        c('BA_plot', 'BA_sp') := NA
    ]

    # calculate plot basal area (BA in m2/ha)
    treeData[
        plot_id == pt_i & subplot_id == spt_i & status == 1,
        BA_plot := sum(indBA) * 1e4/subPlot_size,
        by = year_measured
    ]

    # fill NAs of BA (due to dead trees) with the value from the plot
    treeData[
        plot_id == pt_i & subplot_id == spt_i,
        BA_plot := nafill(nafill(BA_plot, "locf"), "nocb"),
        by = year_measured
    ]

    # species basal area per plot (BA_sp) as a proxy of seed source
    treeData[
        plot_id == pt_i & subplot_id == spt_i & status == 1,
        BA_sp := sum(indBA) * 1e4/subPlot_size,
        by = .(year_measured, species_id)
    ]

    # fill NAs the same as for BA
    treeData[
        plot_id == pt_i & subplot_id == spt_i,
        BA_sp := nafill(nafill(BA_sp, "locf"), "nocb"),
        by = .(year_measured, species_id)
    ]

    # Species relative basal area to overcome the potential opposite response of
    # regeneration in function of BA (i.e. competition) and BA_sp (i.e. seed source)
    treeData[
        plot_id == pt_i & subplot_id == spt_i,
        relativeBA_sp := BA_sp/BA_plot,
        by = .(year_measured, species_id)
    ]
    # 0/0 = NA
    treeData[
        plot_id == pt_i & subplot_id == spt_i & is.na(relativeBA_sp), relativeBA_sp := 0
    ]

    # Basal area of larger individuals than the focal individual (competitive index)
    BA_comp <- function(size, plotSize, BA_ind) {
        sapply(
        size,
        function(x)
            sum(BA_ind[size > x]) * 1e4/plotSize
        )
    }

    treeData[
        plot_id == pt_i & subplot_id == spt_i & status == 1,
        BA_comp := BA_comp(dbh, unique(subPlot_size), indBA),
        by = .(year_measured)
    ]

    # Individual basal area relative to the plot basal area
    treeData[
        plot_id == pt_i & subplot_id == spt_i & status == 1,
        relativeBA_comp := indBA/sum(indBA),
        by = .(year_measured)
    ]
}



# Remove already dead trees (that were never observed alive)
# Removing 8% of tree_id; 6% of observations
#------------------------------------------------------

treeData[, nbMeasure := .N, by = tree_id]
treeData[, nbDead := sum(status == 2), by = tree_id]

treeData <- treeData[nbMeasure != nbDead]



# Remove multiple dead events
# Because I am interested only in the alive -> dead event
#------------------------------------------------------

treeData[, nbDead := sum(status == 2), by = tree_id]

rm_multi_dead <- function(st)
{
    # which is the first dead event?
    first_dead <- min(which(st == 2))

    # vector to keep
    toKeep <- st == 1
    toKeep[first_dead] <- TRUE

    return( toKeep )
}

treeData[nbDead > 1, toKeep := rm_multi_dead(status), by = tree_id]
treeData <- treeData[toKeep == TRUE | is.na(toKeep)]



# Deal with tree_id with multiple species name
# 2.3% of tree_ids have more than one species code
# 1/3 of those are from trees with different design_id (discart)
#------------------------------------------------------

treeData[, nbSp := length(unique(species_id)), by = tree_id]

# Remove those with different design_id
treeData[, nbMeasure := .N, by = tree_id]
treeData[, nbDes := length(unique(design_id)), by = tree_id]

rm_diffDesign <- function(sp, des)
{
    if(length(des) == 2) {
        return( c(T, T) )
    }else{
        return( des != 1 )
    }
}

treeData[
    nbSp > 1 & nbDes > 1,
    toRm := rm_diffDesign(species_id, design_id),
    by = tree_id
]
treeData <- treeData[toRm != TRUE | is.na(toRm)]


# For the remaining individuals with more than one species_id, but that have
# the same design, I will assume the last measure is the correct
fix_sp <- function(sp)
{
    n <- length(sp)
    return( rep(sp[n], n) )
}
treeData[nbSp > 1, species_id := fix_sp(species_id), by = tree_id]



# Remove plots with missing climate
# Only one plot is missing climate
#------------------------------------------------------

# get NA by plot_id and year_measured
NA_plot <- treeData[, sum(is.na(bio_01_mean)), by = .(plot_id, year_measured)]

# Filter for years with climate data (up to 2018)
plot_toRm <- NA_plot[year_measured < 2019 & V1 > 0, unique(plot_id)]

treeData <- treeData[plot_id != plot_toRm]



# Clean columns
#------------------------------------------------------

treeData[,
    c(
        'sumArea', 'isGood', 'nbMeasure', 'toRm',
        'nbDead', 'toKeep', 'nbSp', 'nbDes'
    ) := NULL
]
