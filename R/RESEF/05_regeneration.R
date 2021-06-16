##############################
# Prepare regeneration dataset
# Will Vieira
# Jan 21, 2021
# Last edited: May 26, 2021
##############################


##############################
# regCover: percentage cover of plants with height < 1.3 m
# nbStems: stems height > 1.3 m and dbh < 1.1 cm
##############################


##############################
# Objectives
# - Recreate 3 separated datasets using reg_data and tree_data:
#   - seedling: regCover or percentage cover of plants with height < 1.3 m
#   - sapling: number of stems with height > 1.3 m and dbh < 1 cm
#   - juvenile1: number of stems with dbh > 1 cm and dbh < 5.1 cm
#   - juvenile2: number of stems with dbh > 5.1 cm and dbh < 9.1 cm
##############################


##############################
# Steps
# - For seedling:
#    - Calculate basal area of all adults within target subplot (competition effect)
#    - Calculate focal species basal area for adults within neighbor subplot around target subplot (seed source effect)
# - For sapling and juvenile:
# - Same competition effect as in seedling
# - Create lag regCover as new variable as a source of seedling to sapling
# - Create lag sapling as a source to juvenile
##############################


# Load tree_data before transition transformation
#load('data/RESEF/tree_reg_RESEF_beforeTransition.Rdata')
tree_data <- readRDS('data/RESEF/tree_data_beforeTransition.RDS')


# TODO if I tranform regCover into number of regeneration I must do it here

# Function to get subplot IDs of the 8 neighborhood around a target subplot
# Input is the target subplot and all the subplots within the plot
getNeighbors <- function(subplot_id, subplots)
{
    # get first and second element of subplot id
    firstEl <- as.numeric(substring(subplot_id, 1, 1))
    secondEl <- as.numeric(substring(subplot_id, 2, 2))

    # get max of x and y over all subplots within a plot
    max_X <- max(as.numeric(substring(subplots, 1, 1)))
    max_Y <- max(as.numeric(substring(subplots, 2, 2)))

    # create a vector
    firstEl_vec <- c(firstEl - 1, firstEl, firstEl + 1)
    secondEl_vec <- c(secondEl - 1, secondEl, secondEl + 1)

    # expand possibilites
    exp_mt <- expand.grid(firstEl_vec, secondEl_vec)

    # Remove negative plots
    exp_mt <- exp_mt[!apply(exp_mt, 1, function(x) any(x < 0)), ]

    # Remove subplots outside plot (bigger than max_*)
    exp_mt <- exp_mt[!exp_mt$Var1 > max_X, ]
    exp_mt <- exp_mt[!exp_mt$Var2 > max_Y, ]

    # reduce expanded matrix in a vector combining first and second element
    paste0(exp_mt$Var1, exp_mt$Var2)
}




##################################################################################
# Seedling
##################################################################################

seedling <- data.table()

# Loop over plot
for(plotID in unique(tree_data$plot_id))
{
    treePlot <- tree_data[plot_id == plotID]
    regPlot <- reg_data[plot_id == plotID]

    # years of measurement for tree_data and reg_data
    plotYears_tree <- sort(unique(c(treePlot[, year0], treePlot[, year1])))
    plotYears_reg <- regPlot[, unique(year)]
    
    # loop over years within plot
    for(Year in 2:length(plotYears_reg))
    {
        # continue only if we find data in this specific year for reg_data
        if(regPlot[year == plotYears_reg[Year], .N] > 0)
        {
            # Create generic row with information at the plot and year level
            # This will be used later when new rows will be added for species with 0 regeneration
            empty_row <- regPlot[which(year == plotYears_reg[Year])[1]]
            empty_row <- empty_row[, `:=`(subplot_id = NA, regCover = 0, spCode = NA)]

            # Calculate deltaYear
            deltaYear <- plotYears_reg[Year] - plotYears_reg[Year - 1]

            # loop over subplots within plot and year
            for(subplotID in treePlot[, unique(subplot_id)])
            {
                # get neighborhood subplots
                neighbors <- getNeighbors(subplotID, treePlot[, unique(subplot_id)])
                
                # Total area of neighbors subplot to calculate basal area
                neighborsArea <- length(neighbors) * 100
                            
                # get alive adults within plot, year - 1, and subplot neighbors
                treeAdults <- treePlot[year0 == plotYears_tree[which(plotYears_tree == plotYears_reg[Year]) - 1] &
                                       subplot_id %in% neighbors &
                                       state0 == 'alive']
                
                # total basal area for target subplot (competition effect)
                subplot_BA <- treeAdults[subplot_id == subplotID & state1 == 'alive', sum(indBA) * 1e4/100]

                # The total basal area of individuals who died
                subplot_BA_dead <- treeAdults[subplot_id == subplotID & state1 == 'dead', sum(indBA) * 1e4/100]
                
                # species specific basal area within neighbors subplots
                sp_BA <- setNames(treeAdults[state1 == 'alive', sum(indBA) * 1e4/neighborsArea, by = spCode], c('spCode', 'BA_sp'))

                # recreate the whole table
                final_regPlot <- regPlot[subplot_id == subplotID & year == plotYears_reg[Year]]

                # Add rows for adult species in the neighbors subplot that does not have regeneration
                spsToAdd <- sp_BA[!spCode %in% final_regPlot$spCode, spCode]
                if(length(spsToAdd) > 0)
                {
                    rowsToAdd <- empty_row[rep(1, length(spsToAdd)), ]
                    rowsToAdd <- rowsToAdd[, `:=`(subplot_id = rep(subplotID, length(spsToAdd)), spCode = spsToAdd)]
                    final_regPlot <- rbind(final_regPlot, rowsToAdd)
                }

                # add adult information into final_regPlot
                final_regPlot <- merge(final_regPlot, sp_BA, by = 'spCode', all = TRUE)
                final_regPlot[, `:=`(subplot_BA = subplot_BA,
                                     subplot_BA_dead = subplot_BA_dead,
                                     deltaYear = deltaYear)]

                # Some plots have reneration but not a single adult in the neighbors (prob large dispersal)
                # Add zero to BA_sp for these species without any close adult
                final_regPlot[is.na(BA_sp), BA_sp := 0]

                # Remove nbStems
                final_regPlot[, nbStems := NULL]
                
                # merge into final data.table
                seedling <- rbind(seedling, final_regPlot)
            }
        }else{
            cat('Plot', plotID, '; year', plotYears_reg[Year], 'did not have data\n')
        }

    }
    cat('   Creating seedling dataset -> plot', which(plotID == unique(tree_data$plot_id)), 'of', length(unique(tree_data$plot_id)), '\r')
}




##################################################################################
# Sapling
##################################################################################


sapling <- data.table()

# Loop over plot
for(plotID in unique(tree_data$plot_id))
{
    treePlot <- tree_data[plot_id == plotID]
    regPlot <- reg_data[plot_id == plotID]

    # years of measurement for tree_data and reg_data
    plotYears_tree <- sort(unique(c(treePlot[, year0], treePlot[, year1])))
    plotYears_reg <- regPlot[, unique(year)]
    
    # loop over years within plot
    for(Year in 2:length(plotYears_reg))
    {
        # continue only if we find data in this specific year for reg_data
        if(regPlot[year == plotYears_reg[Year], .N] > 0)
        {
            # Create generic row with information at the plot and year level
            # This will be used later when new rows will be added for species with 0 regeneration
            empty_row <- regPlot[which(year == plotYears_reg[Year])[1]]
            empty_row <- empty_row[, `:=`(subplot_id = NA, nbStems = 0, spCode = NA)]

            # Calculate deltaYear
            deltaYear <- plotYears_reg[Year] - plotYears_reg[Year - 1]

            # loop over subplots within plot and year
            for(subplotID in treePlot[, unique(subplot_id)])
            {
                # get alive adults within plot, year - 1
                treeAdults <- treePlot[year0 == plotYears_tree[which(plotYears_tree == plotYears_reg[Year]) - 1] &
                                       subplot_id == subplotID &
                                       state0 == 'alive']
                
                # total basal area for target subplot (competition effect)
                subplot_BA <- treeAdults[state1 == 'alive', sum(indBA) * 1e4/100]

                # The total basal area of individuals who died
                subplot_BA_dead <- treeAdults[state1 == 'dead', sum(indBA) * 1e4/100]
                
                # regeneration cover of subplot from year - 1
                reg_sp <- setNames(
                            regPlot[subplot_id == subplotID &
                                    year == plotYears_reg[Year - 1] &
                                    regCover > 0,
                                    c('spCode', 'regCover'), with = FALSE],
                            c('spCode', 'regCover_lag'))

                # recreate the whole table
                final_regPlot <- regPlot[subplot_id == subplotID & year == plotYears_reg[Year]]

                # Add rows for adult species in the neighbors subplot that does not have regeneration
                spsToAdd <- reg_sp[!spCode %in% final_regPlot$spCode, spCode]
                if(length(spsToAdd) > 0)
                {
                    rowsToAdd <- empty_row[rep(1, length(spsToAdd)), ]
                    rowsToAdd <- rowsToAdd[, `:=`(subplot_id = rep(subplotID, length(spsToAdd)),
                                                  spCode = spsToAdd)]
                    final_regPlot <- rbind(final_regPlot, rowsToAdd)
                }

                # add adult information into final_regPlot
                final_regPlot <- merge(final_regPlot, reg_sp, by = 'spCode', all = TRUE)
                final_regPlot[, `:=`(subplot_BA = subplot_BA,
                                     subplot_BA_dead = subplot_BA_dead,
                                     deltaYear = deltaYear)]
                
                # Add zero to regCover_lag for the species with nbStems but no regCover on the year - 1
                final_regPlot[is.na(regCover_lag), regCover_lag := 0]

                # Remove species in which both regCover_lag AND nbStems equals zero
                final_regPlot <- final_regPlot[!(nbStems == 0 & regCover_lag == 0)]

                # Remove regCover of current year
                final_regPlot[, regCover := NULL]
                
                # merge into final data.table
                sapling <- rbind(sapling, final_regPlot)
            }
        }else{
            cat('Plot', plotID, '; year', plotYears_reg[Year], 'did not have data\n')
        }
    }
    cat('   Creating sapling dataset -> plot', which(plotID == unique(tree_data$plot_id)), 'of', length(unique(tree_data$plot_id)), '\r')
}



##################################################################################
# Juvenile 1 and 2
##################################################################################


juvenile1 <- juvenile2 <- data.table()

# Loop over plot
for(plotID in unique(tree_data$plot_id))
{
    treePlot <- tree_data[plot_id == plotID]

    # years of measurement for tree_data
    plotYears_tree <- sort(unique(c(treePlot[, year0], treePlot[, year1])))
    
    # loop over years within plot
    for(Year in 2:length(plotYears_tree))
    {
        # continue only if we find data in this specific year for reg_data
        if(treePlot[year1 == plotYears_tree[Year], .N] > 0)
        {
            # Create generic row with information at the plot and year level
            # empty_row <- treePlot[year1 == plotYears_reg[Year]][1, ]
            # empty_row <- empty_row[, `:=`(subplot_id = NA, nbStems = 0, spCode = NA)]

            for(subplotID in treePlot[, unique(subplot_id)])
            {
                # get alive individuals from plot_id, year, and subplot_id
                treeSub <- treePlot[year1 == plotYears_tree[Year] &
                                    subplot_id == subplotID &
                                    state1 == 'alive']
                
                # number of trees by species for the two classes of size (juv1 and juv2)
                nbJuv1 <- treeSub[dbh1 <= 51, .N, by = spCode]
                nbJuv2 <- treeSub[dbh1 > 51 & dbh1 < 91, .N, by = spCode]

                # get number of saplings for the subplot a year before (source of juv1)
                nbSapling_lag <- setNames(
                                   reg_data[plot_id == plotID &
                                            subplot_id == subplotID &
                                            year == plotYears_tree[Year - 1] &
                                            nbStems > 0,
                                            c('nbStems', 'spCode'), with = FALSE],
                                   c('sapling_lag', 'spCode'))

                # get number of juv1 for the subplot a year before (source of juv2)
                nbJuv1_lag <- setNames(
                                treePlot[year1 == plotYears_tree[Year] &
                                        subplot_id == subplotID &
                                        state0 == 'alive' &
                                        dbh0 <= 51,
                                        .N, by = spCode],
                                c('spCode', 'juv1_lag'))

                # Merge current number of juv with source information
                nbJuv1 <- merge(nbJuv1, nbSapling_lag, by = 'spCode', all = TRUE)
                nbJuv2 <- merge(nbJuv2, nbJuv1_lag, by = 'spCode', all = TRUE)

                # Fill NA information with zero (missing N or lag info)
                setnafill(nbJuv1, fill = 0, cols = c('N', 'sapling_lag'))
                setnafill(nbJuv2, fill = 0, cols = c('N', 'juv1_lag'))

                # Recreate the dataset with information at the plot level
                rowsToAdd <- treeSub[1, setdiff(names(treeSub),
                                                c('tree_id', 'canopyState', 'heightMeasured',
                                                'height', 'x', 'y', 'obs', 'spCode', 'indBA',
                                                'cumX', 'cumY', 'maxRadius', 'neighborBA',
                                                'neighborBA2', 'BA_sp', 'relativeBA_sp', 'nbMeasure',
                                                'year0', 'dbh1', 'dbh0', 'growth', 'state_original',
                                                'state1', 'state0')), with = FALSE]
                
                juv1 <- cbind(rowsToAdd[rep(1, nrow(nbJuv1)), ], nbJuv1)
                juv2 <- cbind(rowsToAdd[rep(1, nrow(nbJuv2)), ], nbJuv2)

                # merge into final data.table
                juvenile1 <- rbind(juvenile1, juv1)
                juvenile2 <- rbind(juvenile2, juv2)
            }
        }else{
            cat('Plot', plotID, '; year', plotYears_reg[Year], 'did not have data\n')
        }
    }
    cat('   Creating juvenile dataset -> plot', which(plotID == unique(tree_data$plot_id)), 'of', length(unique(tree_data$plot_id)), '\r')
}



# Save

saveRDS(seedling, file = 'data/RESEF/seedling_dt.RDS')
saveRDS(sapling, file = 'data/RESEF/sapling_dt.RDS')
saveRDS(juvenile1, file = 'data/RESEF/juvenile1_dt.RDS')
saveRDS(juvenile2, file = 'data/RESEF/juvenile2_dt.RDS')


#





################################################
# Prepare data for JAGS model
################################################



#############
# Steps
# - 4 size stages: sapling, juv1, juv2, and adults
# - For each stage:
# - Create a matrix with row sites (plot+subplot_id) and year columns
#############

# load data sets just in case
sapling <- readRDS('data/RESEF/sapling_dt.RDS')
juvenile1 <- readRDS('data/RESEF/juvenile1_dt.RDS')
juvenile2 <- readRDS('data/RESEF/juvenile2_dt.RDS')
adults <- readRDS('data/RESEF/tree_data_beforeTransition.RDS')


# Filter plot_id with at least 5 measures to assure enough temporal variability
uqYear_plot <- sapling[, unique(year), by = plot_id]
plotToKeep <- uqYear_plot[, .N, by = plot_id][N >= 5, plot_id]
uqYear_plot <- uqYear_plot[plot_id %in% plotToKeep]

# Create a plot_id x year unique ID so all four datasets have the same spatio-temporal extension
uqYear_plot[, plotYear_id := paste(plot_id, V1, sep = '_')]

# prepare unique site_ID for all plot_ids
uqSubplot_plot <- sapling[plot_id %in% plotToKeep, unique(subplot_id), by = plot_id]
uqSubplot_plot[, site := paste(plot_id, V1, sep = '_')]


# Get the largest species among the 4 datasets
spIds <- 
names(
    which(
        table(
            c(
                sapling[plot_id %in% plotToKeep, .N, by = spCode][order(N, decreasing = T)[1:13], spCode],
                juvenile1[plot_id %in% plotToKeep, .N, by = spCode][order(N, decreasing = T)[1:13], spCode],
                juvenile2[plot_id %in% plotToKeep, .N, by = spCode][order(N, decreasing = T)[1:13], spCode],
                adults[plot_id %in% plotToKeep, .N, by = spCode][order(N, decreasing = T)[1:13], spCode]
            )
        ) == 4
    )
)


# Create a subplot + plot unique ID and plot_id + year ID
sapling[, `:=`(site = paste0(plot_id, '_', subplot_id), plotYear_id = paste0(plot_id, '_', year))]
juvenile1[, `:=`(site = paste0(plot_id, '_', subplot_id), plotYear_id = paste0(plot_id, '_', year1))]
juvenile2[, `:=`(site = paste0(plot_id, '_', subplot_id), plotYear_id = paste0(plot_id, '_', year1))]
adults[, `:=`(site = paste0(plot_id, '_', subplot_id), plotYear_id = paste0(plot_id, '_', year))]


# Filter for plot_id with at least 5 measures and for 
sapling <- sapling[site %in% uqSubplot_plot$site & plotYear_id %in% uqYear_plot$plotYear_id]
juvenile1 <- juvenile1[site %in% uqSubplot_plot$site & plotYear_id %in% uqYear_plot$plotYear_id]
juvenile2 <- juvenile2[site %in% uqSubplot_plot$site & plotYear_id %in% uqYear_plot$plotYear_id]
adults <- adults[site %in% uqSubplot_plot$site & plotYear_id %in% uqYear_plot$plotYear_id]


# Fill the empty matrix for each stage class and species_id
out_sp <- list()
for(spId in spIds)
{

    # Species specific df
    sapling_sp <- sapling[spCode == spId]
    juvenile1_sp <- juvenile1[spCode == spId]
    juvenile2_sp <- juvenile2[spCode == spId]
    adults_sp <- adults[spCode == spId]

    # As the plots are not sampled on the same year, standardize the years to a sequence 1:6
    year_to_id <- function(y)
        return( factor(y, levels = unique(y), labels = 1:length(unique(y))) )

    sapling_sp[, year_id := year_to_id(year), by = plot_id]
    juvenile1_sp[, year_id := year_to_id(year1), by = plot_id]
    juvenile2_sp[, year_id := year_to_id(year1), by = plot_id]
    adults_sp[, year_id := year_to_id(year), by = plot_id]

    # As adults are individually marked, count number of alive individuals by site and year_id
    # Also filter for dbh >= 91 mm as the definition of adults
    adults_sp <- adults_sp[state2 == 'alive' & dbh >= 91, .N, by = .(site, year_id)]

    # Long to large format
    sapling_la <- dcast(sapling_sp, site ~ year_id, value.var = 'nbStems')
    juvenile1_la <- dcast(juvenile1_sp, site ~ year_id, value.var = 'N')
    juvenile2_la <- dcast(juvenile2_sp, site ~ year_id, value.var = 'N')
    adults_la <- dcast(adults_sp, site ~ year_id, value.var = 'N')

    # There are some missing sites in the above as no individuals of target species where found at a specific survey
    # Find the missing sites and fill with 0 (true absences)
    sapling_la <- rbind(
        sapling_la,
        data.table(
            site = uqSubplot_plot$site[!uqSubplot_plot$site %in% unique(sapling_la$site)],
            `1` = NA, `2` = NA, `3` = NA, `4` = NA, `5` = NA, `6` = NA
        )
    )
    juvenile1_la <- rbind(
        juvenile1_la,
        data.table(
            site = uqSubplot_plot$site[!uqSubplot_plot$site %in% unique(juvenile1_la$site)],
            `1` = NA, `2` = NA, `3` = NA, `4` = NA, `5` = NA, `6` = NA
        )
    )
    juvenile2_la <- rbind(
        juvenile2_la,
        data.table(
            site = uqSubplot_plot$site[!uqSubplot_plot$site %in% unique(juvenile2_la$site)],
            `1` = NA, `2` = NA, `3` = NA, `4` = NA, `5` = NA, `6` = NA
        )
    )
    adults_la <- rbind(
        adults_la,
        data.table(
            site = uqSubplot_plot$site[!uqSubplot_plot$site %in% unique(adults_la$site)],
            `1` = NA, `2` = NA, `3` = NA, `4` = NA, `5` = NA, `6` = NA
        )
    )


    # As we subset the df to a specific species, and some years the target species is not found,
    # the operation above creates NAs values for years in which should be 0.
    # Next function will check for each plot_id what are the measured years that are still NA (and replace to 0)
    check_year <- function(cols, plotYear)
    {
        plotid <- sub("\\_.*", "", cols[1])
        yr_id <- 1:plotYear[plot_id == plotid, .N]

        # fill NA with 0
        years_id <- cols[2:length(cols)]
        years_id[yr_id][is.na(years_id[yr_id])] <- 0

        return( c(cols[1], as.numeric(years_id)) )
    }

    # Convert the data.frame to matrix where the col site becomes the rowname
    df2mt <- function(df, plotYear)
    {
        # first check year (returns a matrix of character)
        mt_ch <- t(apply(df, 1, check_year, plotYear = plotYear))

        # first col site to rownames
        rownames(mt_ch) <- mt_ch[, 1]
        
        # remove first col and rename the remain columns
        mt_ch <- t(apply(mt_ch[, -1], 1, as.numeric))
        
        # character to numeric
        colnames(mt_ch) <- 1:ncol(mt_ch)

        return( mt_ch )
    }

    sapling_mt <- df2mt(sapling_la, plotYear = uqYear_plot)
    juvenile1_mt <- df2mt(juvenile1_la, plotYear = uqYear_plot)
    juvenile2_mt <- df2mt(juvenile2_la, plotYear = uqYear_plot)
    adults_mt <- df2mt(adults_la, plotYear = uqYear_plot)

    # reorder the rows of each matrix so each row site match between them
    juvenile1_mt <- juvenile1_mt[match(rownames(sapling_mt), rownames(juvenile1_mt)), ]
    juvenile2_mt <- juvenile2_mt[match(rownames(sapling_mt), rownames(juvenile2_mt)), ]
    adults_mt <- adults_mt[match(rownames(sapling_mt), rownames(adults_mt)), ]

    out_sp[[spId]] <- 
        simplify2array(
            list(
                sapling_mt,
                juvenile1_mt,
                juvenile2_mt,
                adults_mt
            )
        )

}


# Save
saveRDS(out_sp, file = 'data/RESEF/jags_data.RDS')





# Viz JAGS data

# ABIBAL
os <- out_sp[['ABIBAL']]


# Stage variation over time for all sites
#######################################################

xLim <- c(1, 6)

par(mfrow = c(2, 2))
plot(0, pch = '', xlim = xLim, ylim = range(os[, , 1], na.rm = TRUE), main = 'Sapling')
for(i in 1:nrow(os[, , 1]))
{
    points(1:6, os[i, , 1], type = 'l', col = rgb(0, 0, 0, 0.3))
    points(1:6, os[i, , 1])
}
plot(0, pch = '', xlim = xLim, ylim = range(os[, , 2], na.rm = TRUE), main = 'Juvenile 1')
for(i in 1:nrow(os[, , 2]))
{
    points(1:6, os[i, , 2], type = 'l', col = rgb(0, 0, 0, 0.3))
    points(1:6, os[i, , 2])
}
plot(0, pch = '', xlim = xLim, ylim = range(os[, , 3], na.rm = TRUE), main = 'Juvenile 2')
for(i in 1:nrow(os[, , 3]))
{
    points(1:6, os[i, , 3], type = 'l', col = rgb(0, 0, 0, 0.3))
    points(1:6, os[i, , 3])
}
plot(0, pch = '', xlim = xLim, ylim = range(os[, , 4], na.rm = TRUE), main = 'Adults')
for(i in 1:nrow(os[, , 4]))
{
    points(1:6, os[i, , 4], type = 'l', col = rgb(0, 0, 0, 0.3))
    points(1:6, os[i, , 4])
}


# Plot variation
#######################################################

# Get sites without zero (better to viz)
sitesToPlot <- which(apply(os, 1, function(x) sum(x == 0, na.rm = T)) == 0)

par(mfrow = c(6, 7), mar = c(0.4, 2, 0.2, 0.2))
for(site in sitesToPlot)
{
    yLim <- range(os[site, , 1:4], na.rm = TRUE)

    plot(0, pch = '', xlim = xLim, ylim = yLim, xaxt = 'n')
    for(i in 1:4)
    {
        points(os[site, , i], col = i, type = 'l')
        points(os[site, , i], col = i)
    }
}
legend('topleft', legend = c('Sap', 'Juv1', 'Juv2', 'Adu'), lty = 1, col = 1:4, bty = 'n')




# Get site_ID in which any of Juv1, Juv2, or Adults have more individuals on t+1 compared to t
check_increment <- function(x)
{
    # this function will be applied at each site (row)
    # So it returns a matrix year rows x age cols
    # For cols 2:nAges, return:
    # - TRUE: if any t+1 have higher number than t
    for(i in 2:ncol(x))
    {
        for(j in 1:(nrow(x) - 1))
        {
            result <- x[j+1, i] > x[j, i]
            if(!is.na(result) & result)
                return(1)
        }
    }
    return(0)
}

rowIncr = apply(n, 1, check_increment)
