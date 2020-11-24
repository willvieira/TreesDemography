##############################
# Calculate competition
# Will Vieira
# Nov 18, 2020
##############################



##############################
# Steps
# - Transform subplot xy to a continuous xy at the plot level
# - Calculate individual height and maximum crown area using Purves et al. 2008 allometries
# - Calculate a competition index based on neighbor trees
# - Calculate subplot basal area
##############################






##########################################################################################
# Transform subplot xy to a continuous xy at the plot level
##########################################################################################

# subplots are numbered with two character numbers from 0 to 9, the first represents x and the second y
# So a subplot numbered `03` means that it is placed in the first row (0) and third (3) column of the plot

# first fix the subpots under 10 that have only one character (e.g. fix `3` to `03`)
tree_data[, subplot_id := sprintf('%02d', subplot_id)]
reg_data[, subplot_id := sprintf('%02d', subplot_id)]


# Fix tree_id with `NA` position
cat(round(tree_data[is.na(x), length(unique(tree_id))]/tree_data[, length(tree_id)] * 100, 2),
    '% of tree_id have not xy position;',
    'these trees have a mean dbh of', round(tree_data[is.na(x), mean(dbh)/10], 2), 'cm')

# get tree_id of NA position to note in obs
missingXY <- tree_data[is.na(x), unique(tree_id)]
tree_data[tree_id %in% missingXY, obs := paste0(obs, '; random xy position assigned by Will on nov 18, 2020')]

# get random positions at the subplot level for these trees
tree_data[is.na(x), x := sample(1:1000, 1), by = tree_id]
tree_data[is.na(y), y := sample(1:1000, 1), by = tree_id]


# Calculate cumulative position with te following formula
# xy + (1000 + subplot_id[ij]) where 1000 xy is either x or y at the subplot,
# 1000 is the size of subplot in cm, and i or j are the first or second character 

# Get first or second character of subplot_id with substring()
tree_data[, cumX := x + (1000 * as.numeric(substring(subplot_id, 1, 1)))]
tree_data[, cumY := y + (1000 * as.numeric(substring(subplot_id, 2, 2)))]





##########################################################################################
# Calculate individual height and maximum crown area from Purves 2008
##########################################################################################


# Calculate individual height (for missing individuals only)

    # 2.2% of data does not have height
    tree_data[is.na(height) & state2 == 'alive', .N]/tree_data[, .N] * 100

    # source parameters and functions from Amaël's GitHub repo
    devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/createData/parametersAllometries.R')
    devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/toolFunctions.R')

    # get parametrised species
    tmp <- tempfile()
    download.file('https://github.com/amael-ls/code_R0niche/raw/master/createData/ls_speciesParametrised.rds', tmp)
    parametrisedSpecies <- readRDS(tmp)
    parametrisedSpecies <- sps_code$spCode[sps_code$CODE %in% parametrisedSpecies[, 1]]

    # species not parametrised
    missingPars <- tree_data[, unique(spCode)][!tree_data[, unique(spCode)] %in% parametrisedSpecies]

    # Note I am estimating height from Purves on obs column
    tree_data[spCode %in% parametrisedSpecies & is.na(height) & state2 == 'alive', obs := paste0(obs, '; height estimated with parameters from Purves 2008')]

    ## Calculate height
    tree_data[, height := height/10] # convert height from cm to m
    tree_data[spCode %in% parametrisedSpecies & is.na(height) & state2 == 'alive', height := dbhToHeight(dbh, purves2007_allometries[species == sps_code$CODE[which(sps_code$spCode == spCode)], a],
        purves2007_allometries[species == sps_code$CODE[which(sps_code$spCode == spCode)], b], mm = TRUE), by = spCode]


    tree_data[is.na(height), .N]/nrow(tree_data) * 100
    # 1% of data does not have height (NA)
    # But most of this is due dead trees, which ecologicaly do not compete for light
    tree_data[is.na(height) & state2 == 'alive', .N]
    # Only 12 alive individuals do not have height
    # So for those species I get the heigth based in their DHP according to a regression line for the correlation between DHP and heigth
    # For those individuals already dead, I will keep height = 0 as they should not compete (for the S* calculation)
    srg <- summary(lm(height ~ poly(dbh, 2 , raw = TRUE), tree_data))
    tree_data[is.na(height) & state2 == 'alive', height := (dbh^2 * rnorm(1, srg$coefficients[3, 1], srg$coefficients[3, 2])) + (dbh * rnorm(1, srg$coefficients[2, 1], srg$coefficients[2, 2])) + rnorm(1, srg$coefficients[1, 1], srg$coefficients[1, 2])]

#




# Calculate maximum radius of crown area using Purves 2008

    dbhToMaxCrownArea = function(dbh, T_param, C0_C1)
    {
        # Table S2
        R0_C0 = C0_C1[parameter == "R0", C0]
        R0_C1 = C0_C1[parameter == "R0", C1]

        R40_C0 = C0_C1[parameter == "R40", C0]
        R40_C1 = C0_C1[parameter == "R40", C1]

        # Appendix S3, Eq S3.3 (erroneously denoted S2.3 in the article)
        R0 = (1 - T_param)*R0_C0 + T_param*R0_C1
        R40 = (1 - T_param)*R40_C0 + T_param*R40_C1

        # Calculate potential max radius Eq S1.6, /!\ dbh in mm /!\
        Rp_max = R0 + (R40 - R0)*dbh/400

        return(Rp_max)
    }

    # match parameters species with db species
    purves2007_allometries[, sp2 := gsub('-', '', sp)]
    purves2007_allometries <- purves2007_allometries[species != '28731-ACE-SAC'] # remove duplicated species

    # Add species especific `T` parameter to speed up calculation
    tree_data[, T_param := purves2007_allometries[sp2 == unique(spCode), T], by = spCode]

    # Fill the missing values of T_param (4% of the db) due to the 8 species missing specific parameters
    # I will feed these species with parameters from other species of the same genus
    # If any genus is fond, I get the mean from all species    
    for(sp in missingPars)
    {
        spMissGenus <- substring(sp, 1, 3) # get genus of missing species
        T_pars <- purves2007_allometries[substring(sp2, 1, 3) == spMissGenus, T] # T parameters of species with the same genus

        # In case of missing similar genus
        if(!length(T_pars) > 0)
            T_pars <- purves2007_allometries[, T]
        
        tree_data[spCode == sp, T_param := mean(T_pars, na.rm = TRUE)]
    }

    # Get max crown area (in meters given dbh is is mm)
    tree_data[, maxRadius := dbhToMaxCrownArea(dbh, as.numeric(T_param), C0_C1)]

    # remove T_param column
    tree_data[, T_param := NULL]

#





##########################################################################################
# Plot trees distribution within a plot
##########################################################################################

# Set TRUE to create animated gif
if(FALSE) {

    # angles for drawing points around the circle
    theta = seq(0, 2 * pi, length.out = 200)

    dir.create('plots')
    count <- 1; totalCount <- nrow(tree_data[, .N, by = .(plot_id, year)]) * 2
    for(Plot in tree_data[, unique(plot_id)])
    {
        plotyears <- tree_data[plot_id == Plot]
        # define max of stems for the plot
        nbStems_max <- reg_data[plot_id == Plot, sum(nbStems), by = .(subplot_id, year)][, max(V1)]

        # define limit of plot (either 5000x5000 or 1000x5000)
        xLim <- c(0, (max(as.numeric(plotyears[, substring(subplot_id, 1, 1)])) + 1) * 1000)
        yLim <- c(0, (max(as.numeric(plotyears[, substring(subplot_id, 2, 2)])) + 1) * 1000)

        # define size of figure based on size of plot
        figHeight = ifelse(yLim[2] == 5000, 6.72, 12.8)
        figWidth = ifelse(xLim[2] == 5000, 6.72, 12.8)

        # plot dbh only and max crown area
        for(i in c('dbh', 'crownArea'))
        {   
            for(yr in plotyears[, unique(year)])
            {
                png(filename = paste0('plots/plot', Plot, '_', yr, '_', i, '.png'), height = figHeight, width = figWidth, units = 'in', res = 300)
                toplot <- plotyears[year == yr]
                toplot[, rowpos := .I]
                toplot[, colState := ifelse(state2 == 'alive', '#0F6F18', '#DF0A0A')]
                toplot[, colStateTrans := ifelse(state2 == 'alive', '#0F6F184D', '#DF0A0A4D')]

                par(mar = rep(0, 4), oma = rep(0, 4))
                plot(xLim, yLim, type = 'n',axes = FALSE, ann = FALSE, asp = 1)

                # Grid
                segments(seq(1000, xLim[2] - 1000, 1000), 0, seq(1000, xLim[2] - 1000, 1000), yLim[2], col = 'gray', lwd = 0.5)
                segments(0, seq(1000, yLim[2] - 1000, 1000), xLim[2], seq(1000, yLim[2] - 1000, 1000), col = 'gray', lwd = 0.5)
                segments(0, 0, xLim[2], 0)
                segments(0, 0, 0, yLim[2])
                segments(xLim[2], 0, xLim[2], yLim[2])
                segments(0, yLim[2], xLim[2], yLim[2])

                # nb stems and reg cover
                if(i == 'dbh')
                {
                    for(subplot in reg_data[plot_id == Plot, unique(subplot_id)])
                    {
                        # nb stems
                        nbstems <- reg_data[plot_id == Plot & subplot_id == subplot & year == yr, sum(nbStems)/nbStems_max * 1000]

                        # rec position
                        xleft <- as.numeric(substring(subplot, 1, 1)) * 1000
                        xright <- xleft + nbstems
                        ybottom <- as.numeric(substring(subplot, 2, 2)) * 1000 + 10
                        ytop <- ybottom + 15
                        rect(xleft, ybottom, xright, ytop, border = NA, col = '#9D7660')

                        # reg cover
                        regcov <- reg_data[plot_id == Plot & subplot_id == subplot & year == yr, sum(regCover)]
                        
                        # rec position
                        xright <- xleft + regcov
                        ybottom <- ytop + 5
                        ytop <- ybottom + 15
                        rect(xleft, ybottom, xright, ytop, border = NA, col = '#f25700')
                    }

                    # legend
                    segments(0, -20, 0, -100, lwd = 0.4); segments(1000, -20, 1000, -100, lwd = 0.4); segments(0, -60, 1000, -60, lwd = 0.4)
                    text(0, -160, '0', cex = 0.6, col = '#f25700'); text(1000, -160, '100%', cex = 0.6, col = '#f25700'); text(500, -150, '% reg cover', cex = 0.7, col = '#f25700')
                    text(0, -260, '0', cex = 0.6, col = '#9D7660'); text(1000, -260, nbStems_max, cex = 0.6, col = '#9D7660'); text(500, -250, '# max of stems', cex = 0.7, col = '#9D7660')
                }
                text(200, yLim[2] + 80, yr)

                # add tree max crown area
                if(i == 'crownArea')
                    toplot[, polygon(x = maxRadius * 100 * cos(theta) + cumX, y = maxRadius * 100 * sin(theta) + cumY, border = colStateTrans, col = colStateTrans), by = rowpos]

                # add trees
                toplot[, polygon(x = dbh/10/2 * cos(theta) + cumX, y = dbh/10/2 * sin(theta) + cumY, border = colState, col = colState), by = rowpos]

                dev.off()

                # progress
                cat('   Ploting ', round(count/totalCount * 100, 2), '%\r'); count <- count + 1
            }
        }
    }

    # Convert png in gif
    for(Plot in tree_data[, unique(plot_id)])
    {
        for(i in c('dbh', 'crownArea'))
        {
            # convert in gif
            system(paste0('convert -delay 100 -loop 1 plots/plot', Plot, '*_', i, '.png plots/plot_', Plot, i, '.gif'))
            # delete png files
            system(paste0('rm plots/plot', Plot, '*_', i, '.png'))
        }
        cat('   Converting', which(Plot == tree_data[, unique(plot_id)]), 'of', tree_data[, length(unique(plot_id))], '\r')
    }
}





##########################################################################################
# Calculate a competition index based on neighbor trees
##########################################################################################

neighborBA <- function(tree_id, cumX, cumY, maxRadius, indBA)
{
    n <- length(unique(tree_id))
    # loop over all tree_ids
    neighborsBA <- numeric(n)
    for(i in 1:n)
    {
        # which ones are inside the max radius of target tree i?
        neighborTrees <- (cumX - cumX[i])^2 + (cumY - cumY[i])^2 < (maxRadius[i] * 100)^2
        # remove the target tree from the list
        neighborTrees[i] <- FALSE
        # sum basal area of neightbor trees
        neighborsBA[i] <- sum(indBA[neighborTrees])
    }
    return( neighborsBA )
}

# Calculating for and considering alive individuals only
tree_data[state2 == 'alive', neighborBA := neighborBA(tree_id, cumX, cumY, maxRadius, indBA), by = .(plot_id, year)]





##########################################################################################
# Calculate subplot basal area
##########################################################################################

# subplot (stand) basal area
# calculate subplot basal area (BA in m2/ha)
tree_data[state2 == 'alive', BA := sum(indBA) * 1e4/100, by = .(plot_id, subplot_id, year)]

# fill NAs of BA (due to dead trees) with the value from the plot
tree_data[, BA := nafill(BA, "locf"), by = .(plot_id, subplot_id, year)]
tree_data[, BA := nafill(BA, "nocb"), by = .(plot_id, subplot_id, year)]

# species basal area per plot (BA_sp) as a proxy of seed source
tree_data[state2 == 'alive', BA_sp := sum(indBA) * 1e4/100, by = .(plot_id, subplot_id, year, spCode)]

# fill NAs the same as for BA
tree_data[, BA_sp := nafill(BA_sp, "locf"), by = .(plot_id, subplot_id, year, spCode)]
tree_data[, BA_sp := nafill(BA_sp, "nocb"), by = .(plot_id, subplot_id, year, spCode)]

# Species relative basal area to overcome the potential tradeoff between the response of
# regeneration to BA (i.e. competition) and BA_sp (i.e. seed source)
tree_data[, relativeBA_sp := BA_sp/BA, by = .(plot_id, subplot_id, year, spCode)]
# 0/0 = NA
tree_data[is.na(relativeBA_sp), relativeBA_sp := 0]
