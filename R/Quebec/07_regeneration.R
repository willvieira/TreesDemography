##############################
# Organize seedling and sapling data
# May 29, 2020
##############################


##############################
# Steps
#   - Get year of measurement instead of NO_MES
#   - Update species_id code and get year of measurement instead of NO_MES
#   - Keep only plots for seedling and sapling that are also present in the final_dt
#
#  Seedlings
#   - Convert NB_SEMIS NA to 1
#   - Remove first measure of plot
#   - Fill absence measures
#   - Calculate NB_SEMIS by plot - year and species_id
##############################

# TODO Temporary start

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(data.table))
spCodeFile <- "rawData/ref_spCode.csv"
sps_code <- read.csv2(spCodeFile)
final_dt <- readRDS('data/quebec/treeDataQuebec_all.RDS')

#


# Data description

    # Seedlings:
     # - Height >= 30 cm
     # - DHP <= 1 cm

    # seedling$CL_HT_SEMI
        # A	Semis de 15 à 60 cm de hauteur
        # B	Semis 60 cm et + jusqu'à un dhp < ou égal à 1 cm
        # C	Semis 30 cm et + jusqu'à un dhp < ou égal à 1 cm
        
    
    # Sapling:
     # - DHP > 1 cm <= 90 cm
    
    # sapling$CL_DHP
        # 002 DHP se situe entre 1,1 cm et 3,0 cm 
        # 004 DHP se situe entre 3,1 cm et 5,0 cm
        # 006 DHP se situe entre 5,1 cm et 7,0 cm
        # 008 DHP se situe entre 7,1 cm et 9,0 cm
    # sapling$NB_TIGE
        # number of stems
    # sapling$TIGE_HA
        # number of stems per hectare (stems/ha)
    # sapling$ST_HA
        # Basal area per hectare (m²/ha)

#



# Load data

    seedling = as.data.table(sf::st_read('rawData//PEP_GDB//PEP.gdb', layer = 'STATION_SEMIS'))
    sapling = as.data.table(sf::st_read('rawData//PEP_GDB//PEP.gdb', layer = 'DENDRO_GAULES'))

    # check NAs
    print('seedling:')
    knitr::kable(sort(apply(seedling, 2, function(x) length(which(complete.cases(x))) / nrow(seedling) * 100)))
    print('sapling')
    knitr::kable(sort(apply(sapling, 2, function(x) length(which(complete.cases(x))) / nrow(sapling) * 100)))

#



# Update species_id code and get year of measurement instead of NO_MES

    # Species_ids
    seedling[, sp_code := sps_code$spCode[match(ESSENCE, sps_code$qc_code)]]
    seedling[, ESSENCE := NULL]
    
    sapling[, sp_code := sps_code$spCode[match(ESSENCE, sps_code$qc_code)]]
    sapling[, ESSENCE := NULL]

    # Keep only species in which there are adults in the tree_data
    seedling <- seedling[sp_code %in% unique(final_dt$sp_code2)]
    sapling <- sapling[sp_code %in% unique(final_dt$sp_code2)]


    # year
    # Plot date of measurements
    plot_mes <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "PLACETTE_MES")
    plot_mes <- plot_mes %>%
    filter(ID_PE %in% seedling$ID_PE) %>%
    mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
    dplyr::select(ID_PE, NO_MES, ID_PE_MES, year_measured)
    plot_mes <- as.data.table(plot_mes)
    
    seedling <- merge(seedling, plot_mes[, -2], by = c('ID_PE', 'ID_PE_MES'))
    sapling <- merge(sapling, plot_mes[, -2], by = c('ID_PE', 'ID_PE_MES'))

#


# Keep only plots for seedling and sapling that are also present in the final_dt

    plotsToKeep <- unique(final_dt$ID_PE)
    
    seedling <- seedling[ID_PE %in% plotsToKeep]
    sapling <- sapling[ID_PE %in% plotsToKeep]

#




##################################################################################
# Seedlings
##################################################################################


# Convert NB_SEMIS NA to 1

    # NB_SEMIs was counted only for comercial species, while non-comercial species had just its height class
    # However, if we look at non-comercial species, there will be a line of height class fo each individual of
    # a specific species into a MICRO_PLACETTE. It means every line with a height class represents at least one
    # individual, and therefore we could assume NB_SEMIS is 1
    # Take as an example the following measures of the PLOT_YEAR == 009910010101
    # seedling[ID_PE_MES == '009910010101', ]
    # We can see that for MICRO_PE 01 and 02 there are two individuals for the same species
    seedling <- data.table::setnafill(seedling, fill = 1, cols = 'NB_SEMIS')

#



# Remove first measure of plot

    # First remove the first measure of the plot as we are going to use the plot_info from the last time step
    # As it is much more realistic to think number of seedlings > 30 cm height were affected by the past variables
    seedling <- seedling[NO_MES != 1]

#



# Fill absence measures

    # In the seedling dt we have only the number of seedlings found in the micro plot
    # But we do not have the absent seedlings (e.g. those species present as an adult in the plot but not as seeding)
    # Check here for species present in the plot in the last measurement (lag) but with no seedling and 
    # Add spicies_id with nbRecruit == 0

    plots <- unique(seedling$ID_PE)
    
    for(plot in plots)
    {
        # Unique years
        years <- plot_mes[ID_PE == plot, sort(unique(year_measured))]

        plotDT <- seedling[0, ]
        for(year in 1:(length(years) - 1))
        {
            basicInfo <- plot_mes[ID_PE == plot & year_measured == years[year + 1], c(1, 3, 2)]

            # Unique species_id-year for adults and seedling
            sp_adult <- final_dt[ID_PE == plot & year0 == years[year], unique(sp_code2)]
            sp_seedling <- seedling[ID_PE == plot & year_measured == years[year + 1], unique(sp_code)]
            spToAdd <- sp_adult[!sp_adult %in% sp_seedling]

            # create dt to insert
            n = length(spToAdd)
            basicInfo <- basicInfo[rep(basicInfo[, .I], length(spToAdd))]
            basicInfo[, `:=`(MICRO_PE = rep(NA, n),
                             NB_SEMIS = rep(0, n),
                             CL_HT_SEMI = rep(NA, n),
                             DENS_SEMI = rep(NA, n),
                             sp_code = spToAdd,
                             year_measured = rep(years[year + 1], n))]

            plotDT <- rbind(plotDT, basicInfo)
               
        }

        # merge with seedling_dt
        seedling <- rbind(seedling, plotDT)

        # progress bar
        cat('     Filling absence measures', floor(which(plot == plots)/length(plots) * 100), '%\r')

    }

#



# Calculate NB_SEMIS by plot - year and species_id

    recruit <- seedling[, sum(NB_SEMIS), by = .(ID_PE, year_measured, sp_code)]
    names(recruit)[4] <- 'nbRecruit'

#



# Get plot info

    # First I will calculate the mean canopyDistance by species_id to have a species
    # specific competition effect other than the Basal area
    final_dt[, meanCanopyDistance := mean(canopyDistance, na.rm = TRUE), by = .(ID_PE, year0, sp_code2)]

    # Here for each plot-year, I get the composition of adult trees from the previous measurement
    # I also get the climate, pertubation and non enviroment variables from the last measurement
    outDT <- final_dt[0, c(1, 74:75, 9, 12:68, 70:72, 85, 78)]
    for(plot in plots)
    {
        # Unique years
        years <- plot_mes[ID_PE == plot, sort(unique(year_measured))]

        for(year in 1:(length(years) - 1))
        {
            # get plot-year information from adult data base
            plotYearDT <- unique(final_dt[ID_PE == plot & year0 == years[year], c(1, 74:75, 9, 12:68, 70:72, 85)], by = 'sp_code2')
            
            # Some years have been removed previously, so check if it is still there, ignore year otherwise
            # So there will be less plot-year information in the output
            if(nrow(plotYearDT) > 0)
            {
                # update to year + 1 to add a lag
                plotYearDT[, year0 := years[year + 1]]

                # check if all recruitment species have its adult in the plot
                sp_adult <- plotYearDT[, unique(sp_code2)]
                sp_recruit <- recruit[ID_PE == plot & year_measured == years[year + 1], unique(sp_code)]

                # if some species are missing, add them with 0 BA
                if(!all(sp_recruit %in% sp_adult))
                {
                    spToAdd <- sp_recruit[!sp_recruit %in% sp_adult]
                    dtToAdd <- plotYearDT[rep(plotYearDT[1, .I], length(spToAdd))]
                    
                    dtToAdd$sp_code2 <- spToAdd
                    dtToAdd[, `:=`(BA_sp = NA, relativeBA_sp = NA, meanCanopyDistance = NA)]
                    
                    plotYearDT <- rbind(plotYearDT, dtToAdd)
                }
                # if there are adult species not present in the recruit dt,
                # break as it should have at least nbRecruit = 0
                if(!all(sp_adult %in% sp_recruit))
                    stop(paste('Plot =', plot, 'and year =', years[year], 'do not have the same species'))

                if(any(is.na(plotYearDT$ID_PE))) stop(paste('Plot =', plot, 'and year =', years[year], 'have NAs'))
                
                cat(names(plotYearDT)[!names(plotYearDT) %in% names(outDT)])
                outDT <- rbind(outDT, plotYearDT)
            }
        }
        # progress bar
        cat('     Getting plot information', floor(which(plot == plots)/length(plots) * 100), '%\r')
    }

    # merge with recruit
    names(outDT)[which(names(outDT) %in% c('year0', 'sp_code2'))] <- c('sp_code', 'year_measured')
    recruit = merge(recruit, outDT, by = c('ID_PE', 'year_measured', 'sp_code'))
    recruit[, sp_code2 := sp_code]
    recruit[, sp_code := NULL]
    
    # remove column
    final_dt[, meanCanopyDistance := NULL]

#



# save seedlings

    saveRDS(recruit, 'data/quebec/fec_dt.RDS')

#




# Exploration

    library(ggplot2)
    library(GGally)
    
    dt = recruit[, setdiff(names(recruit), c('ID_PE', 'year_measured', 'sp_code', 'ecoreg3', 'ORIGINE', 'PERTURB', 'ORIGINE2', 'PERTURB2', 'CL_DRAI')), with = FALSE]
    ggpairs(data = dt)

#



##################################################################################
# Saplings
##################################################################################


# Think about the saplings information
# - 
# -     

