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


# Data description

    # ADULTS are measured in the whole plot (399.7312 m2)

    ## SEEDLING (measured in 4 microplots of 4.01 m2 = 16.05 m2):
     # - Height >= 30 cm
     # - DHP <= 1 cm

    # seedling$CL_HT_SEMI
        # A	Semis de 15 à 60 cm de hauteur
        # B	Semis 60 cm et + jusqu'à un dhp < ou égal à 1 cm
        # C	Semis 30 cm et + jusqu'à un dhp < ou égal à 1 cm
        
    
    ## SAPLING OR GAULES (measured in the subplot of 40.04 m2):
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




################################################################################
# Ingrowth into adult class
# Defined by the number of individuals crossing the 9 cm dbh (or the 12.7 cm)
################################################################################

# Before editing the final_dt obj, save it
saveRDS(final_dt, "data/quebec/treeDataQuebec_beforeTransition.RDS")


# Recruit (all individuals entering the population)

    # Remove plot_id with only one year_measured
    # Number of measures by plot_id
    final_dt[, nbYear_measured := length(unique(year_measured)), by = ID_PE]
    # keep plots with more than one measures (so I can quantify recruitment number)
    final_dt <- final_dt[nbYear_measured > 1]

    # set dt ordered by year
    setorderv(final_dt, cols = "year_measured", order = 1)

    # function to define if measurement is a recruit or not
    getRecruitment <- function(year_measured, tree_id, status)
    {
    # get nb years and unique
        uqYear = unique(year_measured)
        nbYear = length(uqYear)

        # vector for nb recruitment (values are for plot & year_measured level)
        nbRecruit <- rep(0, sum(uqYear[1] == year_measured))
        # vector specifying if it is a recruitment for each measure
        isRecruit <- rep(FALSE, sum(uqYear[1] == year_measured))

        for(year in 1:(nbYear -1))
        {
            # which tree_ids are recruit for this year?
            newRecruit <- !(tree_id[which(uqYear[year + 1] == year_measured)] %in% tree_id[which(year_measured %in% uqYear[1:year])])

            # verify if tree is not dead
            newRecruit[status[which(uqYear[year + 1] == year_measured)] != 'alive'] = FALSE

            isRecruit <- append(isRecruit, newRecruit)

            # how many per year?
            nbRecruit <- append(nbRecruit, rep(sum(newRecruit)/(uqYear[year + 1] - uqYear[year]), sum(uqYear[year + 1] == year_measured)))
        }

        return( list(isRecruit, nbRecruit) )
    }

    final_dt[, c('isRecruit', 'nbRecruit') := getRecruitment(year_measured, tree_id, state), by = ID_PE]


    # The threshold to ingrowth changes in function of the origem of the database (org_db_loc)
    # 9.1 for qc_pp
    # 12.7 for us_pp (highest threshold)
    # For all the other sources which the threshold is inferior to 12.7 cm, define ingrowth as the trees growth through the limit of 12.7 cm

    # get individuals with size inferior and supperior to the threshold
    tree_toConsider <- function(dbhTree, threshold) {
        if(length(dbhTree) == 1)
            return ( FALSE )
        
        if(any(dbhTree >= threshold) & any(dbhTree < threshold)){
            return( TRUE )
        }else{
            return( FALSE )
        }
    }

    final_dt[state == 'alive' & !is.na(DHP), possibleRecruit := tree_toConsider(DHP, 127), by = tree_id]

    getRecruit <- function(dbhTree, threshold)
    {
        rec <- rep(FALSE, length(dbhTree))

        # which is the latest higthest size inferior to the threshold?
        supInf <- max(which(dbhTree < threshold))

        # is there any already adult before this position? If so, no recruit then
        if(supInf > 1)
            if(any(dbhTree[1:(supInf-1)] >= threshold))
            return( rec )

        rec[which(dbhTree >= threshold)[1]] <- TRUE

        return( rec )
    }

    final_dt[possibleRecruit == TRUE, isRecruit2 := getRecruit(DHP, 127), by = tree_id]
    final_dt[is.na(isRecruit2), isRecruit2 := FALSE]

    final_dt[DHP >= 127, isRecruit_127 := isRecruit | isRecruit2]
    final_dt[is.na(isRecruit_127), isRecruit_127 := FALSE]


    # function to compute deltaYear for plot based measures
    # (deltaYear column is for individual information)
    get_deltaYear <- function(years)
    {
        uqYear <- unique(years)

        deltaYear <- rep(NA, sum(uqYear[1] == years))
        for(i in 2:length(uqYear))
        {
            deltaYear <- append(
            deltaYear,
            rep(
                uqYear[i] - uqYear[i-1],
                sum(uqYear[i] == years)
                )
            )
        }
        return( deltaYear )
    }

    final_dt[, deltaYear_plot := get_deltaYear(year_measured), by = ID_PE]


    # recalculate basal area for adults only (exclude recruits)
    # calculate plot basal area (BA in m2/ha)
    final_dt[
        state == 'alive' & isRecruit == FALSE & isRecruit_127 == FALSE,
        BA_adult := sum(indBA) * 1e4/399.7212,
        by = .(year_measured, ID_PE)
    ]

    # fill NAs of BA (due to dead trees) with the value from the plot
    # NAs will still persist in plots where all individuals are dead in a specific year
    final_dt[, 
        BA_adult := nafill(nafill(BA_adult, "locf"), "nocb"),
        by = .(year_measured, ID_PE)
    ]

    # For persistent NA where all individuals of the plot are dead in a year)
    # That means that there are not competing individuals, so BA is iqual 0
    final_dt[, BA_adult := nafill(BA_adult, fill = 0)]

    # species basal area per plot (BA_sp) as a proxy of seed source
    final_dt[
        state == 'alive' & isRecruit == FALSE & isRecruit_127 == FALSE,
        BA_adult_sp := sum(indBA) * 1e4/399.7212,
        by = .(year_measured, ID_PE, sp_code2)
    ]
    
    # fill NAs the same as for BA
    final_dt[,
        BA_adult_sp := nafill(nafill(BA_adult_sp, "locf"), "nocb"),
        by = .(year_measured, ID_PE, sp_code2)
    ]
    final_dt[, BA_adult_sp := nafill(BA_adult_sp, fill = 0)]

    # Species relative basal area to overcome the potential opposite response of
    # regeneration in function of BA (i.e. competition) and BA_adult_sp (i.e. seed source)
    final_dt[, 
        relativeBA_adult_sp := BA_adult_sp/BA_adult,
        by = .(year_measured, ID_PE, sp_code2)]
    # 0/0 = NA
    final_dt[is.na(relativeBA_adult_sp), relativeBA_adult_sp := 0]


    # group by plot, year, and species_id
    fec_dt = final_dt[,
        .(
            nbRecruit = sum(isRecruit),
            nbRecruit_127 = sum(isRecruit_127),
            deltaYear_plot = unique(deltaYear_plot),
            plot_size = 399.7212,
            SHAPE = unique(SHAPE),
            cellID = unique(cellID),
            bio_01_mean = unique(bio_01_mean),
            bio_12_mean = unique(bio_12_mean),
            bio_01_sd = unique(bio_01_sd),
            bio_12_sd = unique(bio_12_sd),
            cellID = unique(cellID),
            s_star = unique(s_star),
            BA_plot = unique(BA_plot),
            BA_adult = unique(BA_adult),
            BA_adult_sp = unique(BA_adult_sp),
            relativeBA_adult_sp = unique(relativeBA_adult_sp)
        ),
        by = .(ID_PE, year_measured, sp_code2)
    ]

    # Transfor data table in a transition form
    # nbRecruit from year0 to year1
    fec_dt[, year1 := year_measured]
    fec_dt[, year0 := year1 - deltaYear_plot]
    fec_dt[, year_measured := NULL]


    # delay plot level variables of one measurement
    # i.e. nbRecruit as a function of last BA_plot, BA_sp, etc
    delayed_plot <- function(year1, vars)
    {
        uqYear <- unique(year1)
        lnYear <- length(uqYear)

        # position of values in function of their year measured
        posL <- list()
        for(year in 1:lnYear)
            posL[year] <- list(uqYear[year] == year1)

        # vector with nMeasure by year to repeate value of year before
        yearRep <- c()
        for(year in 1:lnYear)
            yearRep <- append(yearRep, sum(uqYear[year] == year1))


        # delayed variable values by one year
        outList <- list()
        # -2 because two last variables are sp dependent (another loop after this one)
        for(var in 1:length(vars))
        {
            uqVar <- c()
            for(year in 1:lnYear)
                uqVar <- append(uqVar, unique((vars[[var]][posL[[year]]])))

            uqVar <- c(NA, uqVar[1:(length(uqVar) - 1)])

            outList[var] <- list(rep(uqVar, yearRep))
        }

        return( outList )
    }

    fec_dt[,
        c(
            's_star',
            'BA_plot',
            'BA_adult'            
        ) := delayed_plot(
            year1,
            vars = list(
                s_star,
                BA_plot,
                BA_adult
            )
        ),
        by = ID_PE
    ]


    # Now get delayed values for BA_adult_sp and relativeBA_adult_sp because
    # these two variables are species dependent
    delayed_BA_sp <- function(year1, species_id, vars)
    {
        uqYear <- unique(year1)
        lnYear <- length(uqYear)

        # position of values in function of their year measured
        posL <- list()
        for(year in 1:lnYear)
        posL[year] <- list(uqYear[year] == year1)

        outList <- list()
        for(var in 1:length(vars)) {

        varLag <- rep(NA, length(year1))
        for(year in 1:(lnYear - 1)) {

            uqSpecies_id <- unique(species_id[posL[[year + 1]]])

            for(sp in uqSpecies_id) {

            # get position where sp is present for current and year + 1
            posSpecies <- species_id[posL[[year + 1]]] == sp
            posSpecies_lag <- species_id[posL[[year]]] == sp

            # Update dealyed info IF species was present in previous inventory, otherwise is 0
            if(any(posSpecies_lag)) {
                # update values in a delayed way
                varLag[posL[[year + 1]]][posSpecies] <- vars[[var]][posL[[year]]][posSpecies_lag]
            }else {
                varLag[posL[[year + 1]]][posSpecies] <- 0
            }
            }
        }
        outList <- append(outList, list(varLag))
        }

        return( outList )
    }

    # get delayed measure of one measurement for each plot_id, year_measured, and species_id
    fec_dt[,
        c(
            'BA_adult_sp',
            'relativeBA_adult_sp'
        ) := delayed_BA_sp(
            year1,
            sp_code2,
            vars = list(
                BA_adult_sp,
                relativeBA_adult_sp
            )
        ),
        by = ID_PE
    ]


    # drop first year of measurement as now table is a transition way
    fec_dt = fec_dt[!is.na(deltaYear_plot), ]

#



# Add missing recruitment for when adults were present in the last year
# but no recruitment was found for that species
# This fix was needed only 19 plot-year times from the 44045 plot-year comb

for(plot_id in fec_dt[, unique(ID_PE)])
{
    # get unique years from specific plot 
    uqYear <- final_dt[ID_PE == plot_id, unique(year_measured)]

    # loop over the years to check if there are alive adult species
    # in `year0` that are not present in `year1`
    for(yr in uqYear[-length(uqYear)])
    {
        # is there any missing species?
        adults_year0 <- final_dt[
            ID_PE == plot_id & year_measured == yr &
            state == 'alive' & DHP > 127,
            unique(sp_code2)
        ]
        rec_year1 <- fec_dt[ID_PE == plot_id & year0 == yr, unique(sp_code2)]
        missingSps <- setdiff(adults_year0, rec_year1)
        
        # if so, add the info needed for the species and append row
        if(length(missingSps) > 0) {
            rowRef <- fec_dt[ID_PE == plot_id & year0 == yr][1, ]
            for(sp in missingSps)
            {
                # add sp info
                rowRef[, sp_code2 := sp]
                rowRef[, c('nbRecruit', 'nbRecruit_127') := 0]
                rowRef[, c('BA_adult_sp', 'relativeBA_adult_sp') := final_dt[ID_PE == plot_id & year_measured == yr & sp_code2 == sp, .(BA_adult_sp, relativeBA_adult_sp)][1, ]]
                
                # append
                fec_dt <- rbind(
                    fec_dt,
                    rowRef
                )
            }
        }
    }
}





################################################################################
# Seedlings
################################################################################


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



# Compute number of seedlings by plot_id - year - species_id

    seedling_dt <- seedling[,
        .(nbSeedling = sum(NB_SEMIS)),
        by = .(ID_PE, year_measured, sp_code)
    ]

#



# Merge with main data

     fec_dt[
        seedling_dt,
        nbSeedling := i.nbSeedling,
        on = c(
            ID_PE = 'ID_PE',
            year0 = 'year_measured',
            sp_code2 = 'sp_code'
        )
    ]

    # fill NA
    fec_dt[, nbSeedling := nafill(nbSeedling, fill = 0)]

#




################################################################################
# Saplings
################################################################################


# wide sapling classes into columns to keep plot-year-species key
    
    sapling_dt = sapling %>%
        select(ID_PE, year_measured, sp_code, CL_DHP, NB_TIGE) %>%
        tidyr::pivot_wider(
            names_from = 'CL_DHP',
            values_from = 'NB_TIGE',
            names_prefix = 's',
            values_fill = 0
        ) %>%
        as.data.table()

#



# merge with main data

     fec_dt[
        sapling_dt,
        `:=`(
            nbSapling_002 = i.s002,
            nbSapling_004 = i.s004,
            nbSapling_006 = i.s006,
            nbSapling_008 = i.s008
        ),
        on = c(
            ID_PE = 'ID_PE',
            year0 = 'year_measured',
            sp_code2 = 'sp_code'
        )
    ]

    # fill NA
    setnafill(
        fec_dt,
        type = 'const',
        fill = 0,
        cols = paste0('nbSapling_00', c(2, 4, 6, 8))
    )

    # total sapling
    fec_dt[,
        nbSapling_total := nbSapling_002 + nbSapling_004 +
            nbSapling_006 + nbSapling_008
    ]

#



# Save all

  if(!dir.exists('data/quebec/')) dir.create('data/quebec')

    saveRDS(tree_data, "data/quebec/tree_data.RDS")
    st_write(plot_xy, "data/quebec/plot_xy32198.gpkg", append = FALSE)
    saveRDS(ecoreg_df, "data/quebec/ecoreg_df.RDS")
    st_write(bound_Qc, "data/quebec/bound_Qc.gpkg")
    saveRDS(env_data, "data/quebec/env_data.RDS")
    saveRDS(plot_climVars, "data/quebec/bio-cmi-pcp.RDS")
    saveRDS(growth_dt, "data/quebec/growth_transition_dt.RDS")
    saveRDS(mort_dt, "data/quebec/mort_transition_dt.RDS")
    saveRDS(fec_dt, "data/quebec/fec_dt.RDS")
    saveRDS(final_dt[isRecruit_127 == TRUE], "data/quebec/sizeIngrowth_dt.RDS")

#
