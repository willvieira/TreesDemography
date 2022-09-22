#------------------------------------------------------
# Ingrowth or number of individuals ingressing the population
# Will Vieira
# August 6, 2022
# Last edited: September 19, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Prepare seedling data (FIA table)
#   - Create sapling data (dbh > 2.5 & < 12.7 cm)
#   - Ingrowth to the 12.7 cm population
#------------------------------------------------------



library(data.table)



#------------------------------------------------------
#------------------------------------------------------

# Load Seedling data from FIA

#------------------------------------------------------
#------------------------------------------------------

# Seedlings count in microplot inside each of the four subplots
# Area of a microplot: 13.495 m2

# TREECOUNT: Number of seedlings inside microplot
# Seedling is defined as all individuals with DBH < 2.54 cm
# And hight > 15 cm of conifers and hight > 30 cm for hardwood

# load SURVEY TABLE
seedling_tb <- fread(
    cmd = "awk 'NR==1||FNR!=1' rawData/FIA/csv/*_SEEDLING.csv",
    header = T
)

# keep only data from 1-4 subplots
seedling_tb <- seedling_tb[SUBP <= 4]

# Only 3% of the dataset has empty count of seedlings due to the first two 
# inventories, seedlings where counted for up to 6 individuals (TREECOUNT_CALC)
seedling_tb[, sum(is.na(TREECOUNT))/.N * 100]

# Feed TREECOUNT with info from TREECOUNT classes
# Expect the class 6, which represents 6+ seedlings
seedling_tb[
    is.na(TREECOUNT) & TREECOUNT_CALC < 6,
    TREECOUNT := TREECOUNT_CALC  
]

# For the remaining NA TREECOUNT observations (0.99%) in which TREECOUNT_CALC 
# is iqual to 6+, I will generate random values fallowing the TREECOUNT
# distribution
probs <- seedling_tb[
    TREECOUNT >= 6,
    hist(TREECOUNT, breaks = seq(6, max(TREECOUNT), 1))$density
]

seedling_tb[
    is.na(TREECOUNT),
    TREECOUNT := sample(
        6:(length(probs) + 5),
        .N,
        prob = probs,
        replace = TRUE
    )
]


# Create base plot, subplot, and species columns to match adult db
# plot_id
seedling_tb[,
    plot_id := paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = '.')
]
seedling_tb[,
    subplot_id := SUBP
]

# year
seedling_tb[,
    year_measured := INVYR
] 

# species_id
spRef <- fread(file.path('data', 'FIA', 'sp_ref.csv'))
spRef_fix <- fread(file.path('rawData', 'FIA', 'FIA_spFixCode.csv'))

# fix us code
seedling_tb[
    spRef_fix,
    SPCD := i.new_code,
    on = c(SPCD = 'us_code')
]

seedling_tb[
    spRef,
    species_id := i.spIds,
    on = 'SPCD'
]

# Only 318 obs (0.01%) still have no species_id (ignored)
seedling_tb[, sum(is.na(species_id))/.N * 100]


# seedling were counted by microplot AND subplot condition
# Here I will ignore the condition and sum the count by
# subplot, year, and species
seedling_dt <- seedling_tb[,
    .(nbSeedling = sum(TREECOUNT)),
    by = .(plot_id, subplot_id, year_measured, species_id)
]






#------------------------------------------------------
#------------------------------------------------------

# Sapling 

#------------------------------------------------------
#------------------------------------------------------


# Source clean data script ignoring filter to remove small indvs
#------------------------------------------------------

# load script
cleanR <- readLines(file.path('R', 'FIA', '05_cleanData.R'))

# remove the filter to keep only adults from subplot
cleanR[grep('dbh >= 127', cleanR)] <- ''

# source file
temp <- tempfile()
writeLines(cleanR, temp)
source(temp)



# Sapling dt
sapling <- copy(treeData[dbh < 127])

# sum number of saplings by plot, subplot, year and species
sapling_dt <- sapling[
    status == 1,
    .(nbSapling = .N),
    by = .(plot_id, subplot_id, year_measured, species_id)
]




#------------------------------------------------------
#------------------------------------------------------

# Ingrowth

#------------------------------------------------------
#------------------------------------------------------


# Remove plot_id with only one year_measured
#------------------------------------------------------

treeData[,
    nbYear_measured := length(unique(year_measured)),
    by = .(plot_id, subplot_id)
]
treeData <- treeData[nbYear_measured > 1]

# set dt ordered by year
setorderv(treeData, cols = "year_measured", order = 1)

# function to define if measurement is a recruit or not
getRecruitment <- function(year_measured, tree_id, status, dbh)
{
    # get nb years and unique
    uqYear = unique(year_measured)
    nbYear = length(uqYear)

    # vector for nb recruitment (values are for plot & year_measured level)
    nbRecruit <- rep(0, sum(uqYear[1] == year_measured))
    # vector specifying if is a recruitment for each measure
    isRecruit <- rep(FALSE, sum(uqYear[1] == year_measured))

    for(year in 1:(nbYear -1))
    {
        # which tree_ids are recruit for this year?
        newRecruit <- !(tree_id[which(uqYear[year + 1] == year_measured)] %in%
                    tree_id[which(year_measured %in% uqYear[1:year])])

        # verify if tree is not dead
        newRecruit[status[which(uqYear[year + 1] == year_measured)] == 2] = FALSE

        # verify if tree_id is bellow 127 dbh threshold
        newRecruit[dbh[which(uqYear[year + 1] == year_measured)] < 127] = FALSE

        isRecruit <- append(isRecruit, newRecruit)

        # how many per year?
        nbRecruit <- append(nbRecruit, rep(sum(newRecruit)/(uqYear[year + 1] - uqYear[year]), sum(uqYear[year + 1] == year_measured)))
    }

    return( list(isRecruit, nbRecruit) )
}

treeData[,
    c('isRecruit', 'nbRecruit') := getRecruitment(
        year_measured, tree_id, status, dbh
    ),
    by = .(plot_id, subplot_id)
]



# function to compute deltaYear for plot based measures
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

treeData[, deltaYear_plot := get_deltaYear(year_measured), by = plot_id]


# recalculate basal area for adults only (exclude recruits)
# calculate plot basal area (BA in m2/ha)
treeData[
    status == 1 & isRecruit == FALSE & dbh >= 127,
    BA_adult := sum(indBA) * 1e4/subPlot_size,
    by = .(plot_id, subplot_id, year_measured)
]

# fill NAs of BA (due to dead trees) with the value from the plot; NAs will
# still persist in plots where all individuals are dead in a specific year
treeData[,
    BA_adult := nafill(nafill(BA_adult, "locf"), "nocb"),
    by = .(plot_id, subplot_id, year_measured)
]

# For persistent NA where all individuals of the plot are dead in a year)
# That means that there are not competing individuals, so BA is iqual 0
treeData[, BA_adult := nafill(BA_adult, fill = 0)]

# species basal area per plot (BA_sp) as a proxy of seed source
treeData[
    status == 1 & isRecruit == FALSE & dbh >= 127,
    BA_adult_sp := sum(indBA) * 1e4/subPlot_size,
    by = .(plot_id, subplot_id, year_measured, species_id)
]

# fill NAs the same as for BA
treeData[,
    BA_adult_sp := nafill(nafill(BA_adult_sp, "locf"), "nocb"),
    by = .(plot_id, subplot_id, year_measured, species_id)
]
treeData[, BA_adult_sp := nafill(BA_adult_sp, fill = 0)]

# Species relative basal area to overcome the potential opposite response of
# regeneration in function of BA (i.e. competition) and BA_adult_sp (i.e. seed source)
treeData[, 
    relativeBA_adult_sp := BA_adult_sp/BA_adult,
    by = .(plot_id, subplot_id, year_measured, species_id)
]
# 0/0 = NA
treeData[is.na(relativeBA_adult_sp), relativeBA_adult_sp := 0]



# group by plot, year, and species_id
fec_dt = treeData[,
    .(
        nbRecruit = sum(isRecruit),
        deltaYear_plot = unique(deltaYear_plot),
        plot_size = unique(subPlot_size),
        latitude = unique(latitude),
        longitude = unique(longitude),
        bio_01_mean = unique(bio_01_mean),
        bio_12_mean = unique(bio_12_mean),
        bio_01_sd = unique(bio_01_sd),
        bio_12_sd = unique(bio_12_sd),
        climate_cellID = unique(climate_cellID),
        s_star = unique(s_star),
        BA_plot = unique(BA_plot),
        BA_adult = unique(BA_adult),
        BA_adult_sp = unique(BA_adult_sp),
        relativeBA_adult_sp = unique(relativeBA_adult_sp)
    ),
    by = .(plot_id, subplot_id, year_measured, species_id)
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
    by = .(plot_id, subplot_id)
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
        species_id,
        vars = list(
            BA_adult_sp,
            relativeBA_adult_sp
        )
    ),
    by = .(plot_id, subplot_id)
]


# drop first year of measurement as now table is a transition way
fec_dt = fec_dt[!is.na(deltaYear_plot), ]


# Add missing recruitment for when adults were present in the last year
# but no recruitment was found for that species (ungly code...)
# This was needed for 0.9% of all plot/subplot/year combinations

# First ignore last year of measurement (not used as source of adults)
treeData[,
    yearsToKeep :=
        year_measured != unique(year_measured)[length(unique(year_measured))],
    by = plot_id
]

uqSpecies_plotYear <- treeData[
    status == 1 & dbh >= 127 & yearsToKeep == TRUE,
    .(spIds = unique(species_id)),
    by = .(plot_id, subplot_id, year_measured)
]

uqPlotYear <- treeData[
    status == 1 & dbh >= 127 & yearsToKeep == TRUE,
    length(unique(species_id)),
    by = .(plot_id, subplot_id, year_measured)
]

# Loop over rows to add missing species and
# count the number of times I had to incluse a empty row in the fec_dt
count = 0; countMissing = 0
for(i in 1:nrow(uqPlotYear))
{
    # Get adult species
    adults_year0 <- uqSpecies_plotYear[
        plot_id == uqPlotYear$plot_id[i] &
        subplot_id == uqPlotYear$subplot_id[i] &
        year_measured == uqPlotYear$year_measured[i],
        spIds
    ]

    # get recruit evenement for the obs t + 1
    rec_year1 <- fec_dt[
        plot_id == uqPlotYear$plot_id[i] &
        subplot_id == uqPlotYear$subplot_id[i] &
        year0 == uqPlotYear$year_measured[i],
        species_id
    ]

    # is there any missing species?         
    missingSps <- setdiff(adults_year0, rec_year1)

    # if so, add the info needed for the species and append row
    if(length(missingSps) > 0) {
        rowRef <- fec_dt[
            plot_id == uqPlotYear$plot_id[i] &
            subplot_id == uqPlotYear$subplot_id[i] &
            year0 == uqPlotYear$year_measured[i]
        ][1, ]
        for(sp in missingSps)
        {
            # add sp info
            rowRef[, species_id := sp]
            rowRef[, nbRecruit := 0]
            rowRef[, 
                c('BA_adult_sp', 'relativeBA_adult_sp') :=
                    treeData[
                        plot_id == uqPlotYear$plot_id[i] &
                        subplot_id == uqPlotYear$subplot_id[i] &
                        year_measured == uqPlotYear$year_measured[i] &
                        species_id == sp,
                        .(BA_adult_sp, relativeBA_adult_sp)
                    ][1, ]
            ]
            
            # append
            fec_dt <- rbind(
                fec_dt,
                rowRef
            )
            
            countMissing = countMissing + 1
        }
    }
    cat('   Progress ', round(i/nrow(uqPlotYear) * 100, 1), '%\r')
    count = count + 1
}




#------------------------------------------------------
#------------------------------------------------------

# Merge all together

#------------------------------------------------------
#------------------------------------------------------

# seedling
fec_dt[
    seedling_dt,
    nbSeedling := i.nbSeedling,
    on = c(
        plot_id = 'plot_id',
        subplot_id = 'subplot_id',
        year0 = 'year_measured',
        species_id = 'species_id'
    )
]

# fill NA
fec_dt[, nbSeedling := nafill(nbSeedling, fill = 0)]


# sapling
fec_dt[
    sapling_dt,
    nbSapling := i.nbSapling,
    on = c(
        plot_id = 'plot_id',
        subplot_id = 'subplot_id',
        year0 = 'year_measured',
        species_id = 'species_id'
    )
]

# fill NA
fec_dt[, nbSapling := nafill(nbSapling, fill = 0)]


# save
saveRDS(fec_dt, file.path('data', 'FIA', 'fec_dt.RDS'))
