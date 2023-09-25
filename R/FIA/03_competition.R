#------------------------------------------------------
# Calculate competition variables
# Will Vieira
# July 11, 2022
#------------------------------------------------------



#------------------------------------------------------
# Steps
#   - Adjust columns units
#   - Load functions and parameters allometries from Purves 2007
#   - Fill allometric parameters for species with missing parameters
#   - Estimate height
#   - Calculate S* and canopyDistance
#   - Calculate plot and species basal area
#------------------------------------------------------




library(data.table); setDTthreads(15)

treeData <- readRDS('data/FIA/tree_tb.RDS')



#------------------------------------------------------
#------------------------------------------------------

# Adjust columns units

#------------------------------------------------------
#------------------------------------------------------

# inch to millimeter
treeData[, dbh := dbh * 25.4] 

# feet to meter
treeData[, total_height := total_height * 0.3048]
treeData[, actual_height := actual_height * 0.3048]



#------------------------------------------------------
#------------------------------------------------------

# Load functions and parameters allometries from Purves 2007 (Amael's GH repo)

#------------------------------------------------------
#------------------------------------------------------

# source parameters and functions from Amaël's GitHub repo
devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/createData/parametersAllometries.R')
devtools::source_url('https://raw.githubusercontent.com/amael-ls/code_R0niche/master/toolFunctions.R')

# get parametrised species
tmp <- tempfile()
download.file('https://github.com/amael-ls/code_R0niche/raw/master/createData/ls_speciesParametrised.rds', tmp)
parametrisedSpecies <- setDT(readRDS(tmp))

# edit species name to match and remove unused columns
parametrisedSpecies <- parametrisedSpecies[, species_id := gsub('-', '', species_id)][, species_id]
purves2007_allometries[, species := gsub('-', '', species)]
purves2007_allometries[, sp := NULL]



#------------------------------------------------------
#------------------------------------------------------

# Fill allometric parameters for species with missing parameters
# This represents 5.3% of total observations

#------------------------------------------------------
#------------------------------------------------------

# First fill parameters with the mean of the functional group
purves2007_allometries[
  treeData,
  sp_group := i.species_group,
  on = c(species = 'species_id')
]

purves2007_allometries_SpGroup <- purves2007_allometries[,
    .(
      a = mean(a, na.rm = TRUE),
      b = mean(b, na.rm = TRUE),
      T = mean(T, na.rm = TRUE)
    ),
    by = sp_group
]

# fill main parameter table with mean from functional group parameters
# Save the species_id in which group does not have parameters mean
sp_noGroup <- c()
for(sp in setdiff(unique(treeData$species_id), parametrisedSpecies))
{
  spGroup <- treeData[species_id == sp, unique(species_group)]
  
  if(spGroup %in% purves2007_allometries_SpGroup$sp_group) {
    dt_toAdd <- purves2007_allometries_SpGroup[sp_group == spGroup]
    dt_toAdd$species = sp
    
    purves2007_allometries <- rbind(
      purves2007_allometries,
      dt_toAdd
    )
  }else{
    sp_noGroup <- append(sp_noGroup, sp)
  }
}

# 331 observations (0.01%) still missing parameters
treeData[species_id %in% sp_noGroup, .N]/nrow(treeData) * 100


# fill by fuzzy matching with species name and TSN code
missingSp <- data.frame(
  missSp = sp_noGroup,
  closeSp = NA,
  maxDist = NA
)
for(i in 1:nrow(missingSp))
{
  Max_dist <- seq(0.1, 0.9, 0.001)
  test = 1
  
  while(test <= length(Max_dist))
  {
    out <- agrep(
      missingSp$missSp[i],
      purves2007_allometries$species,
      max.distance = Max_dist[test],
      value = TRUE
    )
    
    if(length(out) > 0)
    {
      missingSp[i, 'maxDist'] <- Max_dist[test]
      missingSp[i, 'closeSp'] <- out[1] # if more than one, get the first one only
      break

    }else{
      test <- test + 1
    }
  }
}

# filter those species with a closer taxonomic name
# Using a maximum accepted distance between matching names of .3
missingSp <- subset(missingSp, maxDist < 0.3)

for(i in 1:nrow(missingSp))
{
  parsCloseSp <- purves2007_allometries[
    species == missingSp$closeSp[i],
    c('a', 'b', 'T'),
    with = FALSE
  ]

  purves2007_allometries <- rbind(
    purves2007_allometries,
    as.data.table(
      c(
        species = missingSp$missSp[i],
        parsCloseSp,
        sp_group = NA
      )
    )
  )
}

# 133 observations still missing parameters
# Fill the remaining observations with the mean between all species
treeData[!species_id %in% purves2007_allometries$species, .N]

purves2007_allometries <- rbind(
  purves2007_allometries, 
  as.data.table(
    list(
      species = setdiff(
      treeData[, unique(species_id)],
      purves2007_allometries$species
      ),
      a = purves2007_allometries[, mean(a)],
      b = purves2007_allometries[, mean(b)],
      T = purves2007_allometries[, mean(T)],
      sp_group = NA
    )
  )
)

# two species had T parameter missing, fill with mean from species group
# if no species group, fill with mean from all species
purves2007_allometries[
  is.na(T) & !is.na(sp_group),
  T := purves2007_allometries_SpGroup[sp_group == 23, T]
]
purves2007_allometries[
  is.na(T),
  T := purves2007_allometries[, mean(T, na.rm = TRUE)]
]


# add parameters to main data tree 
treeData[
  purves2007_allometries,
  `:=`(
    a = i.a,
    b = i.b,
    T = i.T
  ),
  on = c(species_id = 'species')
]

# remove downloaded file
file.remove('purves2007_allometries.rds')





#------------------------------------------------------
#------------------------------------------------------

# Estimate height

#------------------------------------------------------
#------------------------------------------------------

treeData[,
  est_height := dbhToHeight(
      dbh,
      a,
      b,
      mm = TRUE
  )
]

# I will use the variable `actual_height` as the most accurate estimation of an
# individual height.
# If NA, I will then use the variable `total_height`, but happened for only 397 observations (0.12%)
# Finally, I will use `est_height` created in the section above. 13% of observations will use this variable to calculate S*

# get most accurate estimation
treeData[, used_height := actual_height]

# next
treeData[is.na(used_height), used_height := total_height]

# next
treeData[is.na(used_height), used_height := est_height]




#------------------------------------------------------
#------------------------------------------------------

# Calculate S* and canopyDistance

#------------------------------------------------------
#------------------------------------------------------


# get plot and year combination to calculate S*
treeData[, subPlot_year := paste0(plot_id, subplot_id, year_measured)]


# function to calculate competition (s*)
canopyHeight_mid = function(
  height, supBound, sp_code, plotArea,
  C0_C1, a, b, T,
  plot_year,
  tolSize = 0.01
){

  cat('Plot-year', plot_year, '\r')

  supBound = max(supBound)
  infBound = 0
  s_star = 0
  sumArea = 0

  beyondSupBound_ind = supBound < height # Exclude trees taller than supBound (useful for 2nd, 3rd, ... layer computation)

  while (supBound - infBound > tolSize)
  {

    s_star = (supBound + infBound)/2
    overstorey_ind = s_star <= height
    indices = overstorey_ind & !beyondSupBound_ind
    
    if (!any(indices))
    {
      supBound = s_star
      next;
    }

    distToTop = height[indices] - s_star

    sumArea = sum(
      heightToCrownArea(
        height[indices],
        distToTop,
        a[indices],
        b[indices],
        T[indices],
        C0_C1
      )
    )
    
    if (sumArea < plotArea)
      supBound = s_star
    if (sumArea >= plotArea)
      infBound = s_star

  }
  return (list(s_star, sumArea))
}



# Filter to calculate s* for a plot/subplot/year event:
# Alive individuals only (status == 1)
# Adults sampled from the whole subplot (dbh > 127)
# Remove the 15 individuals that are alive but have NA dbh
treeData[
  status == 1 &
  dbh > 127 &
  !is.na(dbh),
  c('s_star', 'sumArea') := canopyHeight_mid(
    used_height,
    max(used_height),
    species_id,
    plotArea = unique(subPlot_size),
    C0_C1, a, b, T,
    subPlot_year
  ),
  by = subPlot_year
]

# fill NA to those inviduals that didn't pass last filter
treeData[, s_star := unique(s_star[!is.na(s_star)]), by = subPlot_year]


# check if all plot_year have one unique value of s_star
if(!all(treeData[, length(unique(s_star)) == 1, by = subPlot_year]$V1)) stop('Some plot_year have more than one s_star value')

# Calculate canopyDistance
treeData[, canopyDistance := (used_height - s_star)]

# Remove provisory columns
treeData[, c('a', 'b', 'T', 'used_height', 'subPlot_year') := NULL]






#------------------------------------------------------
#------------------------------------------------------

# Calculate plot and species basal area per year

#------------------------------------------------------
#------------------------------------------------------

# some fixes
treesToRemove <- treeData[status == 1 & is.na(dbh), tree_id] # Remove alive trees without dbh
treeData <- treeData[!tree_id %in% treesToRemove]


# Individual BA
treeData[
  status == 1,
  indBA := pi * (dbh/(2 * 1000))^2
]

# calculate plot basal area (BA in m2/ha)
treeData[
  status == 1,
  BA_plot := sum(indBA) * 1e4/subPlot_size,
  by = .(plot_id, subplot_id, year_measured)
]

# fill NAs of BA (due to dead trees) with the value from the plot
treeData[,
  BA_plot := nafill(nafill(BA_plot, "locf"), "nocb"),
  by = .(plot_id, subplot_id, year_measured)
]

# species basal area per plot (BA_sp) as a proxy of seed source
treeData[
  status == 1,
  BA_sp := sum(indBA) * 1e4/subPlot_size,
  by = .(plot_id, subplot_id, year_measured, species_id)
]

# fill NAs the same as for BA
treeData[,
  BA_sp := nafill(nafill(BA_sp, "locf"), "nocb"),
  by = .(plot_id, subplot_id, year_measured, species_id)
]

# Interspecific BA
treeData[,
  BA_inter := BA_plot - BA_sp
]

# Species relative basal area to overcome the potential opposite response of
# regeneration in function of BA (i.e. competition) and BA_sp (i.e. seed source)
treeData[,
  relativeBA_sp := BA_sp/BA_plot,
  by = .(plot_id, subplot_id, year_measured, species_id)
]
# 0/0 = NA
treeData[is.na(relativeBA_sp), relativeBA_sp := 0]

# Basal area of larger individuals than the focal individual (competitive index)
BA_comp <- function(size, plotSize, BA_ind) {
    sapply(
      size,
      function(x)
        sum(BA_ind[size > x]) * 1e4/plotSize
    )
}

treeData[
  status == 1,
  BA_comp := BA_comp(dbh, unique(subPlot_size), indBA),
  by = .(plot_id, subplot_id, year_measured)
]

# Individual basal area relative to the plot basal area
treeData[
  status == 1,
  relativeBA_comp := indBA/sum(indBA),
  by = .(plot_id, subplot_id, year_measured)
]


# Basal area of larger individuals than the focal individual
# For intra vs interspecies (competitive index)
BA_comp_spIntra <- function(size, species_id, plotSize, BA_ind) {
    BA_comp_sp <- sapply(
      1:length(size),
      function(x)
        sum(
          BA_ind[species_id %in% species_id[x]][size[species_id %in% species_id[x]] > size[x]]
        ) * 1e4/plotSize
    )
    BA_comp_inter <- sapply(
      1:length(size),
      function(x)
        sum(
          BA_ind[!species_id %in% species_id[x]][size[!species_id %in% species_id[x]] > size[x]]
        ) * 1e4/plotSize
    )
    return( list(BA_comp_sp, BA_comp_inter) )
}

treeData[
  status == 1,
  c('BA_comp_sp', 'BA_comp_inter') := BA_comp_spIntra(dbh, species_id, unique(subPlot_size), indBA),
  by = .(plot_id, subplot_id, year_measured)
]


# save
saveRDS(treeData, 'data/FIA/treeData_sStar.RDS')
