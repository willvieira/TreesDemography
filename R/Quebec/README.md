# Clean and organize Quebec data

This is a modified version of the original code from Marie-Hélène Brice ([link](https://github.com/mhBrice/Quebec_data)) with some functions from Amaël Lesquin.


# Setup

To run all the scripts just open `R` and run:

```r
scripts <- paste0('R/Quebec/', dir('R/Quebec/', pattern = '.R'))
lapply(as.list(scripts), source)
```

## Steps

Each of the following steps is a `R` script :

### Clean tree data
  - Download gdb file and extract useful layers
  - Keep only necessary columns
  - change species code
  - Merge with plot info
  - remove some states (25 (intru), 44, 45, 46 (dead recruit), 34, 35, 36 (dead forgotten))
  - Correct tree ids
  - reclassify some states
  - Simplify state to alive, dead, unknown
  - check if some PE MES were deleted by mistake
  - join xy
  - calculate basal area

### Assign plot to an ecoregion
  - download ecoregions
  - reclassify ecoregions
  - assign ecoregions for plots
  - get quebec boundary

### Get non climatic environmental variables
  - load variables from db
  - reclassify variables

### Extract bioclimatic variables at 2km2 resolution
  - download bioclimatic data
  - check if climate ref are the same
  - stack over years for each variable (bio, cmi, pcp)
  - Extract climate data for plot locations
  - Fix climate NA using adjancent cells
  - Correct for temperature variables
  - Rolling avarage climate variables for the last 5 and 10 years
  - Merge all BIOCLIM, CMI, and PCP variables into one data.frame

### Calculate plot, species and individual variables
  - Merge all data from above into one single data.table object
  - Calculate individual height using Purves et al. 2008 allometries
  - Calculate competition s_star, canopyStatus and canopyDistance
  - Calculate plot and species basal area

### Prepare data in a transition way
  - Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0), and state (state after deltaYear)
  - Transform db in a transition way
  - Separate between growth and mortality df
  - Filter growth_dt to nbMeasure > 1
  - Remove extreme growth rate (0 < growth < 35 mm/year)
  - Filter for natual dead only
  - Remove first dead measurement for mortality
  - Create a mort column [0 - 1]
