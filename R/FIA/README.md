# Clean and organize FIA data


# Setup

To run all the scripts just open `R` and run:

```r
scripts <- dir('R/FIA/', pattern = '.R', full.names = TRUE)
sapply(scripts, source)
```

## Steps

Each of the following steps is a `R` script :

### Download data
  - Define US states we will be using
  - Download db zip files
  - Unzip and extract tables:
      - `TREE`, `PLOT`, `COND`, `SUBPLOT`, `SUBP_COND`, and `SURVEY` 
  - download species referene name

### Merge files
  - Load db tables while merging all states into single obj
  - Merge plot, condition and subplot condition into one obj
  - Filter plot/survey to have permanent plots (remeasured) and assure all states follow the same protocol
  - load tree data for plots of interest
  - Assign plot size, tree_id, and species name
  - rename columns

### Competition
  - Adjust columns units
  - Load functions and parameters allometries from Purves 2007
  - Fill allometric parameters for species with missing parameters
  - Estimate height
  - Calculate S* and canopyDistance
  - Calculate plot and species basal area

### 