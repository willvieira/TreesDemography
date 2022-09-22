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

### Climate
  - Stack over years for each variable (bio, cmi)
  - Extract climate data for plot locations
  - Fill NA plots with adjacent information
  - Fix climate units
  - Rolling average of 5 years

### Clean data
  - Remove individuals from micro plot
  - Keep only alive and dead status
  - Fix resurrected
  - Remove already dead and multiple dead events
  - Fix indivudals with multiple species_id
  - Remove plots with missing climate

### Transition
  - Calculate growth (dbh1 - dbh0), deltaYear (year1 - year0), and state (state after deltaYear)
  - Transform db in a transition way
  - Remove extreme growth rate (0 < growth < 35 mm/year)
  - Keep natural dead events for mortality
  - remove already indivuduals

### Ingrowth
  - Seedlings:
    - Fix species_id
    - Fix count of seedlings for initial inventories where classes were used
    - For a particular class of 6+, genarate seedling count from the population distribution (grouped species)
    - Count number of seedling per plot/subplot/year/species
  - Saplings:
    - Count number of alive individuals with with dbh < 127 mm
  - Ingrowth:
    - Tag individuals that cross the 127 mm dbh threshold as ingrowth
    - Recalculate all BA indices considering only adults (dbh > 127 mm) and no recruits (ingrowth)
    - Group data by plot/subplot/year/species and transform db in a transition
    - Adjust individual and plot variables for year0 reference
  - Merge seedlings and saplings db to the ingrowth
