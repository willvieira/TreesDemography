# Dataset preparation

The following scripts involve extracting and organizing data from the `FIA` and `Quebec` forest inventories.
Note that the RESEF dataset network is not used in this study due to its different protocol and lack of openness.
The following steps outline how to reproduce the dataset used in our study:

1. Execute the scripts within the `FIA` and `Quebec` folders, following the provided instructions in those folders. Note that the order of execution between the folders is trivial.

2. After completing the data retrieval and preparation in both the `FIA` and `Quebec` datasets, run the `merge_db.R` script to combine the data from both datasets and generates the final datasets used in our study:

- `treeData.RDS`: This file contains the complete dataset in tidy format, with one observation per row.

From the `treeData.RDS` dataset, four additional datasets are derived, each representing the transition between time $t$ and time $t + \Delta t$:
   
- `growth_dt.RDS`: Contains growth rate information between time $t$ and $t + \Delta t$.
- `status_dt.RDS`: Contains the status (alive or dead) of individuals at both time $t$ and time $t + \Delta t$.
- `fec_dt.RDS`: Contains ingrowth rate (number of individuals entering the population at 127 mm) between time $t$ and $t + \Delta t$.
- `sizeIngrowth_dt.RDS`: Contains the size distribution of all individuals that entered the population for each remeasurements.
