##############################
# Clean tree data
# May 23, 2020
##############################


##############################
# Steps
#   - Download gdb file and extract useful layers
#   - Keep only necessary columns
#   - change species code
#   - Merge with plot info
#   - remove some states (25 (intru), 44, 45, 46 (dead recruit), 34, 35, 36 (dead forgotten))
#   - Correct tree ids
#   - reclassify some states
#   - Simplify state to alive, dead, unknown
#   - check if some PE MES were deleted by mistake
#   - join xy
#   - basal area
##############################



suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(data.table))




# Download db file and extract layers

  if(!dir.exists('rawData')) dir.create('rawData')
  rawData <- 'rawData/PEP_GDB.zip'
  
  # download file
  if(!dir.exists('rawData/PEP_GDB'))
      download.file('ftp://transfert.mffp.gouv.qc.ca/Public/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/PEP_GDB.zip', rawData, method = 'auto', quiet = TRUE)

  # unzip file
  unzip(rawData, exdir = 'rawData')
  file.remove(rawData)

  # Species code
  spCodeFile <- "rawData/ref_spCode.csv"
  if(!file.exists(spCodeFile))
      download.file('https://raw.githubusercontent.com/mhBrice/talk_thesis/master/data/ref_spCode.csv', spCodeFile, method = 'auto', quiet = TRUE)


  # Check the list of layers in the gdb
  rgdal::ogrListLayers("rawData/PEP_GDB/PEP.gdb")

  # Plot coordinates
  plot_xy <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "PLACETTE")

  # Plot date of measurements
  plot_mes <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "PLACETTE_MES")

  # DHP + cote DENDRO_ARBRES
  tree_mes <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "DENDRO_ARBRES")

  # Disturbances
  pep_pe <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "STATION_PE")

  # Species code
  sps_code <- read.csv2(spCodeFile)

#



# Keep only necessary columns 

  # Remove abandonned plots (STATUT_MES %in% c(AB, RE, RL, DE, NT, SR))

  table(plot_mes$STATUT_MES)
  ID_PE_aband <- plot_mes %>% filter(!is.na(STATUT_MES))

  plot_mes <- plot_mes %>%
    filter(!(ID_PE %in% ID_PE_aband$ID_PE)) %>%
    mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
    dplyr::select(ID_PE, ID_PE_MES, year_measured) 

  plot_xy <- plot_xy %>%
    filter(ID_PE %in% plot_mes$ID_PE) %>%
    dplyr::select(ID_PE, SHAPE) 

  tree_mes <- tree_mes %>%
    filter(ID_PE %in% plot_mes$ID_PE) %>%
    dplyr::select(-c(NO_MES, IN_ESS_NC, IN_1410, DEFOL_MIN, 
                    CL_QUAL:PRIO_RECOL, STADE_DEGR:VMB_HA))

#



# Change species code

  tree_mes$sp_code <- sps_code$spCode[match(tree_mes$ESSENCE, sps_code$qc_code)]


  sum(!(plot_mes$ID_PE_MES %in% unique(tree_mes$ID_PE_MES)))
  # 534 PE_MES are missing from tree_data...
  missing_ID <- plot_mes %>% filter(!(ID_PE_MES %in% tree_mes$ID_PE_MES))
  pert <- pep_pe %>% filter(ID_PE %in% missing_ID$ID_PE)
  pert2 <- pert %>% group_by(ID_PE) %>% filter(all(is.na(ORIGINE)))
  length(unique(pert2$ID_PE))
  # all, but 11 ID_PE, were highly disturbed hence no tree
  # Remove the 11 ID_PE for which we cannot explain the absence of tree

#



# Merge with plot info

  tree_data0 <- plot_mes %>% filter(!(ID_PE %in% pert2$ID_PE)) %>%
    left_join(tree_mes, by = c("ID_PE", "ID_PE_MES"))

  added_ID <- tree_data0 %>% filter(!(ID_PE_MES %in% tree_mes$ID_PE_MES))

  levels(tree_data0$ETAT) <- c(levels(tree_data0$ETAT), "AllDead")
  tree_data0$ETAT[tree_data0$ID_PE_MES %in% added_ID$ID_PE_MES] <- "AllDead"

  # Recode plot id, tree id
  tree_data <- tree_data0 %>%
    mutate(plot_id = as.factor(ID_PE)) %>%
    mutate(tree_id = ID_ARBRE) %>%
    mutate(NO_ARBRE = as.character(NO_ARBRE)) %>%
    arrange(plot_id, tree_id) 

#



# REMOVE ALL GAULES & DHP < 90 mm

  #but not dhp = 0 or NA because can be dead trees

  tree_data <- tree_data %>% filter(!ETAT %in% c("GV","GM","GA")) %>%
    filter(DHP > 90 | DHP == 0 | is.na(DHP))

#



# REMOVE SOME STATES
  # Remove etat == 25 (intru), 44, 45, 46 (dead recruit), 34, 35, 36 (dead forgotten)

  rm <- tree_data$tree_id[tree_data$ETAT %in% c(25,44,45,46,34,35,36)]
  tree_data <- tree_data %>% 
    filter(!(tree_id %in% rm)) 

#



# CORRECT SOME TREE ID

  dead_state <- c(14, 16, 23, 24, 26, 44, 46, 54, 55, 56)
  live_state <- c(10, 12, 30, 32, 40, 42, 50, 52)

  dead_id <- tree_data$tree_id[tree_data$ETAT %in% dead_state]
  dead <- tree_data %>% 
    filter(tree_id %in% dead_id) %>%
    group_by(tree_id) %>%
    arrange(year_measured) %>%
    slice(-(1:min(which(ETAT %in% dead_state)))) # remove all rows before dead state by tree id


  # Resurection
  # then look if there are still living state in dead2
  resurected <- dead %>% subset(ETAT %in% live_state)
  length(unique(resurected$ID_ARBRE))
  # 384 resurections = mainly renumbering mistakes because disturbances

  # Only correct those that are new recruits (caused by disturbances)
  # Remove the others
  resur_recru <- resurected %>% group_by(ID_ARBRE) %>% filter(any(ETAT==40))
  resur_rm <- resurected %>% filter(!(ID_ARBRE %in% resur_recru$ID_ARBRE))
  # resur_many <- resurected %>% filter(ID_PE_MES %in% names(which(table(resurected$ID_PE_MES)>3)))
  # match(resur_many$ID_PE, resur_recru$ID_PE)
  # match(resur_recru$ID_PE, resur_many$ID_PE)
  tree_data <- tree_data %>% filter(!(ID_ARBRE %in% resur_rm$ID_ARBRE))

  for(id in unique(resur_recru$tree_id)) { # loop over resurected trees
    tmp <- subset(tree_data, tree_id == id)
    tmp_p <- subset(tree_data, plot_id == unique(tmp$plot_id))
    d <- which(tmp$ETAT %in% dead_state) # lines where dead
    l <- which(tmp$ETAT %in% live_state) # lines where alive
    if(any(min(d) < l)) { # if alive after death
      # change tree no for the last no + 1 in this plot
      tmp$NO_ARBRE[l[min(which(l > min(d)))]:nrow(tmp)] <- (max(as.numeric(tmp_p$NO_ARBRE), na.rm = TRUE) + 1)
    }
    tree_data$NO_ARBRE[which(tree_data$tree_id==id)] <- tmp$NO_ARBRE
  }

  # 2 x DEAD - Remove tree once it's dead
  dead2 <- dead %>% filter(!(tree_id %in% resurected$tree_id))

  tree_data <- tree_data %>% filter(!(ID_ARB_MES %in% dead2$ID_ARB_MES))

  # Create unique tree ids again
  fac <- with(tree_data, paste(tree_data$plot_id, tree_data$NO_ARBRE))
  tree_data <- within(tree_data, tree_id <- match(fac, unique(fac)))

  # Are all tree_id associated with only one species?
  tree_id_verif <- table(tree_data$tree_id, tree_data$sp_code)
  tree_id_verif[tree_id_verif > 0] <- 1
  length(which(rowSums(tree_id_verif) > 1)) / nrow(tree_id_verif)
  # 6318 ->  0.9% d'erreur
  dupli_tree_id <- unique(tree_data$tree_id)[which(rowSums(tree_id_verif) > 1)]
  dupli_plot_id <- subset(tree_data, tree_id %in% dupli_tree_id) 
  # NOPE - some tree ids are associated with more than one species; most seem like identification mistakes but some are from a bad renumbering
  # take the last species identification (remove NAs first)
  last_sp_code <- tree_data[tree_data$tree_id %in% dupli_tree_id,] %>%
    group_by(tree_id) %>% 
    filter(!is.na(sp_code)) %>%
    top_n(1, year_measured) %>%
    select(tree_id, sp_code)

  for(id in last_sp_code$tree_id)
    tree_data$sp_code[which(tree_data$tree_id == id)] <- last_sp_code$sp_code[last_sp_code$tree_id == id]

  # Remove trees with missing info
  # is.na(sp_code) & is.na(ETAT) & is.na(DHP)
  miss_info <- tree_data[with(tree_data, which(is.na(sp_code) & is.na(ETAT) & is.na(DHP))), ]
  table(miss_info$ID_PE)
  # Remove ID_PE == 7209501402 -> problem with the data
  tree_data <- tree_data %>% filter(!(ID_PE %in% "7209501402"))
  tree_data <- tree_data %>% filter(!(tree_id %in% miss_info$tree_id))

  # Add sp_code to ID_ARB_MES that have NAs (either dead or 29)
  na_sp_code <- subset(tree_data, is.na(sp_code))

  new_sp_code <- subset(tree_data, tree_id %in% na_sp_code$tree_id) %>% 
    group_by(tree_id) %>% slice(1L) #%>% filter(!is.na(sp_code))

  tree_data <- tree_data %>% group_by(tree_id) %>%
    dplyr::mutate(sp_code2 = dplyr::first(sp_code)) 

  tree_data <- tree_data[-with(tree_data, which(is.na(sp_code2) & ETAT != "AllDead")), ]
  # remaining NAs are species that were dead the first time they were inventoried


  # Manage ETAT == 29
  # (non-identifiable or forked trees)
  # which PEP MES contains ETAT = 29
  pepmes29 <- unique(subset(tree_data, ETAT == 29)$ID_PE_MES)
  tree_data29 <- tree_data[which(tree_data$ID_PE_MES %in% pepmes29), ]

  # which PEP MES contains ETAT=29 but not ETAT=5x (Renumbered)
  pepmes29_5x <- unique(tree_data29[(tree_data29$ETAT %in% c(50,52,54,55,56)), ]$ID_PE_MES)
  tree_data29_no5x <- subset(tree_data29, !(ID_PE_MES %in% pepmes29_5x))

  # which PEP MES has been disturbed
  disturb_pepmes <- subset(pep_pe, complete.cases(ORIGINE) | complete.cases(PERTURB))$ID_PE_MES

  # 1. if ETAT=29 and !=5X and there was a disturbance in the PEP MES change ETAT to 24
  # sometimes only 1 ETAT=29 in the disturbed PEP MES, but many other dead trees (ETAT=24,23)
  # it makes sense to consider them as 24 as well
  tree_29_disturb <- subset(tree_data29_no5x, ID_PE_MES %in% disturb_pepmes & ETAT == 29)
  tree_data$ETAT[which(tree_data$ID_ARB_MES %in% tree_29_disturb$ID_ARB_MES)] <- 24
    
  # 2. Add a new state variable and put as "unknown" all tree measures with ETAT==29 from PEP MES that does not contain any ETAT == 5X and were not disturbed
  # in growth model -> we'll remove the tree measure with unknown state and if there is only one tree measurement, we'll remove the tree_id completely
  # in mortality model -> we'll remove the tree measure with unknown state and if there is only one tree measurement, we'll remove the tree_id completely
  tree_data$state <- NA

  tree_29_nodisturb <- subset(tree_data29_no5x, !(ID_PE_MES %in% disturb_pepmes) & ETAT == 29)

  # some PEP MES still have numerous 29 (1 pep mes have more than 10) ... unreported disturbance? 
  which(table(tree_29_nodisturb$ID_PE_MES) > 10)

  # it makes sense to keep them and consider them as 24
  treeidmes29 <- subset(tree_29_nodisturb, ID_PE_MES %in% names(which(table(tree_29_nodisturb$ID_PE_MES) > 10)))
  tree_data$ETAT[tree_data$ID_ARB_MES %in% treeidmes29$ID_ARB_MES] <- 24

  # refresh
  tree_29_nodisturb <- tree_29_nodisturb[-which(tree_29_nodisturb$ID_ARB_MES %in% treeidmes29$ID_ARB_MES), ]

  # Add state == unknown
  tree_data$state[which(tree_data$ID_ARB_MES %in% tree_29_nodisturb$ID_ARB_MES)] <- "unknown"

  # if ETAT==29 and ETAT==54,55,56 (renumbered and dead) in the plot -> try to match
  # tree29 <- tree_data[which(tree_data$ETAT==29),]
  # tree54 <- tree_data[which(tree_data$ETAT %in% c(54,55,56)),]
  # 
  # tree2954 <- subset(tree_data, ID_PE_MES %in% tree29$ID_PE_MES & ID_PE_MES %in% tree54$ID_PE_MES)

  # for(p in unique(tree2954$ID_PE_MES)) {
  #   tmp <- subset(tree2954, ID_PE_MES==p)
  #   
  #   tmp29 <- tmp[tmp$ETAT == 29,]
  #   tmp5x <- tmp[tmp$ETAT %in% c(54,55,56),]
  #   
  #   species <- intersect(unique(tmp5x$sp_code),unique(tmp29$sp_code))
  #   
  #   # if no species in common
  #   if(length(species)==0) { tmp$state[tmp$ID_ARB_MES %in% c(tmp_sp29$ID_ARB_MES,tmp_sp5x$ID_ARB_MES)] <- "unknown"
  #   } else {
  #     for(sp in species) {
  #       tmp_sp29 <- tmp29[tmp29$sp_code==sp,]
  #       tmp_sp5x <- tmp5x[tmp5x$sp_code==sp,]
  #       
  #       l29 <- length(tmp_sp29$tree_id)
  #       l5x <- length(tmp_sp5x$tree_id)
  #       n <- min(l29, l5x)
  #       
  #       # match id
  #       for(i in 1:n) { 
  #         tree_data$tree_id[which(tree_data$tree_id %in% tmp_sp5x$tree_id[i])] <- tmp_sp29$tree_id[i]
  #         tree_data <- tree_data[-which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[i]),]  
  #         # DHP non comparable
  #         tree_data$DHP_NC[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[i])] <- "NC"
  #         }
  #       
  #       # unknown for the rest
  #       if(n < l29) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[(n+1):l29])] <- "unknown" }
  #       if(n < l5x) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[(n+1):l5x])] <- "unknown" }
  #     }
  #   }
  # }

  # 3. For the rest of ETAT==29,5X assign the state unknown to the tree measure
  tree_data$state[which(tree_data$ETAT == 29)] <- "unknown"
  tree_data$state[which(tree_data$ETAT %in% c(50,52))] <- "alive"
  tree_data$state[which(tree_data$ETAT %in% c(54,55,56))] <- "dead"

#



# RECLASSIFY STATES

  # ETAT == 26 -> state == "harvested"
  tree_data$state[which(tree_data$ETAT == 26)] <- "harvested"

  # ETAT == 10,12,30,32,40,42 -> state == "alive"
  tree_data$state[which(tree_data$ETAT %in% c(10,12,30,32,40,42))] <- "alive"

  # ETAT == 14,15,16,23,24,54,55,56 -> state == "dead"
  tree_data$state[which(tree_data$ETAT %in% c(14,15,16,23,24,54,55,56) & is.na(tree_data$state))] <- "dead"

  # ETAT == NA -> state == "unknown"
  tree_data$state[which(is.na(tree_data$ETAT))] <- "unknown" 
  # maybe we should remove the ID_PE_MES with unknown states from analysis

  tree_data$state[which(tree_data$ETAT == "AllDead")] <- "dead" 

#



# Check if some PE MES were deleted by mistake

  plot_mes_deleted <- setdiff(unique(tree_data0$ID_PE_MES), unique(tree_data$ID_PE_MES))
  # 545 PE MES were deleted because empty (no living mature trees)
  # except ID_PE == 7209501402 -> problem with the data

  plot_mes_deleted <- plot_mes_deleted[-grep("7209501402", plot_mes_deleted)]
      
  plot_deleted <- tree_data0 %>% filter(ID_PE_MES %in% plot_mes_deleted) %>%
    mutate(state = "AllDead") %>%
    distinct(ID_PE, ID_PE_MES, year_measured, state)


  ### Add missing PE MES

  tree_data <- tree_data %>% 
    bind_rows(plot_deleted) %>%
    arrange(ID_PE, NO_ARBRE) %>% 
    group_by(ID_PE) %>%
    dplyr::mutate(plot_id = dplyr::first(plot_id))


  setdiff(unique(tree_data0$ID_PE_MES), unique(tree_data$ID_PE_MES)) 

#



# Join xy

  tree_data <- left_join(tree_data, plot_xy, by = "ID_PE")

  plot_xy <- right_join(plot_xy, unique(tree_data[,c("ID_PE", "plot_id")]), by = "ID_PE")

#



# Basal area

  tree_data$indBA <- pi * (tree_data$DHP/(2 * 1000))^2

#
