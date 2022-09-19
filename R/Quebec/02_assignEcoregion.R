##############################
# Assign plot to an ecoregion
# May 23, 2020
##############################


##############################
# Steps
#   - download ecoregions
#   - reclassify ecoregions
#   - assign ecoregions for plots
#   - get quebec boundary
##############################




# Download ecoregion data

  dtLink = 'https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Classification_ecologique/CLASSI_ECO_QC_GDB.zip'

  ecoDir = 'rawData/CLASSI_ECO_GDB'

  if(!dir.exists(ecoDir))
  {
    ecoFile = 'rawData/CLASSI_ECO_REGION.zip'
    
    if(!file.exists(ecoFile))
      download.file(url = dtLink, destfile = ecoFile, method = 'auto', quiet = TRUE)

    unzip(ecoFile, exdir = 'rawData')
    file.remove(ecoFile)
  }

#



# Reclassify ecoregions

  ecoregion <- sf::st_read('rawData/CLASSI_ECO_QC_GDB/CLASSI_ECO_QC.gdb')

  ecoregion <- ecoregion %>% select(SOUS_DOM11 = SDOM_BIO)
  
  # Reclassify ecoregion - 11, 10, 6, or 3
  ecoregion <- ecoregion %>% 
    mutate(SOUS_DOM6 = recode_factor(SOUS_DOM11,
                                    "1" = "Sugar maple-bitternut hickory",
                                    "2E" = "Sugar maple-basswood",   
                                    "2O" = "Sugar maple-basswood",
                                    "3E" = "Sugar maple-yellow birch",  
                                    "3O" = "Sugar maple-yellow birch",
                                    "4E" = "Balsam fir-yellow birch",
                                    "4O" = "Balsam fir-yellow birch",
                                    "5E" = "Balsam fir-white birch",
                                    "5O" = "Balsam fir-white birch", 
                                    "6E" = "Spruce-moss",
                                    "6O" = "Spruce-moss"))

  ecoregion <- ecoregion %>% 
    mutate(SOUS_DOM3 = case_when(SOUS_DOM11 %in% c("1","2E","2O","3E", "3O") ~ "Hardwood", 
                                SOUS_DOM11 %in% c("4E", "4O") ~ "Mixed",
                                SOUS_DOM11 %in% c("5E", "5O", "6E", "6O") ~ "Boreal"))


  # Convert coordinates
  ecoregion <- sf::st_transform(ecoregion, 32198)

#



# ASSIGN PLOTS TO ECOREGION

  #### Assigning each sample to the region where it belongs ####
  xy_assign_reg <- sf::st_intersection(plot_xy, ecoregion) %>% 
    dplyr::distinct(ID_PE, .keep_all = TRUE)

  # mapview::mapview(xy_assign_reg, zcol = "SOUS_DOM3")

  # Some plots are missing because they fall just outside the ecoregion polygon area (some are to far north, some are close to the boundary)
  # I'll assign them to the nearest neighboor region

  xy_unassign <- plot_xy %>%
    dplyr::filter(!(plot_id %in% xy_assign_reg$plot_id)) 


  xy_unassign <- sf::st_join(xy_unassign, ecoregion, join = st_nearest_feature)
    
  # mapview::mapview(xy_unassign, zcol = "SOUS_DOM3")

  xy_assign_reg1 <- dplyr::bind_rows(xy_assign_reg, xy_unassign) %>% 
    dplyr::arrange(plot_id)

  ecoreg_df <- xy_assign_reg1 %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(ecoreg11 = SOUS_DOM11, ecoreg6 = SOUS_DOM6, ecoreg3 = SOUS_DOM3)

  ### QUEBEC BOUNDARY ####

  # Create a polygone of the region without subdivision
  bound_CAN <- raster::getData(country = 'CAN', level = 1, path = "data/quebec")
  bound_Qc <- bound_CAN[bound_CAN@data$NAME_1 == "Québec",]

  bound_Qc <- st_as_sf(bound_Qc)
  # mapview::mapview(bound_Qc)

#
