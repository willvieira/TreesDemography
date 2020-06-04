##############################
# Get non climatic environmental variables
# May 29, 2020
##############################


##############################
# Steps
#   - load variables from db
#   - reclassify variables
##############################



# Environmental variables of interest:
  # Soil variables (humus type, humus ph, pierrosity, drainage)
  # Disturbances (logging, burning, windfall, insect outbreak)
  # Forest age



# Get data and variables of interest

  # Layer containing soil variables (humus, texture, ph)
  pep_sol <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "STATION_SOL")

  # Layer containing disturbance variables and drainage
  pep_pe <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "STATION_PE")

  # Layer containing age of selected trees in all PE MES
  pep_arb <- sf::st_read("rawData/PEP_GDB/PEP.gdb", layer = "DENDRO_ARBRES_ETUDES")

  # For soil type, take the measurements for each PEP MES (humus type, organic matter depth, sometimes vary through year mainly because of disturbances)
  # For analysis, we could decide to take only the last measurements or the mean (for quantitative variable)

  # % of complete cases
  knitr::kable(sort(apply(pep_sol, 2, function(x) length(which(complete.cases(x))) / nrow(pep_sol) * 100)))
  knitr::kable(sort(apply(pep_pe, 2, function(x) length(which(complete.cases(x))) / nrow(pep_pe) * 100)))
  knitr::kable(sort(apply(pep_arb, 2, function(x) length(which(complete.cases(x))) / nrow(pep_arb) * 100)))


  # SOIL VARIABLES
  pep_sol <- pep_sol %>%
    dplyr::select(ID_PE, ID_PE_MES, TYPEHUMUS, EPMATORG, PH_HUMUS,PH_HORIZB, POURCPIERR)

  # DISTURBANCE VARIABLES
  pep_pe <- pep_pe %>%
    dplyr::select(ID_PE, ID_PE_MES, CL_DRAI, ORIGINE, PERTURB)

  # DISTURBANCE VARIABLES
  pep_arb <- pep_arb %>%
    dplyr::select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES, AGE)

  # JOIN VARIBALES
  env_data <- tree_data %>%
    dplyr::select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
    dplyr::distinct() %>%
    dplyr::left_join(pep_sol, by = c("ID_PE", "ID_PE_MES")) %>%
    dplyr::left_join(pep_pe, by = c("ID_PE", "ID_PE_MES"))

  age_data <- tree_data %>%
    dplyr::select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES) %>%
    dplyr::left_join(pep_arb, by = c("ID_PE", "ID_PE_MES", "ID_ARBRE", "ID_ARB_MES"))

#



# RECLASSIFY VARIABLES

  knitr::kable(sort(table(pep_pe$ORIGINE))) # 3965 plots with coupe total
  # ORIGINE => Les perturbations naturelles et les interventions anthropiques d’origine qui ont éliminé plus de 75 % de la surface terrière du peuplement précédent
  knitr::kable(sort(table(pep_pe$PERTURB)))

  ## DRAINAGE
  env_data <- env_data %>%
    dplyr::mutate(CL_DRAI2 = dplyr::case_when(CL_DRAI %in% 0 ~ "excessif",
                                              CL_DRAI %in% c(10:14) ~ "rapide",
                                              CL_DRAI == 16 ~ "complexe",
                                              CL_DRAI %in% c(20:24) ~ "bon",
                                              CL_DRAI %in% c(30:34) ~ "modere",
                                              CL_DRAI %in% c(40:44) ~ "imparfait",
                                              CL_DRAI %in% c(50:54) ~ "mauvais",
                                              CL_DRAI %in% c(60:64) ~ "tres_mauvais"))
  
  env_data$CL_DRAI2 <- as.factor(env_data$CL_DRAI2)


  ## PERTURBATION D'ORIGINE
  env_data <- env_data %>%
    dplyr::mutate(ORIGINE2 = dplyr::case_when(ORIGINE == "BR" ~ "burn",
                                              ORIGINE %in% c("CBA", "CBT", "CPR", "CT") ~ "logging",
                                              ORIGINE %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
                                              ORIGINE %in% c("P", "PLN", "PLR", "ENS") ~ "plantation",
                                              ORIGINE == "ES" ~ "severe_outbreak",
                                              ORIGINE == "FR" ~ "wasteland"))
  
  env_data$ORIGINE2 <- as.factor(env_data$ORIGINE2)

  ## PERTURBATION PARTIELLE
  env_data <- env_data %>%
    dplyr::mutate(PERTURB2 = dplyr::case_when(PERTURB == "BRP" ~ "partial_burn",
                                              PERTURB %in% c("CAM","CB","CD","CDL","CE","CJ","CP","DLD","EPC") ~
                                                "partial_logging",
                                              PERTURB == "EL" ~ "light_outbreak",
                                              PERTURB %in% c("CHP", "VEP", "DP") ~ "partial_windfall")) # DP = dépérissement
  
  env_data$PERTURB2 <- as.factor(env_data$PERTURB2)

  ## AGE
  age_data <- age_data %>%
    dplyr::group_by(ID_PE_MES) %>%
    dplyr::summarise(age_mean = mean(as.integer(AGE), na.rm = T)) %>%
    tidyr::replace_na(list(age_mean=NA))

  env_data <- env_data %>%
    left_join(age_data, by = "ID_PE_MES")

  # Order and select last soil measures

  env_data <- env_data %>% 
    dplyr::select(ID_PE:year_measured, 
                  ORIGINE:PERTURB, 
                  ORIGINE2:PERTURB2, age_mean,
                  TYPEHUMUS:CL_DRAI, CL_DRAI2) %>%
    dplyr::group_by(plot_id) %>%
    dplyr::arrange(year_measured, .by_group = TRUE) %>% 
    dplyr::mutate_at(vars(TYPEHUMUS:CL_DRAI2), last)

#


