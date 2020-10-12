#############################################
# Random forest explain and predict
# Will Vieira
# September 24, 2020
# Last updated: September 30, 2020
#############################################


#############################################
# Steps
# For growth
# - Prepare growth data
# - Run random forest with same data used in the bayesian sampling (using 4 predictors or all variables)
# - Use the RF model to predict the remaining data
# - Calculate Mean Squared Error of predictions
# - Plots
# - save
#############################################


library(data.table)
library(ranger)
library(tidyverse)
set.seed(0.0)


# Prepare growth data
 
    growth <- readRDS('data/quebec/growth_dt.RDS')

    # get latitude and longitude from SHAPE list
    getCoord <- function(SHAPE, coord = 1) {
        n <- length(SHAPE)
        xy <- unique(unlist(SHAPE))
        return (rep(xy[coord], n))
    }
    growth[, longitude := getCoord(SHAPE, coord = 1), by = ID_PE]
    growth[, latitude := getCoord(SHAPE, coord = 2), by = ID_PE]

    # Fix PERTURB2 and ORIGINE2 NAs to "notDisturbed"
    growth[is.na(PERTURB2), PERTURB2 := 'notDisturbed']
    growth[is.na(ORIGINE2), ORIGINE2 := 'notDisturbed']
    
    # Calculate relative basal area (BA_ind/sum(BA_ind) for each plot-year)
    growth[, sumBA := BA * 399.7312/1e4, by = list(ID_PE, year0)]
    growth[, relativeBA_ind := indBA/sumBA]
    growth[, sumBA := NULL]
     
    # select species ids with at least 2000 measures
    sp_ids <- growth[, .N, by = sp_code2][N > 10000, sp_code2]

    # laod data sample ID to separate between fit and predict data
    for(sp in sp_ids)
        assign(paste0('sampleFit_', sp), readRDS(paste0('sampleOut/growth_', sp, '.RDS')))

#



# Quick viz

    # Growht vs temp
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(value5_bio60_01, growth, fill = value5_bio60_12, color = value5_bio60_12)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))

    # Growht vs prec
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(value5_bio60_12, growth, fill = value5_bio60_01, color = value5_bio60_01)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))

    # Growht vs size
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(dbh0, growth, fill = canopyDistance, color = canopyDistance)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))

    # Growht vs canopyDistance
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(canopyDistance, growth, fill = dbh0, color = dbh0)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))

    # Growth vs BA
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(BA, growth, fill = dbh0, color = dbh0)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))

#



# Run random forest with same data used in the bayesian sampling

    # variables to keep or to remove for two different models
    # First keep the four main variables we are interested
    # Second use all variables that are not correlated between themself
    
    # Model 1
    varsToKeep1 <- c('growth', 'dbh0', 'canopyDistance', 'value5_bio60_01', 'value5_bio60_12')

    # Model 2
    varsToKeep2 <- c('growth', 'dbh0', 'BA', 'value5_bio60_01', 'value5_bio60_12')

    # Model full
    varsToRm <- c("value5_pcp60_01", "value5_pcp60_02", "value5_pcp60_03", "value5_pcp60_04",
                "value5_pcp60_05", "value5_pcp60_06", "value5_pcp60_07", "value5_pcp60_08",
                "value5_pcp60_09", "value5_pcp60_10", "value5_pcp60_11", "value5_pcp60_12",
                "value5_cmi60_01", "value5_cmi60_02", "value5_cmi60_03", "value5_cmi60_04",
                "value5_cmi60_05", "value5_cmi60_06", "value5_cmi60_07", "value5_cmi60_08",
                "value5_cmi60_09", "value5_cmi60_10", "value5_cmi60_11", "value5_cmi60_12",
                "value5_bio60_02", "value5_bio60_05", "value5_bio60_06", "value5_bio60_07",
                "value5_bio60_10", "value5_bio60_11", "value5_bio60_12", "value5_bio60_13",
                "value5_bio60_16", "value5_bio60_17", "value5_bio60_18", "value5_bio60_19",
                "ORIGINE", "PERTURB", "CAUS_DEFOL", "ENSOLEIL", "DEFOL_MAX", "relativeBA_sp", "ETAT", "ETAGE_ARB", "sp_code", "SHAPE",
                "ecoreg3", "height", "nbMeasure", "s_star", "canopyStatus", "indBA",
                "BA_sp", "dbh1", "year0", "year1", "state", "mort", "growth_lag", "sp_code2")

    
    # Run random forest for each model (1, 2, and full) and save output in the following lists
    rfModel1_ls <- rfModel2_ls <- rfModelFull_ls <- list();
    for(sp in sp_ids)
    {
        sampleID <- get(paste0('sampleFit_', sp))

        # run RF for model 1
        db <- growth[sp_code2 == sp, varsToKeep1, with = FALSE]
        #db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
        rfModel1_ls[[sp]] <- rf
        
        # run RF for model 2
        db <- growth[sp_code2 == sp, varsToKeep2, with = FALSE]
        #db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
        rfModel2_ls[[sp]] <- rf

        # run RF for model full
        db <- growth[sp_code2 == sp, setdiff(names(growth), varsToRm), with = FALSE]
        #db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
        rfModelFull_ls[[sp]] <- rf

        cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
    }

    lapply(rfModelFull_ls, function(x) knitr::kable(sort(importance(x)/max(importance(x)), decreasing = T)))


#



# Use the RF model to predict the remaining data

    predModel1_ls <- predModel2_ls <- predModelFull_ls <- list()
    for(sp in sp_ids)
    {
        sampleID <- get(paste0('sampleFit_', sp))

        # prediction for model 1
        db <- growth[sp_code2 == sp, c(varsToKeep1, 'sp_code2'), with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModel1_ls[[sp]], db, type = "response")$predictions
        predModel1_ls[[sp]] <- db

        # prediction for model 2
        db <- growth[sp_code2 == sp, c(varsToKeep2, 'sp_code2'), with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModel2_ls[[sp]], db, type = "response")$predictions
        predModel2_ls[[sp]] <- db
        
        # prediction for model full
        db <- growth[sp_code2 == sp, setdiff(names(growth), varsToRm[-60]), with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModelFull_ls[[sp]], db, type = "response")$predictions
        predModelFull_ls[[sp]] <- db
        
        cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
    }

#



# Calculate Mean Squared Error of predictions

    growthPred_m1 <- do.call("rbind", predModel1_ls)
    growthPred_m2 <- do.call("rbind", predModel2_ls)
    growthPred_mFull <- do.call("rbind", predModelFull_ls)

    # squared error
    growthPred_m1[, sqErr := (growth - pred)^2]
    growthPred_m2[, sqErr := (growth - pred)^2]
    growthPred_mFull[, sqErr := (growth - pred)^2]

    # growth variance (total variation)
    growthPred_m1[, gVar := (growth - mean(growth))^2]
    growthPred_m2[, gVar := (growth - mean(growth))^2]
    growthPred_mFull[, gVar := (growth - mean(growth))^2]

    # Mean Squared Error (MSE)
    out_df <- setNames(growthPred_m1[, mean(sqErr), by = sp_code2], c('sp', 'MSE_m1'))
    out_df$MSE_m2 <- growthPred_m2[, mean(sqErr), by = sp_code2][, 2]
    out_df$MSE_mFull <- growthPred_mFull[, mean(sqErr), by = sp_code2][, 2]

    # Rsquared
    out_df$Rsquared_m1 <- growthPred_m1[, 1-(sum(sqErr)/sum(gVar)), by = sp_code2][, 2]
    out_df$Rsquared_m2 <- growthPred_m2[, 1-(sum(sqErr)/sum(gVar)), by = sp_code2][, 2]
    out_df$Rsquared_mFull <- growthPred_mFull[, 1-(sum(sqErr)/sum(gVar)), by = sp_code2][, 2]

    # Long format
    MSE_lg <- tidyr::pivot_longer(out_df[, -(5:7)], names_prefix = 'MSE_',
                                  cols = c('MSE_m1', 'MSE_m2', 'MSE_mFull'),
                                  names_to = 'model', values_to = 'MSE')
    
    out_lg <- out_df[, -(2:4)] %>%
                tidyr::pivot_longer(names_prefix = 'Rsquared_',
                                    cols = c('Rsquared_m1', 'Rsquared_m2', 'Rsquared_mFull'),
                                    names_to = 'model', values_to = 'Rsquared') %>%
                dplyr::left_join(MSE_lg, by = c('sp', 'model'))

#



# Plots

    library(ggplot2)    
    ggplot(out_lg, aes(y = sp, x = Rsquared, fill = model, colour = model)) +
        geom_point()
    quartz()
    ggplot(out_lg, aes(y = sp, x = MSE, fill = model, colour = model)) +
        geom_point()

#




# Save

    saveRDS(out_lg, 'data/quebec/randomForest_growth.RDS')

#
