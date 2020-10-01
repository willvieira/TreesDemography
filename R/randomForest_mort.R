#############################################
# Random forest explain and predict
# Will Vieira
# September 24, 2020
#############################################


#############################################
# Steps
# For mortality
# - Prepare mortality data
# - Run random forest with same data used in the bayesian sampling (using 4 predictors or all variables)
# - Use the RF model to predict the remaining data
# - Calculate accuracy, accuracy corrected, sensitivity and specificity
# - Plots
# - save
#
#############################################


library(data.table)
library(ranger)
set.seed(0.0)


#############################################
#               Mortality
#############################################

# Prepare mortality data
 
    mort <- readRDS('data/quebec/mort_dt.RDS')

    # get latitude and longitude from SHAPE list
    getCoord <- function(SHAPE, coord = 1) {
        n <- length(SHAPE)
        xy <- unique(unlist(SHAPE))
        return (rep(xy[coord], n))
    }
    mort[, longitude := getCoord(SHAPE, coord = 1), by = ID_PE]
    mort[, latitude := getCoord(SHAPE, coord = 2), by = ID_PE]

    # Fix PERTURB2 and ORIGINE2 NAs to "notDisturbed"
    mort[is.na(PERTURB2), PERTURB2 := 'notDisturbed']
    mort[is.na(ORIGINE2), ORIGINE2 := 'notDisturbed']
    
    # select species ids with at least 2000 measures
    sp_ids <- mort[, .N, by = sp_code2][N > 10000, sp_code2]

    # laod data sample ID to separate between fit and predict data
    for(sp in sp_ids)
        assign(paste0('sampleFit_', sp), readRDS(paste0('sampleOut/mort_', sp, '.RDS')))

#



# Run random forest with same data used in the bayesian sampling

    # variables to keep or to remove for two different models
    # First keep the four main variables we are interested
    # Second use all variables that are not correlated between themself
    
    # Model 1
    varsToKeep1 <- c('mort', 'dbh0', 'canopyDistance', 'value5_bio60_01', 'value5_bio60_12')

    # Model 2
    varsToKeep2 <- c('mort', 'dbh0', 'BA', 'value5_bio60_01', 'value5_bio60_12')
    
    # full model
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
                "BA_sp", "dbh1", "year0", "year1", "state", "growth_lag", "sp_code2", "growth")

    
    # Run random forest for each model (1 and 2) and save output in the following lists
    rfModel1_ls <- rfModel2_ls <- rfModelFull_ls <- list();
    for(sp in sp_ids)
    {
        sampleID <- get(paste0('sampleFit_', sp))

        # run RF for model 1
        db <- mort[sp_code2 == sp, varsToKeep1, with = FALSE]
        db <- db[sampleID, ]
        rf <- ranger(as.factor(mort) ~ ., data = na.omit(db))
        rfModel1_ls[[sp]] <- rf
        
        # run RF for model 1
        db <- mort[sp_code2 == sp, varsToKeep2, with = FALSE]
        db <- db[sampleID, ]
        rf <- ranger(as.factor(mort) ~ ., data = na.omit(db))
        rfModel2_ls[[sp]] <- rf

        # run RF for model full
        db <- mort[sp_code2 == sp, setdiff(names(mort), varsToRm), with = FALSE]
        db <- db[sampleID, ]
        rf <- ranger(as.factor(mort) ~ ., data = na.omit(db))
        rfModelFull_ls[[sp]] <- rf

        cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
    }

#



# Use the RF model to predict the remaining data

    predModel1_ls <- predModel2_ls <- predModelFull_ls <- list()
    for(sp in sp_ids)
    {
        sampleID <- get(paste0('sampleFit_', sp))

        # prediction for model 1
        db <- mort[sp_code2 == sp, varsToKeep1, with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModel1_ls[[sp]], db, type = "response")$predictions
        predModel1_ls[[sp]] <- db

        # prediction for model 2
        db <- mort[sp_code2 == sp, varsToKeep2, with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModel2_ls[[sp]], db, type = "response")$predictions
        predModel2_ls[[sp]] <- db

        # run RF for model full
        db <- mort[sp_code2 == sp, setdiff(names(mort), varsToRm), with = FALSE]
        db <- na.omit(db[-sampleID, ])
        db$pred <- predict(rfModelFull_ls[[sp]], db, type = "response")$predictions
        predModelFull_ls[[sp]] <- db
        
        cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
    }

#



# Calculate accuracy, accuracy corrected, sensitivity and specificity

     acc_dt <- data.table(sp = character(),
                       model = integer(),
                       acc = numeric(),
                       sensitivity = numeric(),
                       specificity = numeric(),
                       accCorrected = numeric())

    for(sp in sp_ids)
    {
        for(mod in c('1', '2', 'Full'))
        {
            db <- get(paste0('predModel', mod, '_ls'))[[sp]]

            TP <- db[mort == 1 & pred == 1, .N]
            TN <- db[mort == 0 & pred == 0, .N]
            FN <- db[mort == 0 & pred == 1, .N]
            FP <- db[mort == 1 & pred == 0, .N]

            acc <- (TP + TN)/(TP + TN + FP + FN)
            sensitivity <- TP/(TP + FN)
            specificity <- TN/(FP + TN)
            accCorrected <- (sensitivity + specificity)/2

            acc_dt <- rbind(acc_dt, data.frame(sp = sp, model = mod, acc = acc, accCorrected = accCorrected, sensitivity = sensitivity, specificity = specificity))
        }
    }

#



# Plots

    # color as a function os sp_ids
    acc_dt <- merge(acc_dt, data.table(sp = sp_ids, col = RColorBrewer::brewer.pal(length(sp_ids), 'Paired')))
    # pch as a function of model
    acc_dt$pch <- 16:18

    par(mfrow = c(1, 2), mar = c(3, 3, .5, .5), oma = c(.5, .5, 2, .2), mgp = c(1.8, 0.3, 0), tck = -.008)
    # Plot Accuracy vs accuracy corrected
    acc_dt[, plot(acc * 100, accCorrected * 100, xlab = 'Accuracy (%)', ylab = 'Accuracy corrected (%)', pch = pch, col = col)]
    segments(acc_dt[model == 1, acc * 100], acc_dt[model == 1, accCorrected * 100], acc_dt[model == 2, acc * 100], acc_dt[model == 2, accCorrected * 100], col = acc_dt[model == 1, col], lwd = 0.8)
    segments(acc_dt[model == 2, acc * 100], acc_dt[model == 2, accCorrected * 100], acc_dt[model == 'Full', acc * 100], acc_dt[model == 'Full', accCorrected * 100], col = acc_dt[model == 2, col], lwd = 0.8)
    legend('topright', legend = c('Model1', 'Model2', 'ModelFull'), pch = 16:18, bty = 'n', cex = 0.85)
    # Plot Sensitivity vs specificity
    acc_dt[, plot(specificity * 100, sensitivity * 100, xlim = c(min(specificity) * 100, (max(specificity) * 100) +  2), ylab = 'Sensitivity (%)', xlab = 'Specificity (%)', pch = pch, col = col)]
    segments(acc_dt[model == 1, specificity * 100], acc_dt[model == 1, sensitivity * 100], acc_dt[model == 2, specificity * 100], acc_dt[model == 2, sensitivity * 100], col = acc_dt[model == 1, col], lwd = 0.8)
    segments(acc_dt[model == 2, specificity * 100], acc_dt[model == 2, sensitivity * 100], acc_dt[model == 'Full', specificity * 100], acc_dt[model == 'Full', sensitivity * 100], col = acc_dt[model == 2, col], lwd = 0.8)
    legend('topright', legend = sp_ids, lty = 1, col = RColorBrewer::brewer.pal(length(sp_ids), 'Paired'), bty = 'n', cex = 0.8)

#



# Save
    
    saveRDS(acc_dt[, setdiff(names(acc_dt), c('col', 'pch')), with = FALSE], 'data/quebec/randomForest_mort.RDS')

#
