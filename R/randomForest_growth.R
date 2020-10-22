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

    # Temp vs prec vs growth
    ggplot(growth[sp_code2 %in% sp_ids & growth < 5.5],
        aes(value5_bio60_01, value5_bio60_12, fill = growth, color = growth)) +
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
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01)) +
        stat_smooth(method = 'lm', formula = 'y ~ poly(x, 2)', col = 'red')

    # Growth vs BA
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(BA, growth, fill = dbh0, color = dbh0)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01)) +
        stat_smooth(method = 'lm', formula = 'y ~ x', col = 'red')

    # Growth vs BA_sp (CNDD)
    ggplot(growth[sp_code2 %in% sp_ids],
        aes(relativeBA_sp, growth, fill = dbh0, color = dbh0)) +
        geom_point(alpha = 0.5, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01)) +
        stat_smooth(method = 'lm', formula = 'y ~ x', col = 'red')
    
    # BA vs canopyDistance vs growth
    ggplot(growth[sp_code2 %in% sp_ids & growth < 5.5],
        aes(BA, canopyDistance, fill = growth, color = growth)) +
        geom_point(alpha = 0.4, size = 0.4) +
        facet_wrap(~ sp_code2) + geom_rug(col = rgb(0, 0, 0, alpha = .01))
        
    # BA vs size vs growth
    ggplot(growth[sp_code2 %in% sp_ids & growth < 5.5],
        aes(dbh0, BA, fill = growth, color = growth)) +
        geom_point(alpha = 0.4, size = 0.4) +
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
        db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db))
        rfModel1_ls[[sp]] <- rf
        
        # run RF for model 2
        db <- growth[sp_code2 == sp, varsToKeep2, with = FALSE]
        db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db))
        rfModel2_ls[[sp]] <- rf

        # run RF for model full
        db <- growth[sp_code2 == sp, setdiff(names(growth), varsToRm), with = FALSE]
        db <- db[sampleID, ]
        rf <- ranger(growth ~ ., data = na.omit(db))
        rfModelFull_ls[[sp]] <- rf

        cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
    }

    lapply(rfModelFull_ls,
           function(x) knitr::kable(sort(importance(x)/max(importance(x)), decreasing = T),
                caption = paste('R2 =', round(x$r.squared, 2), 'MSE = ', round(x$prediction.error, 2))))
    for(sp in sp_ids) cat(sp, '\n', rfModel1_ls[[sp]]$r.squared, '\n', rfModel2_ls[[sp]]$r.squared, '\n')

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

    # Rsquared and MSE for each species
    ggplot(out_lg, aes(y = sp, x = Rsquared, fill = model, colour = model)) +
        geom_point()
    quartz()
    ggplot(out_lg, aes(y = sp, x = MSE, fill = model, colour = model)) +
        geom_point()


    # Real vs predicted growth rate for each species
    ggplot(growthPred_m1, aes(growth, pred)) +
        geom_point(alpha = 0.2, size = 0.2) +
        geom_abline(intercept = 0, slope = 1) +
        facet_wrap(~ sp_code2) +
        xlim(0, 10) + ylim(0, 10)
    
    ggplot(growthPred_m2, aes(growth, pred)) +
        geom_point(alpha = 0.2, size = 0.2) +
        geom_abline(intercept = 0, slope = 1) +
        facet_wrap(~ sp_code2) +
        xlim(0, 10) + ylim(0, 10)

    ggplot(growthPred_mFull, aes(growth, pred)) +
        geom_point(alpha = 0.2, size = 0.2) +
        geom_abline(intercept = 0, slope = 1) +
        stat_smooth(method = 'lm', col = 'red') +
        facet_wrap(~ sp_code2) +
        xlim(0, 10) + ylim(0, 10)

#



# Calculate intercept and slope of fitted curve for real growth vs predicted growth

    curveModel_ls <- list()
    for(sp in sp_ids)
    {
        md1 <- predModel1_ls[[sp]][, lm(pred ~ growth)]
        md2 <- predModel2_ls[[sp]][, lm(pred ~ growth)]
        mdFull <- predModelFull_ls[[sp]][, lm(pred ~ growth)]

        DF <- data.frame(model = c('model1', 'model2', 'modelFull'),
                         Intercept = c(md1$coefficients[[1]], md2$coefficients[[1]], mdFull$coefficients[[1]]),
                         Slope = c(md1$coefficients[[2]], md2$coefficients[[2]], mdFull$coefficients[[2]]))

        curveModel_ls[[sp]] <- DF
    }

#



# Save

    saveRDS(out_lg, 'data/quebec/randomForest_growth.RDS')
    saveRDS(curveModel_ls, 'data/quebec/randomForest_predCurve.RDS')

#






##########################################################################################
# Experiments
##########################################################################################


##########################################
# Experiment 1
# - Removing "upper outliers" or everything is higher than the 90, 95 or 99% quantile
# - The idea is that removing these heigher values, we could fix the pdg (or potential growth rate) to the heigher growth rate 90% 
##########################################

# So here I will try and run the same random forest experiment and see if removing "upper outliers" improves our estimations
rfModel1b_ls <- rfModel2b_ls <- rfModelFullb_ls <- list();
for(sp in sp_ids)
{
    # run RF for model 1
    db <- growth[sp_code2 == sp, varsToKeep1, with = FALSE]
    #db <- db[sampleID, ]
    db <- db[growth <= quantile(growth, probs = 0.99)]
    rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
    rfModel1b_ls[[sp]] <- rf
    
    # run RF for model 2
    db <- growth[sp_code2 == sp, varsToKeep2, with = FALSE]
    #db <- db[sampleID, ]
    db <- db[growth <= quantile(growth, probs = 0.99)]
    rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
    rfModel2b_ls[[sp]] <- rf

    # run RF for model full
    db <- growth[sp_code2 == sp, setdiff(names(growth), varsToRm), with = FALSE]
    #db <- db[sampleID, ]
    db <- db[growth <= quantile(growth, probs = 0.99)]
    rf <- ranger(growth ~ ., data = na.omit(db), importance = 'impurity_corrected')
    rfModelFullb_ls[[sp]] <- rf

    cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
}


# Compare first model with all values of growth to the < 90%
out_dt <- data.table(sp = character(), test = character(), model = character(), Rsquared = numeric(), MSE = numeric(), Imp = character())
for(sp in sp_ids)
{
    for(test in c('', 'b'))
    {
        Rsq <- MSE <- Imp <- setNames(numeric(3), c('1', '2', 'Full'))
        for(model in c('1', '2', 'Full'))
        {
            Rsq[model] <- get(paste0('rfModel', model, test, '_ls'))[[sp]]$r.squared
            MSE[model] <- get(paste0('rfModel', model, test, '_ls'))[[sp]]$prediction.error
            imp <- sort(importance(get(paste0('rfModel', model, test, '_ls'))[[sp]]), decreasing = T)[1:4]
            imp <- imp/max(imp)
            Imp[model] <- paste(paste(names(imp), round(imp, 2)), collapse = '\n')
        }
        out_dt <- rbind(out_dt, data.table(sp = rep(sp, 3), test = rep(test, 3), model = c('1', '2', 'Full'), Rsquared = Rsq, MSE = MSE, Imp = Imp))
    }
}

ggplot() + geom_text(out_dt[model == 'Full'], mapping = aes(Rsquared, MSE, color = test, label = paste0(sp, '\n', Imp)), size = 3.3)

# When removing "upper outliers", MSE decreases considerably.
# R2 however, also decreases depending on the quantile probs.
# 90% reduces considerably R2 while 99% keeps r2 almost the same, with significant reduction in the MSE




##########################################
# Experiment 2
# - Multiple models depending on the latitudinal position
# - Split data between south, mid, and north distribution
# - Run model for each of these locations and test if variable importance changes
##########################################

# calculate latitude limits
limits <- growth[sp_code2 %in% sp_ids, max(latitude), by = sp_code2]
limits$lower <- growth[sp_code2 %in% sp_ids, min(latitude), by = sp_code2]$V1
limits[, diff := (V1 - lower)/3]

# plot distribution along with limits
par(mfrow = c(3, 4))
for(sp in sp_ids)
{
    growth[sp_code2 == sp, plot(longitude, latitude, main = sp)]
    abline(h = limits[sp_code2 == sp, lower + diff], col = 2)
    abline(h = limits[sp_code2 == sp, lower + diff * 2], col = 2)
}


# So here I will try and run the same random forest experiment and see if removing "upper outliers" improves our estimations
rfModelS_ls <- rfModelM_ls <- rfModelN_ls <- list();
for(sp in sp_ids)
{ 
    # run RF full model for the three locations
    # South
    dbAll <- growth[sp_code2 == sp, setdiff(names(growth), varsToRm), with = FALSE]
    db <- dbAll[latitude <= limits[sp_code2 == sp, lower + diff]]
    rf <- ranger(growth ~ ., data = na.omit(db[, latitude := NULL]), importance = 'impurity_corrected')
    rfModelS_ls[[sp]] <- rf

    # Mid
    db <- dbAll[latitude > limits[sp_code2 == sp, lower + diff] & latitude <= limits[sp_code2 == sp, lower + diff * 2]]
    rf <- ranger(growth ~ ., data = na.omit(db[, latitude := NULL]), importance = 'impurity_corrected')
    rfModelM_ls[[sp]] <- rf

    # North
    db <- dbAll[latitude > limits[sp_code2 == sp, lower + diff * 2]]
    rf <- ranger(growth ~ ., data = na.omit(db[, latitude := NULL]), importance = 'impurity_corrected')
    rfModelN_ls[[sp]] <- rf

    cat('   Running for species', which(sp == sp_ids), 'of', length(sp_ids), '\r')
}


# Compare first model with all values of growth to the < 90%
out_dt <- data.table(sp = character(), region = character(), Rsquared = numeric(), MSE = numeric(), sampleSize = numeric(), Imp = character())
for(sp in sp_ids)
{
    Rsq <- MSE <- Imp <- sampleSize <- setNames(numeric(3), c('S', 'M', 'N'))
    for(rg in c('S', 'M', 'N'))
    {
        Rsq[rg] <- get(paste0('rfModel', rg, '_ls'))[[sp]]$r.squared
        MSE[rg] <- get(paste0('rfModel', rg, '_ls'))[[sp]]$prediction.error
        sampleSize[rg] <- get(paste0('rfModel', rg, '_ls'))[[sp]]$num.samples
        imp <- sort(importance(get(paste0('rfModel', rg, '_ls'))[[sp]]), decreasing = T)[1:6]
        imp <- imp/max(imp)
        Imp[rg] <- paste(sp, sampleSize[rg], '\n', paste(paste(names(imp), round(imp, 2)), collapse = '\n'))
    }
    out_dt <- rbind(out_dt, data.table(sp = rep(sp, 3), region = c('S', 'M', 'N'), Rsquared = Rsq, MSE = MSE, sampleSize = sampleSize, Imp = Imp))
}

ggplot() + geom_text(out_dt, mapping = aes(Rsquared, MSE, label = Imp), size = 2.6) + facet_wrap(~ factor(region, levels = c('S', 'M', 'N')))

quartz()
sp <- 'PINBAN'
db <- growth[sp_code2 == sp]
var1 = 'BA'
var2 = 'dbh0'
var3 = 'value5_bio60_01'
var4 = 'value5_bio60_12'

dbPlotN <- db[latitude > limits[sp_code2 == sp, lower + diff * 2]]
dbPlotM <- db[latitude > limits[sp_code2 == sp, lower + diff] & latitude <= limits[sp_code2 == sp, lower + diff * 2]]
dbPlotS <- db[latitude <= limits[sp_code2 == sp, lower + diff]]

par(mfrow = c(3, 4))
for(rg in c('N', 'M', 'S'))
{
    dbPlot <- get(paste0('dbPlot', rg))
    for(var in paste0('var', 1:4))
    {
        plot(dbPlot[, colnames(dbPlot) %in% c(get(var), 'growth'), with = FALSE], col = rgb(0, 0, 0, 0.2))
    }
}

# PERTURB2 for THUOCC
# value5_bio60_09 for BETPAP
# value5_cmi60_sum and ORIGINE2 for PICGLA
# PERTURB2 for BETALL (temperature is not working)
# ORIGINE2 for ABIBAL
# ACESAC temperature is not working!
