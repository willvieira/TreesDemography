##############################
# Prepare ingrowth dataset
# Will Vieira
# Oct 28, 2021
##############################


##############################
# regCover: percentage cover of plants with height < 1.3 m
# nbStems: stems height > 1.3 m and dbh < 1.1 cm
# ingrowth: # of individuals crossing the 1 cm dbh threshold
##############################


# reload tree_data before transition
tree_data <- readRDS('data/RESEF/tree_data_beforeTransition.RDS')




# Remove plot_id with only one year_measured
# Number of measures by plot_id
tree_data[, nbYear := length(unique(year)), by = plot_id]
# keep plots with more than one measures (so I can quantify recruitment number)
tree_data <- tree_data[nbYear > 1]


# function to define if measurement is a recruit or not by plot
getRecruitment <- function(year_measured, tree_id) {

    # get nb years and unique
    uqYear = unique(year_measured)
    nbYear = length(uqYear)

    # vector specifying if is a recruitment for each measure
    isRecruit <- rep(FALSE, sum(uqYear[1] == year_measured))

    for(year in 1:(nbYear -1))
    {
        # which tree_ids are recruit for this year?
        newRecruit <- !(tree_id[which(uqYear[year + 1] == year_measured)] %in%
                        tree_id[which(year_measured %in% uqYear[1:year])])

        isRecruit <- append(isRecruit, newRecruit)
    }

    return( isRecruit )
}


# get recruitment individuals
tree_data[, isRecruit := getRecruitment(year, tree_id), by = plot_id]

# any recruit with dbh higher than 13 cm is probably not a recruit (happened for 1 individual)
tree_data[isRecruit == TRUE & dbh > 130, isRecruit := FALSE]


# ## some plots
# hist(tree_data[, unique(nbRecruit), by = plot_id]$V1, breaks = 30)
# par(mfrow = c(1, 2))
# tree_data[dbh < 500, hist(dbh, breaks = 30, col = 'grey')]
# tree_data[dbh < 80 & isRecruit == T, hist(dbh, breaks = 30, col = 'grey')]    
# ##


tree_data[, nbRecruit := sum(isRecruit), by = .(plot_id, subplot_id, spCode, year)]

# Reduce data.fram to get only number of recruit by plot/subplot, spCode and year
fec <- tree_data[, .SD[1], by = .(plot_id, subplot_id, spCode, year)]

# remove columns with individual information
colToRm <- c('tree_id', 'dbh', 'state', 'canopyState', ' heightMeasured',
             'height', 'x', 'y', 'obs', 'state2', 'indBA', 'cumX', 'cumY',
             'maxRadius', 'neighborBA', 'nbYear', 'isRecruit')
fec_dt <- fec[, setdiff(names(fec), colToRm), with = FALSE]



# Transfor data table in a transition form
# nbRecruit from year0 to year1
fec_dt[, year0 := year]
fec_dt[, year := NULL]


transition_f <- function(year0) {

    uqYear <- unique(year0)
    
    year1 <- numeric()
    for(y in 1:(length(uqYear) - 1)) {
        year1 <- append(year1, rep(uqYear[y + 1], sum(year0 == uqYear[y])))
    }

    year1 <- append(year1, rep(NA, sum(year0 == uqYear[length(uqYear)])))
    
    return( year1 )
}

fec_dt[, year1 := transition_f(year0), by = plot_id]
fec_dt[, deltaYear := year1 - year0]



# as number of recruitment depends on plot level conditions from last time step
# shift nbRecruit by a one time step
fec_dt[, nbRecruit_lg := shift(nbRecruit, fill = NA, type = 'lead'), by = .(plot_id, subplot_id, spCode)]


# drop first year of measurement as now table is a transition way
fec_dt = fec_dt[!is.na(deltaYear), ]


# save
saveRDS(fec_dt, 'data/RESEF/ingrowth_dt.RDS')





# ## QUICK RANDOM FOREST
# toKeep <- c('spCode', 'ecoregion', 'slope', 'drai', 'humus', 'soil', 'densityCover', 'value0_bio_01', 'value0_bio_12', 'BA', 'relativeBA_sp', 'nbRecYear')
# spIds <- fec_dt[, sum(nbRecruit > 0), by = spCode][V1 > 10, spCode]
# rsquared <- data.table()
# for(sp in spIds) { 
#     rf = ranger(nbRecYear ~ ., na.omit(fec_dt[spCode == sp, -c(1, 2, 4, 78, 81:85), with = FALSE]))
#     rf_imp = ranger(nbRecYear ~ ., na.omit(fec_dt[spCode == sp, -c(1, 2, 4, 78, 81:85), with = FALSE]), importance = 'impurity_corrected')
#     imp <- sort(importance(rf_imp)/max(importance(rf_imp)), decreasing = T)
    
#     rfb = ranger(nbRecYear ~ ., na.omit(fec_dt[spCode == sp, toKeep, with = FALSE]))
#     rf_impb = ranger(nbRecYear ~ ., na.omit(fec_dt[spCode == sp, toKeep, with = FALSE]), importance = 'impurity_corrected')
#     impb <- sort(importance(rf_impb)/max(importance(rf_impb)), decreasing = T)
    
#     rsquared <- rbind(
#         rsquared,
#         data.frame(
#             spCode = sp,
#             n = rf$num.samples,
#             MSE = rf$prediction.error,
#             rs = rf$r.squared, 
#             imp = paste(
#                     paste0(
#                         names(imp)[1:2],
#                         paste0(' (', round(imp[1:2], 1), ')')
#                     ),
#                     collapse = '\n'
#                 ),
#             MSEb = rfb$prediction.error,
#             rsb = rfb$r.squared, 
#             impb = paste(
#                     paste0(
#                         names(impb)[1:2],
#                         paste0(' (', round(impb[1:2], 1), ')')
#                     ),
#                     collapse = '\n'
#                 )
#         )
#     )
# }

# rsquared <- merge(rsquared, fec_dt[spCode %in% spIds, sum(nbRecruit > 0), by = spCode], by = 'spCode')
# rsquared[, obsProp := V1/n]

# rsquared %>%
#     ggplot(aes(n, rsb, color = MSEb, label = impb)) +
#     geom_point(size = 3) +
#     geom_text(vjust = -.5)




# plot(rs ~ rs_lg, rsquared, xlim = axisLim, ylim = axisLim, pch = '', xlab = 'R2 (lag)', ylab = 'R2')
# abline(0, 1, col = rgb(0, 0, 0, 0.3))
# text(
#     rsquared$rs_lg, rsquared$rs,
#     labels = paste0(rsquared$sp, ' (', rsquared$n, ')'),
#     cex = 0.65,
#     col = viridis::viridis(nrow(rsquared))[rank(rsquared$n)]
# )
# mtext('Laged recruitment is better? (color if function of sample size)', 3, at = axisLim[1] + (axisLim[2]-axisLim[1])/3)



