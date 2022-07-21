###############################
# Download FIA db files and create RDS files
# Will Vieira
# July 2, 2022
##############################



##############################
# Steps
#   - Define US states we will be using
#   - Download db zip files
#   - Unzip and extract tables:
#       - 'TREE', 'PLOT', 'COND', 'SUBPLOT', 'SUBP_COND', and 'SURVEY' 
#   - download species referene name
##############################


# Part of the code adapted from https://github.com/hunter-stanke/rFIA/issues/29#issuecomment-1122353192


library(RSQLite)
library(data.table)


# Define states
states <- c(
    'ME', 'MA', 'RI', 'CT', 'NJ', 'DE', 'MD',
    'NH', 'VT', 'NY', 'PA', 'VA', 'NC', 'SC', 'GA', 'FL',
    'MI', 'OH', 'IN', 'WV', 'KY', 'TN', 'AL' , 'MS',
    'WI', 'IL', 'MN', 'IA', 'MO', 'AR', 'LA',
    'ND', 'SD', 'NE', 'KS', 'OK', 'TX'
)

# Download db files
##############################
for(st in states[6:length(states)])
{
    cat('Downloading state:', st, '\n')

    adress <- paste0(
        'https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_',
        st,
        '.zip'
    )

    try(
        download.file(
            url = adress,
            destfile = file.path(
                'rawData',
                'FIA',
                paste0('SQLite_FIADB_', st, '.zip')
            )
        )
    )
}



# unzip and transfer db tables to csv
##############################

# create folder
basePath <- file.path('rawData', 'FIA', 'csv')
dir.create(basePath)

for(st in states)
{
    # unzip db
    system(
        paste0(
            'unzip ',
            file.path('rawData', 'FIA', paste0('SQLite_FIADB_', st, '.zip')),
            ' -d ',
            basePath
        )
    )

    # connect to db
    con <- dbConnect(
            SQLite(), 
            file.path(basePath, paste0('FIADB_', st, '.db'))
    )

    # grab all the table names
    # - alternatively, subset this to only those tables you want
    #   e.g. `db_table_names <- c("SURVEY", "PLOT")`
    db_table_names <- c(
        'TREE', 'PLOT', 'COND',
        'SUBPLOT', 'SUBP_COND', 'SURVEY'
    )
    db_table_names = 'SUBP_COND'

    # iterate through the table names and write out a csv for each table in the db
    invisible(
        lapply(db_table_names,
            function(x)
                write.csv(
                    dbReadTable(x, conn = con),
                    file = file.path(basePath, paste0(st, '_', x, '.csv'))
                )
        )
    )

    # close connection
    dbDisconnect(con)

    # Delete unzip file
    file.remove(file.path(basePath, paste0('FIADB_', st, '.db')))

}



# Species reference name
##############################

for(refFile in c('REF_SPECIES.zip', 'REF_SPECIES_GROUP.zip'))
{
    download.file(
        url = paste0(
            'https://apps.fs.usda.gov/fia/datamart/CSV/',
            refFile
        ),
        destfile = paste0(
            'rawData/FIA/',
            refFile
        )
    )

    unzip(
        zipfile = paste0('rawData/FIA/', refFile),
        exdir = 'rawData/FIA/'
    )

    file.remove(
        paste0('rawData/FIA/', refFile)
    )
}
