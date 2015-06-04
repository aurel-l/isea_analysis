#!/usr/bin/Rscript

# source script that is common to all scripts
tryCatch({
    # look for common.R in the same folder than the script
    source(paste0(
        dirname(sub('--file=','',commandArgs(trailingOnly=F)[grep('--file=',commandArgs(trailingOnly=F))])),
        '/common.R'
    ))
}, warning = function(a) {
    # when not run from CLI, assumes common.R is in the working directory
    source('common.R')
})
suppressMessages(library(lattice))
suppressMessages(library(vegan))
suppressMessages(library(SpatialTools))

# variables overwrite
variables$debug = TRUE

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        admix = '../test/test-poisson-marriage-2.csv',
        repeated = TRUE,
        verbose = TRUE
    )
} else {
    #CLI arguments
    parser = ArgumentParser(
        description = 'Analyse the sensitivity of a set of parameters with one changing parameter'
    )

    parser$add_argument(
        'order', type = 'character', help = 'path to the order file'
    )
    parser$add_argument(
        'real', type = 'character', help = 'path to the real admixture data file'
    )
    parser$add_argument(
        'admix', type = 'character', nargs = '?', default = 'stdin',
        help = 'path to the admix file, defaults to stdin'
    )
    parser$add_argument(
        '--repeated', '-r', action='store_true',
        help = 'display repeated randomSeeds'
    )
    parser$add_argument(
        '--verbose', '-v', action='store_true',
        help = 'verbose mode'
    )

    args = parser$parse_args()
}

if (variables$debug) {
    cat('executing in debug mode, ignoring any command line argument\n')
}

data = new.env()
data$conn = file(args$admix, open = 'r')
data$header = strsplit(gsub('"', '', readLines(data$conn, 1L)), ',')[[1]]
data$csvTypes = sapply(
    data$header,
    function(x) variables$types[[x]],
    USE.NAMES = FALSE
)

ref = new.env()
ref$order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')]
ref$real = read.csv(args$real)[c('Island', 'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture')]
# remove islands without data
ref$real = ref$real[!is.na(ref$real$AutosomeAdmixture), ]
# order island with order information
ref$real = ref$real[order(ref$order$order), ]

variables$comparedIslands = ref$real[, 'Island']

variables$firstLoop = TRUE

# environment to store summary information
summary = new.env()
# sensitivities by Islands
summary$sensit = list(
    all = data.frame(),
    aggr = data.frame()
)
# counts of unique parameter sets
summary$counts = list(
    df = data.frame(),
    changing = rep.int(1L, length(variables$paramNames))
)
names(summary$counts$changing) = variables$paramNames

loop = new.env()
loop$counter = 0L
loop$changing = -1L
loop$type

if (args$repeated) {
    randomSeeds = integer(0L)
}
repeat {
    loop$counter = loop$counter + 1L

    data$buffer = readLines(data$conn, 21L)

    # exits the loop if the file has no more line to read
    if (length(data$buffer) == 0L) {
        close(data$conn)
        break
    }

    # parses the raw text as csv to a data frame
    csvConn = textConnection(data$buffer)
    data$df = read.csv(
        csvConn,
        colClasses = data$csvTypes,
        header = FALSE,
        col.names = data$header
    )
    close(csvConn)

    if (args$repeated) {
        randomSeeds = append(randomSeeds, data$df$randomSeed[1L])
    }

    summary$sensit$all = addToSensit(
        summary$sensit$all,
        data$df[, c('Island', variables$summaryNames)]
    )

    summary$counts = addToCounts(
        summary$counts,
        data$df[1, variables$paramNames],
        variables$paramNames
    )
    tmp = sum(summary$counts$changing != 1)
    if (loop$changing != tmp) {
        loop$changing = tmp
        loop$type = analysisType(loop$changing)
    }




    if (args$verbose) {
        cat('\r', paste(
            'Processed simulations:', loop$counter,
            '- Changing parameters:', loop$changing,
            '- Unique parameter sets:', nrow(summary$counts$df)
        ))
        flush.console()
    }
}
if (args$verbose) {
    cat('\n')
    flush.console()
}

if (args$repeated) {
    tab = table(randomSeeds)
    if (min(tab) != max(tab)) {
        cat('repeated randomSeeds:\n')
        print(names(tab[tab == max(tab)]))
    }
}

if (args$verbose) {
    cat('now performing', loop$type, 'analysis')
}

maxCount = max(summary$counts$df$count)

summary$sensit$all$Island = factor(
    summary$sensit$all$Island,
    unique(ref$order$Island)
)
summary$sensit$aggr = aggregate(
    . ~ Island + variable,
    data = summary$sensit$all,
    FUN = sd
)

if (loop$type == 'stability') {
    sourced = 'analysis-stability.R'
} else {
    changing = names(
        summary$counts$changing[order(-summary$counts$changing)]
    )[1:loop$changing]
    for (p in changing) {
        summary$counts$df[, p] = factor(summary$counts$df[, p])
    }
    if (loop$changing == 1) {
        sourced = 'analysis-sensitivity.R'
    } else {
        sourced = 'analysis-sensitivity2D.R'
    }
}

# source next analysis script
tryCatch({
    # look for script in the same folder than the current script
    source(paste0(
        dirname(sub('--file=','',commandArgs(trailingOnly=F)[grep('--file=',commandArgs(trailingOnly=F))])),
        paste0('/', sourced)
    ))
}, warning = function(a) {
    # when not run from CLI, assumes script is in the working directory
    source(sourced)
})
