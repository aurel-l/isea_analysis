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

# variables overwrite
#variables$debug = TRUE

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        admix = '../test/June.csv',
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

# environment to store incoming data
data = new.env()
# connection to input
data$conn = file(args$admix, open = 'r')
# vector of headers from the input
data$header = strsplit(gsub('"', '', readLines(data$conn, 1L)), ',')[[1]]
# corresponding vector of types
data$csvTypes = sapply(
    data$header,
    function(x) variables$types[[x]],
    USE.NAMES = FALSE
)

# environment to store reference data (real or observed data)
ref = new.env()
# order of the islands as defined by the order file
ref$order = read.csv(args$order)[, c('Island', 'order')]
# reference admixture values
ref$real = read.csv(args$real)[
    , c(
        'Island', 'DnaAdmixture', 'AutosomeAdmixture',
        'XChrAdmixture', 'longitude', 'latitude'
    )
]
# order island by order information
ref$real = ref$real[order(ref$order$order), ]
# vector of characters -> factor
ref$real$Island = factor(ref$real$Island, levels = ref$real$Island)
# remove islands without data from reference
ref$real = ref$real[!is.na(ref$real$AutosomeAdmixture), ]
# get interesting admixture values into a numerical matrix (for performance)
ref$realMat = matrix(
    as.numeric(unlist(ref$real[, c('AutosomeAdmixture', 'XChrAdmixture')])),
    ncol = 2
)
# create matrix of spatial distances between islands
ref$geom = SpatialTools::dist1(
    matrix(cbind(ref$real$longitude, ref$real$latitude), ncol = 2)
)
# distances of admixtures between islands in reference data
ref$dist = list(
    AutosomeAdmixture = dist(ref$real$AutosomeAdmixture),
    XChrAdmixture = dist(ref$real$XChrAdmixture)
)

# list of islands that can be compared between reference and simulated data
variables$comparedIslands = ref$real[, 'Island']

# environment to store summary information
summary = new.env()
# list management
summary$size = 0L
# sensitivities by Islands
summary$sensit = list(
    all = list(),
    aggr = data.frame()
)
# counts of unique parameter sets
summary$counts = list(
    df = data.frame(),
    changing = rep.int(1L, length(variables$paramNames))
)
names(summary$counts$changing) = variables$paramNames
# comparisons real / simulated
summary$comp = list(
    all = list(),
    aggr = data.frame()
)

# difference admixtures
summary$diff = list(
    all = list(),
    aggr = data.frame()
)

# environment to store loop information
loop = new.env()
# loop count, == simulations processed
loop$counter = 0L
# number of changing parameters
loop$changing = -1L

# if we specifically asked for this information
if (args$repeated) {
    # vector of the randomSeeds of the simulations processed
    randomSeeds = integer(0L)
}
# infinite loop, will break on EOF of input
repeat {
    # resize lists of pointers if necessary
    if (loop$counter == summary$size) {
        # new size will be the double of the old size
        summary$size = summary$size * 2L
        # reallocate memory for pointers
        length(summary$sensit$all) = summary$size
        length(summary$comp$all) = summary$size
        length(summary$diff$all) = summary$size
    }

    loop$counter = loop$counter + 1L

    # raw content of the input file, chunk of 21 lines
    # corresponds to one simulation, assumes the input is sorted by run
    data$buffer = readLines(data$conn, 21L)

    # exits the loop if the file has no more line to read
    if (length(data$buffer) == 0L) {
        close(data$conn)
        break
    }

    # opens a connection to the chunk of raw text for the simulation
    csvConn = textConnection(data$buffer)
    # parses the content of the connection as csv file into a data frame
    data$df = read.csv(
        csvConn,
        # for performance, specify types by column
        colClasses = data$csvTypes,
        header = FALSE,
        col.names = data$header
    )
    # close the connection immediately after reading it
    close(csvConn)
    # transforms the islands to factors
    data$df$Island = factor(data$df$Island, levels = ref$order$Island)
    # order the islands in the simulation as specified by the order information
    data$df = data$df[order(data$df$Island), ]
    # adds information for the difference between X and Autosome admixtures
    data$df$diffXAuto = data$df$XChrAdmixture - data$df$AutosomeAdmixture

    # if we specifically asked for this information
    if (args$repeated) {
        # adds the current randomSeed to the list of already parsed ones
        randomSeeds = append(randomSeeds, data$df$randomSeed[1L])
    }

    # melted simulation data for admixtures for every island
    summary$sensit$all[[loop$counter]] = melt(
        data$df[, c('Island', variables$summaryNames)],
        id = 'Island'
    )

    simuParams = data$df[1, variables$paramNames]

    if (nrow(summary$counts$df) == 0L) {
        summary$counts$df = simuParams
        summary$counts$df$count = 1L
    } else {
        found = FALSE
        for (i in 1L:nrow(summary$counts$df)) {
            if (all(summary$counts$df[i, variables$paramNames] == simuParams)) {
                summary$counts$df$count[i] = summary$counts$df$count[i] + 1L
                found = TRUE
            }
        }
        if (!found) {
            tmp = cbind(simuParams, 1L)
            colnames(tmp) = c(colnames(simuParams), 'count')
            summary$counts$df = rbind(summary$counts$df, tmp)
            for (p in variables$paramNames) {
                summary$counts$changing[p] = length(unique(summary$counts$df[, p]))
            }
        }
    }

    tmp = sum(summary$counts$changing != 1)
    if (loop$changing != tmp) {
        loop$changing = tmp
        loop$type = analysisType(loop$changing)
    }

    # store difference information
    summary$diff$all[[loop$counter]] = data$df[
        ,
        c('Island', variables$paramNames, 'diffXAuto')
    ]

    # subset of Islands to compare real and simulated
    data$df = data$df[data$df$Island %in% variables$comparedIslands, ]

    ## comparisons
    # admixture information of the simulation as a numerical matrix
    simu = matrix(
        as.numeric(unlist(data$df[, c('AutosomeAdmixture', 'XChrAdmixture')])),
        ncol = 2
    )
    # prepare comparison information data frame with 4 rows for comp values
    compared = data$df[1:4, variables$paramNames]
    # every combination of admixture and comparison value, 4 values
    compared$admixture = rep(c('AutosomeAdmixture', 'XChrAdmixture'), 2)
    compared$comparison = rep(c('MSD', 'cor'), each = 2)
    # mean squared distance (numeric(2)), both admixtures in the same operation
    msd = colMeans((ref$realMat - simu) ^ 2)

    # partial Mantel correlation (numeric(2))
    cor = c(
        # Autosome admixture
        mantel.partial(
            ref$dist$AutosomeAdmixture,
            dist(simu[, 1]),
            ref$geom,
            permutations = 1L
        )$statistic,
        # X Chromosome Admixture
        mantel.partial(
            ref$dist$XChrAdmixture,
            dist(simu[, 2]),
            ref$geom,
            permutations = 1L
        )$statistic
    )
    # add comparison information
    compared$value = c(msd, cor)
    # store comparison information
    summary$comp$all[[loop$counter]] = compared

    # update displayed information if verbose mode is activated
    if (args$verbose) {
        cat('\r', paste(
            'Simulations:', loop$counter,
            '- changing params:', loop$changing,
            '- analysis:', loop$type,
            '- sets:', nrow(summary$counts$df)
        ))
        flush.console()
    }
}
if (args$verbose) {
    # new line so that the next print won't be added after the progress info
    cat('\n')
    flush.console()
}

if (args$repeated) {
    # count the occurences of every
    tab = table(randomSeeds)
    # extract any repeated randomSeed
    repeated = tab[tab != 1]
    # if the extract as a length, with have repeated seeds
    if (length(repeated)) {
        # display them
        cat('repeated randomSeeds:\n')
        cat(names(repeated), sep = ', ')
        cat('\n')
    }
}

# parameter sweep information
XMLFile = toXMLFile(
    subset(summary$counts$df, select = -count),
    prod(summary$counts$changing)
)
# if in debug mode
if (variables$debug) {
    # only display sweep information
    print(XMLFile)
} else {
    # otherwise, only save it in a file
    invisible(saveXML(XMLFile, paste0(variables$now, '-parameters.xml')))
}

if (args$verbose) {
    cat('now aggregating all the data\n')
}

# concatenate at once all the list filled earlier
# 3 lists of pointers to a lot of small memory blocks -> 3 big memory blocks
summary$sensit$all = do.call('rbind', summary$sensit$all)
summary$comp$all = do.call('rbind', summary$comp$all)
summary$diff$all = do.call('rbind', summary$diff$all)
# since we don't have references to the lists of pointers, ask R to gc them
invisible(gc())

if (args$verbose) {
    cat('now performing', loop$type, 'analysis\n')
}

maxCount = max(summary$counts$df$count)

if (loop$type == 'stability') {
    sourced = 'analysis-stability.R'
    changing = c()
} else {
    changing = names(
        summary$counts$changing[order(-summary$counts$changing)]
    )[1:loop$changing]
    for (p in changing) {
        summary$counts$df[, p] = factor(summary$counts$df[, p])
        summary$comp$all[, p] = factor(summary$comp$all[, p])
        summary$diff$all[, p] = factor(summary$diff$all[, p])
    }
    summary$sensit$aggr = aggregate(
        . ~ Island + variable,
        data = summary$sensit$all,
        FUN = sd
    )
    if (loop$changing == 1) {
        sourced = 'analysis-sensitivity.R'
    } else {
        sourced = 'analysis-sensitivity2D.R'
    }
}

summary$comp$all = summary$comp$all[, c(changing, 'admixture', 'comparison', 'value')]
summary$diff$all = summary$diff$all[, c(changing, 'Island', 'diffXAuto')]
if (loop$changing == 2) {
    summary$comp$aggr = aggregate(
        value ~ .,
        data = summary$comp$all,
        FUN = mean
    )
    summary$comp$aggr$sd = aggregate(
        value ~ .,
        data = summary$comp$all,
        FUN = sd
    )$value
    for (p in changing) {
        summary$comp$aggr[, p] = factor(summary$comp$aggr[, p])
    }
} else if(loop$changing == 1) {
    summary$diff$aggr = aggregate(
        as.formula(paste('diffXAuto ~ Island +', changing)),
        data = summary$diff$all,
        FUN = mean
    )
    summary$diff$aggr$stddev = aggregate(
        as.formula(paste('diffXAuto ~ Island +', changing)),
        data = summary$diff$all,
        FUN = sd
    )$diffXAuto
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
