#!/usr/bin/Rscript

# source script that is common to all scripts
tryCatch({
    # look for common.R in the same folder than the script
    source(paste0(
        dirname(sub('--file=','',commandArgs(trailingOnly=F)[
            grep('--file=',commandArgs(trailingOnly=F))
        ])),
        '/common.R'
    ))
}, warning = function(a) {
    # when not run from CLI, assumes common.R is in the working directory
    source('common.R')
})
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(vegan))# !high memory use! (needed for mantel.partial)

# variables overwrite
#variables$debug = TRUE

if (variables$debug) {
    # change parameters here if debugging
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        admix = '../2015_08_24/merged.dat',
        ABC = 'threshold, 0.1 0.05 0.01',
        repeated = TRUE,
        verbose = TRUE
    )
} else {
    #CLI arguments
    parser = ArgumentParser(
        description = 'Analyse of admixture files'
    )

    parser$add_argument(
        'order', type = 'character', help = 'path to the order file'
    )
    parser$add_argument(
        'real', type = 'character',
        help = 'path to the real admixture data file'
    )
    parser$add_argument(
        'admix', type = 'character', nargs = '?', default = 'stdin',
        help = 'path to the admix file, defaults to stdin'
    )
    parser$add_argument(
        '--ABC', '-a', nargs = '?',
        help = paste(
            'perform an ABC analysis, enter the type of tolerance',
            '(absolute or relative) and the list of tolerance values'
        )
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
    cat('executing in DEBUG mode, ignoring any command line argument\n')
}

if (!is.null(args$ABC)) {
    parsed = strsplit(args$ABC, ' *[ ,] *')[[1]]
    variables$ABC = list(
        type = parsed[1],
        tVector = as.numeric(tail(parsed, -1))
    )
    if (!(variables$ABC$type %in% c('threshold', 'tolerance'))) {
        stop(paste(
            variables$ABC$type, 'is not valid (try "tolerance" or "threshold")'
        ))
    }
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
if (is.null(args$ABC)) {
    # counts of unique parameter sets
    summary$counts = list(
        df = data.frame(),
        changing = rep.int(1L, length(variables$paramNames))
    )
    names(summary$counts$changing) = variables$paramNames
}
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
        if (summary$size == 0) {
            summary$size = 1L
        } else {
            summary$size = summary$size * 2L
        }
        # reallocate memory for pointers
        length(summary$sensit$all) = summary$size
        length(summary$comp$all) = summary$size
        length(summary$diff$all) = summary$size
    }

    loop$counter = loop$counter + 1L

    # raw content of the input file, chunk of 21 lines
    # corresponds to one simulation, assumes the input is sorted by run
    data$buffer = readLines(data$conn, variables$nIslands)

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
    # closes the connection immediately after reading it
    close(csvConn)
    # checks that the block of data only contains rows from 1 single simulation
    if (length(unique(data$df$run)) > 1) {
        stop('Uh-oh, this block contains rows from more than one simulation')
    }
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

    if (is.null(args$ABC)) {
        # melted simulation data for admixtures for every island
        summary$sensit$all[[loop$counter]] = melt(
            data$df[, c('Island', variables$summaryNames)],
            id = 'Island'
        )
    }

    simuParams = data$df[1, variables$paramNames]

    # only for grid searches
    if (is.null(args$ABC)) {
        # if not already initialised, creates a data frame for the parameters
        if (nrow(summary$counts$df) == 0L) {
            summary$counts$df = simuParams
            summary$counts$df = data.frame(
                lapply(summary$counts$df, as.character),
                stringsAsFactors = FALSE
            )
            summary$counts$df$count = 1L
        } else {
            found = FALSE
            # tries to find an already corresponding parameter set
            for (i in 1L:nrow(summary$counts$df)) {
                test = summary$counts$df[i, variables$paramNames] == simuParams
                if (all(test)) {
                    summary$counts$df$count[i] = summary$counts$df$count[i] + 1
                    found = TRUE
                }
            }
            if (!found) {
                # if not, adds this set to the end of the data frame of sets
                tmp = cbind(simuParams, 1L)
                colnames(tmp) = c(colnames(simuParams), 'count')
                summary$counts$df = rbind(summary$counts$df, tmp)
                for (p in variables$paramNames) {
                    summary$counts$changing[p] =
                        length(unique(summary$counts$df[, p]))
                }
            }
        }

        tmp = sum(summary$counts$changing != 1)
        if (loop$changing != tmp) {
            loop$changing = tmp
            if (loop$changing == 0) {
                loop$type = 'stability'
            } else if (loop$changing == 1) {
                loop$type = 'sensitivity'
            } else if (loop$changing == 2) {
                loop$type = 'sensitivity'
            } else {
                stop(paste(
                    'cannot perform analysis on', loop$changing,
                    'changing parameters'
                ))
            }
        }
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
    if (!is.null(args$ABC)) {
        # prepare comparison information data frame with 1 rows for comp values
        compared = data$df[1, variables$paramNames]
    } else {
        # prepare comparison information data frame with 4 rows for comp values
        compared = data$df[1:4, variables$paramNames]
        # every combination of admixture and comparison value, 4 values
        compared$admixture = rep(c('AutosomeAdmixture', 'XChrAdmixture'), 2)
        compared$comparison = rep(c('MSD', 'cor'), each = 2)
    }
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
    if (!is.null(args$ABC)) {
        compared[, variables$ABCsummary] =
            c(msd, cor)
        compared$randomSeed = data$df$randomSeed[1]
    } else {
        compared$value = c(msd, cor)
    }
    # store comparison information
    summary$comp$all[[loop$counter]] = compared

    # update displayed information if verbose mode is activated
    if (args$verbose) {
        if (!is.null(args$ABC)) {
            text = '- analysis: ABC'
        } else {
            text = paste(
                '- changing params:', loop$changing,
                '- analysis:', loop$type,
                '- sets:', nrow(summary$counts$df)
            )
        }
        cat('\r', paste('Simulations:', loop$counter, text))
        flush.console()
    }
}
# end of main loop

if (args$verbose) {
    # new line so that the next print won't be added after the progress info
    cat('\n')
    flush.console()
}

if (args$repeated) {
    # count the occurences of every randomSeed
    tab = table(randomSeeds)
    # extract any repeated randomSeed
    repeated = tab[tab != 1]
    # if the vector of repeated randomSeeds has values
    if (length(repeated)) {
        # display them
        cat('repeated randomSeeds:\n')
        cat(names(repeated), sep = ', ')
        cat('\n')
    }
}

# only for grid searches
if (is.null(args$ABC)) {
    # aggregates parameter sweep information...

    suppressMessages(library(XML))
    # ...in this XMLTree
    XMLTree = newXMLNode('sweep')
    # loops on every parameter
    for (p in colnames(summary$counts$df)) {
        if (p == 'count') {
            # this column doesn't need to be in the XML (not a parameter)
            next
        }
        uniqueValues = sort(unique(summary$counts$df[, p]))
        # adds a node in the XML tree
        newXMLNode(
            'parameter',
            attrs = c(
                'name' = p,
                'n' = length(uniqueValues),
                'values' = toString(uniqueValues)
            ),
            parent = XMLTree
        )
    }
    # attribute on the root (number of distinct sets of parameters)
    addAttributes(XMLTree, sets = prod(summary$counts$changing))

    # if in debug mode
    if (variables$debug) {
        # only displays sweep information
        print(XMLTree)
    } else {
        # otherwise, only saves it in a file
        invisible(saveXML(XMLTree, paste0(variables$now, '-parameters.xml')))
    }
}

if (args$verbose) {
    cat('now aggregating all the data\n')
}

# concatenates at once all the list filled earlier and gc them
# 3 lists of pointers to a lot of small memory blocks -> 3 big memory blocks
if (is.null(args$ABC)) {
    summary$sensit$all = do.call('rbind', summary$sensit$all)
    invisible(gc())
}
summary$comp$all = do.call('rbind', summary$comp$all)
invisible(gc())
if (is.null(args$ABC)) {
    summary$diff$all = do.call('rbind', summary$diff$all)
    invisible(gc())
}

if (args$verbose) {
    cat(
        'now performing',
        if (is.null(args$ABC)) loop$type else 'ABC', 'analysis\n'
    )
}

# prepares the data for the visualisation, according to the type of analysis
if (!is.null(args$ABC)) {
    sourced = 'analysis-ABC.R'
} else {
    # grid search
    maxCount = max(summary$counts$df$count)
    if (loop$type == 'stability') {
        # stability
        sourced = 'analysis-stability.R'
        changing = c()
    } else {
        # sensitivity
        changing = names(
            summary$counts$changing[order(-summary$counts$changing)]
        )[1:loop$changing]
        for (p in changing) {
            summary$counts$df[, p] = factor(summary$counts$df[, p])
            summary$comp$all[, p] = factor(summary$comp$all[, p])
            summary$diff$all[, p] = factor(summary$diff$all[, p])
        }
        # standard deviation among all of the data
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

    # drops the unnecessary columns
    if (is.null(args$ABC)) {# not ABC
        summary$comp$all = summary$comp$all[
            ,
            c(changing, 'admixture', 'comparison', 'value')
        ]
    } else {# ABC
        summary$comp$all = summary$comp$all[
            ,
            c(changing, 'admixture', 'comparison', 'value', 'randomSeed')
        ]
    }
    summary$diff$all = summary$diff$all[, c(changing, 'Island', 'diffXAuto')]

    if (loop$changing == 2) {
        # 2d parameter sweep
        # comparisons, mean value (first heat-maps)
        summary$comp$aggr = aggregate(
            value ~ .,
            data = summary$comp$all,
            FUN = mean
        )
        # comparisons, standard deviation value (last heat-maps)
        # value added as a 'stddev' column on the previous 'aggr' data frame
        summary$comp$aggr$stddev = aggregate(
            value ~ .,
            data = summary$comp$all,
            FUN = sd
        )$value
        for (p in changing) {
            summary$comp$aggr[, p] = factor(summary$comp$aggr[, p])
        }
    } else if(loop$changing == 1) {
        # 1d parameter sweep
        # X - Auto, mean value (dot in the plot)
        summary$diff$aggr = aggregate(
            as.formula(paste('diffXAuto ~ Island +', changing)),
            data = summary$diff$all,
            FUN = mean
        )
        # X - Auto, standard deviation value (error bar in the plot)
        # value added as a 'stddev' column on the previous 'aggr' data frame
        summary$diff$aggr$stddev = aggregate(
            as.formula(paste('diffXAuto ~ Island +', changing)),
            data = summary$diff$all,
            FUN = sd
        )$diffXAuto
    }
}

# source next analysis script that does only the visualisation
tryCatch({
    # look for script in the same folder than the current script
    source(paste0(
        dirname(sub('--file=','',commandArgs(trailingOnly=F)[
            grep('--file=',commandArgs(trailingOnly=F))
        ])),
        paste0('/', sourced)
    ))
}, warning = function(a) {
    # when not run from CLI, assumes script is in the working directory
    source(sourced)
})
