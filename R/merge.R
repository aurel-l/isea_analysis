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

# variables overwrite
#variables$debug = TRUE

if (variables$debug) {
    # change parameters here if debugging
    args = list(
        admix = '../../isea_new_new_new/isea/output/admixturebynode.2015.Jun.09.01_50_10.txt',
        batch_param = '',
        output = '',
        progress = TRUE,
        failed = TRUE
    )
} else {
    #CLI arguments
    parser = ArgumentParser(
        description = 'Merge admix and batch_param files into a DB-like output'
    )

    parser$add_argument(
        'admix', type = 'character', help = 'path to the admix file'
    )
    parser$add_argument(
        'batch_param', type = 'character', nargs = '?', default = '',
        help = paste(
            'path to the batch_param file, tries to find it',
            'in the same folder than admix file if not provided'
        )
    )
    parser$add_argument(
        'output', type = 'character', nargs = '?', default = '',
        help = 'output file, if not provided defaults to stdout'
    )
    parser$add_argument(
        '--progress', '-p', action='store_true',
        help = paste(
            'display progress bar, to stdout if not used by output,',
            'otherwise to stderr'
        )
    )
    parser$add_argument(
        '--failed', '-f', action='store_true',
        help = 'display failed runs to stderr'
    )

    args = parser$parse_args()
}

if (args$batch_param == '') {
    # guesses the name of the param file if not provided
    args$batch_param = sub('.txt', '.batch_param_map.txt', args$admix)
}

if (args$progress) {
    variables$nSimulations = as.integer(
        system(
            paste('wc -l', args$batch_param, '| cut -f1 -d" "'),
            intern = TRUE
        )
    )
}

# opens up the connections to the input files
admix = new.env()
admix$conn = file(args$admix, open = 'r')
param = new.env()
param$conn = file(args$batch_param, open = 'r')

# extracts the header of the input files
admix$header = strsplit(gsub('"', '', readLines(admix$conn, 1L)), ',')[[1L]]
param$header = strsplit(gsub('"', '', readLines(param$conn, 1L)), ',')[[1L]]

# retrieves the types of the columns according to the variable name
# types hardcoded in common.R
admix$csvTypes = sapply(
    admix$header,
    function(x) variables$types[[x]],
    USE.NAMES = FALSE
)
param$csvTypes = sapply(
    param$header,
    function(x) variables$types[[x]],
    USE.NAMES = FALSE
)

# subset of useful variables
admix$csvSubset = c('run', 'Label', variables$summaryNames, 'DemeSize')
param$csvSubset = c('run', 'randomSeed', variables$paramNames)

variables$firstLoop = TRUE

if (args$failed) {
    # vector of failed runs
    failedRuns = c()
}
if (args$progress) {
    # starts the progress bar
    pb = txtProgressBar(
        0L, variables$nSimulations, style = 3L,
        file = if(args$output == '') stderr() else ''
    )
}
counter = 0L

# loops on every simulation
repeat {
    counter = counter + 1L
    if (args$progress) {
        # updates the progress bar
        setTxtProgressBar(pb, counter)
    }

    # reads the corresponding lines
    admix$buffer = readLines(admix$conn, variables$nDemes)
    param$buffer = readLines(param$conn, 1L)

    # exits the loop if one of the files has no more line to read
    if (length(admix$buffer) == 0L | length(param$buffer) == 0L) {
        close(admix$conn)
        close(param$conn)
        break
    }

    # parses the raw text as csv to a data frame
    csvConn = textConnection(admix$buffer)
    admix$df = read.csv(
        csvConn,
        colClasses = admix$csvTypes,
        header = FALSE,
        col.names = admix$header
    )[, admix$csvSubset]
    close(csvConn)

    # adds island information from label
    admix$df['Island'] = lapply(
        # from the Label
        admix$df['Label'],
        # extracts the prefix (removes everything starting at the first digit)
        function(x) sub('\\d+(_src)?', '', x)
    )

    if (variables$firstLoop) {
        # gets a list of islands (only once, at the first run)
        variables$islands = unique(admix$df$Island)
    }
    # let's check if this simulation has failed

    # maximum population in the whole simulation
    max = max(admix$df$DemeSize)
    # minimum corresponding size for a deme to be considered alive
    minToleratedSize = variables$toleratedPopRatio * max
    failed = FALSE
    for (i in variables$islands) {
        # vector of booleans, TRUE for 'empty' demes
        empty = (
            admix$df[admix$df$Island == i, 'DemeSize'] < minToleratedSize
        )
        # if too many 'empty' demes on any island
        if(length(empty) * variables$toleratedDeadDemes <= sum(empty)) {
            # this run has failed
            failed = TRUE
            break
        }
    }
    if (failed) {
        if (args$failed) {
            # adds this run to the list of failed runs
            failedRuns = c(failedRuns, admix$df$run[1L])
        }
        # jumps to the next iteration of the loop without doing further work
        next
    }
    # arrived to this point, this run has NOT failed

    if (variables$firstLoop) {
        # creates an 'aggregated' data frame once to be re-used for every run
        aggregated = data.frame(
            run = admix$df$run[1L],
            Island = variables$islands
        )
        aggregated[, variables$summaryNames] = 0.0
    } else {
        aggregated$run = admix$df$run[1L]
    }
    # fills the 'aggregated' object for every island with data from demes
    for (i in 1:length(variables$islands)) {
        # gets the demes corresponding to that island
        demesOfIsland = admix$df[
            admix$df$Island == variables$islands[i],
            variables$summaryNames
        ]
        # fills the mean values for every summary value
        aggregated[i, variables$summaryNames] = colMeans(
            demesOfIsland, na.rm = TRUE
        )
    }

    # gets parameters for this simulation
    csvConn = textConnection(param$buffer)
    param$df = read.csv(
        csvConn,
        colClasses = param$csvTypes,
        header = FALSE,
        col.names = param$header
    )[, param$csvSubset]
    close(csvConn)

    # joins admix and param values
    merged = merge(
        param$df,
        aggregated
    )

    # writes as csv file to chosen output
    write.table(
        merged,
        sep = ',',
        file = args$output,
        row.names = FALSE,
        # writes the header only the first time
        col.names = variables$firstLoop
    )

    variables$firstLoop = FALSE
}
# end of the main loop

if (args$progress) {
    # closes the progress bar
    close(pb)
}
if (length(admix$buffer) != length(param$buffer)) {
    # If the batch_param file has been volontarily trimmed,
    # there's nothing to worry about
    warning('One of the files ended before the other')
}

if (args$failed) {
    # displays number of failed runs and list of failed runs to stderr
    write(
        paste0(
            # number of failed, and total
            length(failedRuns), ' Failed runs out of ', counter - 1L,
            # percentage of failed
            ' (', sprintf(
                # format
                '%3.2f',
                # value in percents
                round(length(failedRuns) * 100L / counter, digits = 2L)
            ), '%)\n',
            # list of failed runs
            paste0(failedRuns, collapse = ', '), '\n'
        ),
        stderr()
    )
}
