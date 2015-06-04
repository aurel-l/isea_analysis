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
        admix = '../../isea_new_new_new/isea/output/admixturebynode.2015.May.25.00_39_13.2.txt',
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
        help = 'path to the batch_param file, tries to find it in the same folder than admix file if not provided'
    )
    parser$add_argument(
        'output', type = 'character', nargs = '?', default = '',
        help = 'output file, if not provided defaults to stdout'
    )
    parser$add_argument(
        '--progress', '-p', action='store_true',
        help = 'display progress bar, to stdout if not used by output, otherwise to stderr'
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

admix = new.env()
admix$conn = file(args$admix, open = 'r')
param = new.env()
param$conn = file(args$batch_param, open = 'r')

admix$header = strsplit(gsub('"', '', readLines(admix$conn, 1L)), ',')[[1]]
param$header = strsplit(gsub('"', '', readLines(param$conn, 1L)), ',')[[1]]

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

#variables$hasDemeSizeInfo = grepl('DemeSize', admix$header)

admix$csvSubset = c('run', 'Label', variables$summaryNames, 'DemeSize')
#if (variables$hasDemeSizeInfo) {
#    admix$csvTypes = c(admix$csvTypes, 'integer')
#    admix$csvSubset = c(admix$csvSubset, 'DemeSize')
#}
param$csvSubset = c('run', 'randomSeed', variables$paramNames)

# define hasFailed according to deme size information (only once)
#if (variables$hasDemeSizeInfo) {
    hasFailed = hasFailedWithDemeInfo
#} else {
#    hasFailed = hasFailedWithoutDemeInfo
#}

variables$firstLoop = TRUE

if (args$failed) {
    failedRuns = c()
}
if (args$progress) {
    pb = txtProgressBar(
        0, variables$nSimulations, style = 3,
        file = if(args$output == '') stderr() else ''
    )
}
counter = 0

# loops on every simulation
repeat {
    counter = counter + 1
    if (args$progress) {
        setTxtProgressBar(pb, counter)
    }

    # reads the corresponding lines
    admix$buffer = readLines(admix$conn, 118L)
    param$buffer = readLines(param$conn, 1L)

    # exits the loop if one of the files has no more line to read
    if (length(admix$buffer) == 0L | length(param$buffer) == 0L) {
        if (length(admix$buffer) != length(param$buffer)) {
            warning('One of the files ended before the other')
        }
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
        admix$df['Label'],
        function(x) sub('\\d+(_src)?', '', x)
    )

    if (variables$firstLoop) {
        # gets a list of islands (only once)
        variables$islands = unique(admix$df$Island)
    }
    # checks if this simulation has failed
    failed = hasFailed(
        admix$df,
        popRatio = variables$toleratedPopRatio,
        deadDemes = variables$toleratedDeadDemes,
        islands = variables$islands,
        summaryNames = variables$summaryNames
    )
    if (failed) {
        if (args$failed) {
            failedRuns = c(failedRuns, admix$df$run[1])
        }
        # jumps to the next iteration of the loop without doing further work
        next
    }

    #
    #if (!variables$hasDemeSizeInfo) {
    #    admix$df$DemeSize = 1
    #}
    aggregated = demesToIslands(
        admix$df,
        summaryNames = variables$summaryNames,
        islands = variables$islands
    )

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
        col.names = variables$firstLoop
    )

    variables$firstLoop = FALSE
}
if (args$progress) {
    close(pb)
}

if (args$failed) {
    write(
        paste0(
            length(failedRuns),
            ' Failed runs (',
            sprintf('%3.2f', round(length(failedRuns) * 100 / counter, digits = 2)),
            '%)\n',
            paste0(
                lapply(failedRuns, function(x) paste0('run ', x, '\n')),
                collapse = ''
            )
        ),
        stderr()
    )
}
