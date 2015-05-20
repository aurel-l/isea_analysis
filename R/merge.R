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
        admix = '',
        batch_param = '',
        output = ''
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

    args = parser$parse_args()

    if (args$batch_param == '') {
        # guesses the name of the param file if not provided
        args$batch_param = sub('.txt', '.batch_param_map.txt', args$admix)
    }
}

# parses files
imports = list(
    admix = read.csv(args$admix)[
        ,
        c('run', 'Label', 'DemeSize', variables$summaryNames)
    ],
    param = read.csv(args$batch_param)
)

# # gets list of runs containing NA values
# failedRuns = unique(
#     imports$admix[complete.cases(imports$admix[, variables$summaryNames]), ]
# )
# if (variables$debug | args$output != '') {
#     cat(paste('removing', length(failedRuns), 'because of NaNs'))
# }
#
# # removes failed runs
# success = imports$admix[imports$admix$run %in% failedRuns, ]
#
# # gets list of runs not accepted (not realistic population sizes)
# failedRuns = c()
# for (r in unique(success$run)) {
#     simu = success[success$run == r]
#     if (max(simu$DemeSize) * variables$toleratedPopRatio > min(simu$DemeSize)) {
#         failedRuns = c(failedRuns, r)
#     }
# }
# if (variables$debug | args$output != '') {
#     cat(paste('removing', length(failedRuns), 'because of too low population sizes'))
# }
#
# # removes failed runs
# success = success[success %in% failedRuns, ]

# adds island information from label and drops label information
imports$admix['Island'] = lapply(
    imports$admix['Label'],
    function(x) sub('\\d+(_src)?', '', x)
)
imports$admix = subset(
    imports$admix,
    select = -c(Label)
)

uniqueIslands = unique(imports$admix$Island)

# lists failed runs
failedRuns = c()
for (r in unique(imports$admix$run)) {
    simu = imports$admix[imports$admix$run == r, ]
    max = max(simu$DemeSize)
    for (i in uniqueIslands) {
        empty = simu[simu$Island == i, 'DemeSize'] < variables$toleratedPopRatio
        # if too many empty (or nearly empty) demes on an island
        if (length(empty) * variables$toleratedDeadDemes <= sum(empty)) {
            failedRuns = c(failedRuns, r)
            break
        }
    }
}
# removes failed runs
if (variables$debug | args$output != '') {
    cat(paste('removing', length(failedRuns), 'failed simulations\n'))
}
success = imports$admix[!imports$admix$run %in% failedRuns, ]

# aggregates by island
aggregated = aggregate(
    . ~ Island + run,
    data = success,
    FUN = mean
)
# changes simple means to weighted means
aggregated[, variables$summaryNames] = do.call(
    'rbind',
    as.list(
        by(
            success,
            success[, c('Island', 'run')],
            function(x) {
                colWeightedMeans(x[, variables$summaryNames], x$DemeSize)
            }
        )
    )
)

# joins admix and param values
merged = merge(
    imports$param,
    aggregated
)

# writes as csv file to chosen output
write.csv(
    merged,
    file = args$output,
    row.names = FALSE
)
