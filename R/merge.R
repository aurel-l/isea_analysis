#!/usr/bin/Rscript
suppressMessages(library(argparse))

debug = FALSE

if (debug) {
    args$admix = ''
    args$batch_param = ''
    args$output = ''
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
        args$batch_param = sub(".txt", ".batch_param_map.txt", args$admix)
    }
}

# parses files
imports.admix = read.csv(args$admix)
imports.param = read.csv(args$batch_param)

# adds island information
imports.admix['Island'] = lapply(
    imports.admix['Label'],
    function(x) sub('\\d+(_src)?', '', x)
)

# aggregates by island
imports.admix = aggregate(
    .~Island+run,
    data = imports.admix,
    FUN = mean
)

# drops tick and label information
imports.admix = subset(
    imports.admix,
    select = -c(tick, Label)
)

# joins admix and param values
merged = merge(
    imports.param,
    imports.admix
)

# write as csv file to chosen output
write.csv(
    merged,
    file = args$output,
    row.names = FALSE
)
