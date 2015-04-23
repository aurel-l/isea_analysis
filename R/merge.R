#!/usr/bin/Rscript

# variable defaults
variables.output = '' # will redirect to stdout

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.admixFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.paramFile = arguments[2]
        if (length(arguments) >= 3) {
            variables.output = arguments[3]
        }
    } else {
        # guesses the name of the param file if not provided
        variables.paramFile = sub(".txt", ".batch_param_map.txt", variables.admixFile)
    }
}

# parses files
imports.admix = read.csv(variables.admixFile)
imports.param = read.csv(variables.paramFile)

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

#
write.csv(
    merged,
    file = variables.output,
    row.names = FALSE
)
