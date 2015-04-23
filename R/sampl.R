#!/usr/bin/Rscript

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.paramFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.admixFile = arguments[2]
    } else {
        variables.admixFile = file("stdin")
    }
}

imports.param = read.csv(variables.paramFile)
imports.admix = read.csv(variables.admixFile)

#remplaces missing values by NA
imports.admix[imports.admix == -1] = NA

#adds island information
imports.admix['Island'] = lapply(
    imports.admix['Label'],
    function(x) sub('\\d+(_src)?', '', x)
)

#samples by island
final = imports.admix[0,]
#print(imports.admix)
for (run in unique(imports.admix$run)) {
    for (island in unique(imports.param$Island)) {
        extract = imports.admix[imports.admix$run == run & imports.admix$Island == island, ]
        sizeSample = imports.param$size[imports.param$Island == island]
        if (nrow(extract) < sizeSample) {
            warning(paste('not enough agents in', island, 'for run', run))
        }
        s = extract[sample(nrow(extract), sizeSample), ]
        agg = aggregate(.~Island, s, FUN = mean)
        final = rbind(final, agg)
    }
}
print(final)
