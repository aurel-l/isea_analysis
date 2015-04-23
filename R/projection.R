#!/usr/bin/Rscript

#variables
variables.modelfile = '../Data/isea_admixture_data.csv'
variables.infile = '../Data/isea_admixture_data_for_comparison_2.csv'
variables.outfile = '../Data/order.csv'
variables.original = c('longitude', 'latitude')
variables.group = 'all_admixture'
variables.key = 'code'

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.infile = arguments[1]
    if (length(arguments) >= 2) {
        variables.modelfile = arguments[2]
        if (length(arguments) >= 3) {
            variables.outfile = arguments[3]
        }
    }
}

#load data
imports.model = read.csv(variables.modelfile)[c(variables.original, variables.group)]
imports.in = read.csv(variables.infile)[c(variables.original, variables.key)]

#thresholding
groups = vector(mode = 'list', length = 2)
groups[[1]] = imports.model[imports.model$all_admixture > 0.9,]
groups[[2]] = imports.model[imports.model$all_admixture < 0.2,]

#Fisher
covs = vector(mode = 'list', length = 2)
covs[[1]] = (dim(groups[[1]])[1] - 1) * cov(groups[[1]][variables.original])
covs[[2]] = (dim(groups[[2]])[1] - 1) * cov(groups[[2]][variables.original])

covInv = solve(covs[[1]] + covs[[2]])

v = covInv %*% colMeans(groups[[1]][variables.original]) - colMeans(groups[[2]][variables.original])

order = t(t(v) %*% t(imports.in[variables.original]))

output = imports.in[variables.key]
#normalise
output$order = 1 - (order - min(order)) / (max(order) - min(order))

#other locations (hardcoded)
output$order[output$code %in% c('Aru', 'Sera', 'Tani')] = 2
output = rbind(output, data.frame(code = c('Laos', 'Thai'), order = c(-1.0, -1.0)))

#order
output = output[order(output$order),]

#file output
write.csv(output, variables.outfile, row.names = FALSE, quote = FALSE)
