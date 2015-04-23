#!/usr/bin/Rscript

#variables
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
    'MitoAdmixture', 'YChrAdmixture'
)
variables.paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile'
)
variables.infoNames = c('Label', 'Island')
#variables.admixFile = '../../isea/isea/output/admixturebynode.2015.Mar.18.20_30_01.txt'
variables.orderFile = '../Data/isea_admixture_data_for_comparison_2.csv'
variables.now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.orderFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.admixFile = arguments[2]
    } else {
        variables.admixFile = file('stdin')
    }
}

#variables.paramFile = sub('.txt', '.batch_param_map.txt', variables.admixFile)

#load data
imports.order = read.csv(variables.orderFile)[c('Island', 'order')]
imports.admix = read.csv(variables.admixFile)
#imports.param = read.csv(variables.paramFile)
nRuns = length(unique(imports.admix$run))

#check random seeds are unique
randomSeeds = imports.admix['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports.admix['Island']))[1] != dim(unique(randomSeeds))[1]) {
    stop('found a repeated random seed')
}

#adds island information
#imports.admix['Island'] = lapply(
#    imports.admix['Label'],
#    function(x) sub('\\d+(_src)?', '', x)
#)

#merge imports
#merged = merge(
#    subset(imports.admix, select = c(variables.summaryNames, variables.infoNames, 'run')),
#    subset(imports.param, select = c(variables.paramNames, 'run')),
#    by = 'run'
#)
merged = merge(
    imports.admix,
    imports.order,
    by = 'Island'
)
merged = merged[order(merged$order),]

#aggregate
summaryData.means = aggregate(
    .~Island+run,
    data = merged,
    FUN = mean
)

summaryData.SD = aggregate(
    .~Island,
    data = summaryData.means,
    FUN = sd
)
summaryData.meanSD = colMeans(
    subset(summaryData.SD, select = variables.summaryNames)
)

#plots
png(
    paste0('stability-', variables.now, '.png'),
    width = 1300, height = 1100
)
par(mfrow = c(3, 2))
boxplot(
    DnaAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('DNA Admixture by zone for', nRuns, 'runs'),
    ylab = 'DNA Admixture',
    xlab = 'Zone'
)
boxplot(
    AutosomeAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Autosome Admixture by zone for', nRuns, 'runs'),
    ylab = 'Autosome Admixture',
    xlab = 'Zone'
)
boxplot(
    XChrAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('X Chromosome Admixture by zone for', nRuns, 'runs'),
    ylab = 'X Chromosome Admixture',
    xlab = 'Zone'
)
boxplot(
    MitoAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Mitochondrial Admixture by zone for', nRuns, 'runs'),
    ylab = 'Mitochondrial Admixture',
    xlab = 'Zone'
)
boxplot(
    YChrAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Y Chromosome Admixture by zone for', nRuns, 'runs'),
    ylab = 'Y Chromosome Admixture',
    xlab = 'Zone'
)
graphics.off()
