#!/usr/bin/Rscript
suppressMessages(library(argparse))

# variables
variables = list(
    debug = FALSE,
    summaryNames = c(
        'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
        'MitoAdmixture', 'YChrAdmixture'
    ),
    paramNames = c(
        'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
        'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile',
        'melanesianDeathRates'
    ),
    # infoNames = c('Label', 'Island'),
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')
)

if (variables$debug) {
    args = list(
        order = '',
        admix = ''
    )
} else {
    # CLI arguments
    parser = ArgumentParser(
        description = 'Analyse the stability of a fixed set of parameters'
    )

    parser$add_argument(
        'order', type = 'character', help = 'path to the order file'
    )
    parser$add_argument(
        'admix', type = 'character', nargs = '?', default = 'stdin',
        help = 'path to the admix file, defaults to stdin'
    )

    args = parser$parse_args()
}

# loads data
imports = list(
    order = read.csv(args$order)[c('Island', 'order')],
    admix = read.csv(args$admix)
)

# checks random seeds are unique
tab = table(imports$admix['randomSeed'])
if (min(tab) != max(tab)) {
    print('repeated randomSeeds:')
    print(names(tab[tab == max(tab)]))
    stop('found at least one repeated random seed')
}

merged = merge(
    imports$admix,
    imports$order,
    by = 'Island'
)
merged = merged[order(merged$order),]

# aggregate
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
    subset(summaryData.SD, select = variables$summaryNames)
)

# plots
png(
    paste0('stability-', variables$now, '.png'),
    width = 1300, height = 1100
)
par(mfrow = c(3, 2), oma = c(1, 0, 0, 0))
boxplot(
    DnaAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('DNA Admixture by zone for', length(tab), 'runs'),
    ylab = 'DNA Admixture',
    xlab = 'Zone'
)
boxplot(
    AutosomeAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Autosome Admixture by zone for', length(tab), 'runs'),
    ylab = 'Autosome Admixture',
    xlab = 'Zone'
)
boxplot(
    XChrAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('X Chromosome Admixture by zone for', length(tab), 'runs'),
    ylab = 'X Chromosome Admixture',
    xlab = 'Zone'
)
boxplot(
    MitoAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Mitochondrial Admixture by zone for', length(tab), 'runs'),
    ylab = 'Mitochondrial Admixture',
    xlab = 'Zone'
)
boxplot(
    YChrAdmixture~factor(Island, levels = unique(merged$Island)),
    data = summaryData.means,
    main = paste('Y Chromosome Admixture by zone for', length(tab), 'runs'),
    ylab = 'Y Chromosome Admixture',
    xlab = 'Zone'
)
mtext(args$admix, side = 1, outer = TRUE)
graphics.off()
