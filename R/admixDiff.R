#!/usr/bin/Rscript
suppressMessages(library(argparse))

#variables
variables = list()
variables$debug = FALSE
variables$summaryNames = c(
    'AutosomeAdmixture', 'XChrAdmixture'
)
variables$paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile',
    'melanesianDeathRates'
)
variables$now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

if (variables$debug) {
    args = list()
    args$order = ''
    args$real = ''
    args$changing = 'migrationProb'
    args$admix = ''
} else {
    #CLI arguments
    parser = ArgumentParser(
        description = 'Analyse the difference between XChr and Auto Admixture with one changing parameter'
    )

    parser$add_argument(
        'order', type = 'character', help = 'path to the order file'
    )
    parser$add_argument(
        'real', type = 'character', help = 'path to the real admixture data file'
    )
    parser$add_argument(
        'changing', type = 'character', help = 'name of the changing parameter'
    )
    parser$add_argument(
        'admix', type = 'character', nargs = '?', default = 'stdin',
        help = 'path to the admix file, defaults to stdin'
    )

    args = parser$parse_args()
}

if (!args$changing %in% variables$paramNames) {
    stop('not a valid parameter')
}

# loads data
imports = list()
imports$admix = read.csv(args$admix)[c('Island', 'randomSeed', variables$paramNames, variables$summaryNames)]
imports$order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')]
imports$real  = read.csv(args$real)[c('Island', variables$summaryNames)]

variables$comparedIslands = unlist(imports$real[!is.na(imports$real$AutosomeAdmixture),]['Island'])

# checks
if (dim(unique(imports$admix[args$changing]))[1] == 1) {
    stop('analysis can\'t be done if parameter doesn\'t change')
}
randomSeeds = imports$admix['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports$admix['Island']))[1] != dim(unique(randomSeeds))[1]) {
    stop('found a repeated random seed')
}

# merges order and real
merged = merge(
    imports$order,
    imports$real[!is.na(imports$real$AutosomeAdmixture),],
    by = 'Island'
)

# merges admix and merged order and real
merged = merge(
    imports$admix,
    merged[!names(merged) %in% variables$summaryNames],
    by = 'Island'
)
merged$Island = factor(merged$Island)
merged = merged[order(merged$order),]
merged$diff = merged$XChrAdmixture - merged$AutosomeAdmixture

png(
    paste0('admixDiff-sensitivity-', args$changing, '-', variables$now, '.png'),
    width = 1000, height = 1000
)
par(mfrow = c(2, 1), oma = c(1, 0, 0, 0))

aggregated = list()
aggregated$mean = aggregate(
    as.formula(paste0('diff ~', args$changing, '+ Island')),
    data = merged,
    FUN = mean
)

if (is.numeric(unlist(merged[args$changing]))) {
    xlimit = c(min(merged[args$changing]), max(merged[args$changing]))
} else {
    c(1, length(unlist(unique(merged[args$changing]))))
}
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = args$changing, ylab = 'difference Auto XChr',
    xlim = xlimit,
    ylim = c(
        min(-0.25, min(aggregated$mean$diff)),
        max(0.25, max(aggregated$mean$diff) + (max(aggregated$mean$diff) - min(aggregated$mean$diff)) * 0.2)
    ),
    main = paste('difference of XChr and Auto admixture depending on', args$changing)
)
i = 0
for(island in levels(aggregated$mean$Island)) {
    i = i + 1

    points(
        as.formula(paste('diff ~', args$changing)),
        data = aggregated$mean[aggregated$mean$Island == island, ],
        col = i, pch = ceiling(i / 4) + 14, type = 'b'
    )
}
axis(
    1,
    at = unlist(unique(merged[args$changing])),
    labels=gsub(
        '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
        unlist(unique(merged[args$changing]))
    )
)
legend(
    "topright", ncol = 4,
    pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
    legend = levels(merged$Island),
    col = 1:length(levels(merged$Island))
)

aggregated$stderr = aggregate(
    as.formula(paste0('diff ~', args$changing, '+ Island')),
    data = merged,
    FUN = function(x) sqrt(var(x)/ length(x))
)
if (is.numeric(unlist(merged[args$changing]))) {
    xlimit = c(min(merged[args$changing]), max(merged[args$changing]))
} else {
    c(1, length(unlist(unique(merged[args$changing]))))
}
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = args$changing, ylab = 'standard error difference Auto XChr',
    xlim = xlimit,
    ylim = c(
        min(aggregated$stderr$diff),
        max(aggregated$stderr$diff) + (max(aggregated$stderr$diff) - min(aggregated$stderr$diff)) * 0.2
    ),
    main = paste('standard error of difference of XChr and Auto admixture depending on', args$changing)
)
i = 0
for(island in levels(aggregated$stderr$Island)) {
    i = i + 1

    points(
        as.formula(paste('diff ~', args$changing)),
        data = aggregated$stderr[aggregated$stderr$Island == island, ],
        col = i, pch = ceiling(i / 4) + 14, type = 'b'
    )
}
axis(
    1,
    at = unlist(unique(merged[args$changing])),
    labels=gsub(
        '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
        unlist(unique(merged[args$changing]))
    )
)
legend(
    "topright", ncol = 4,
    pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
    legend = levels(merged$Island),
    col = 1:length(levels(merged$Island))
)

mtext(args$admix, side = 1, outer = TRUE)
graphics.off()
