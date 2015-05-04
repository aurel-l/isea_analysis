#!/usr/bin/Rscript
suppressMessages(library(argparse))

#variables
variables = list(
    debug = TRUE,
    summaryNames = c(
        'AutosomeAdmixture', 'XChrAdmixture'
    ),
    paramNames = c(
        'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
        'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile',
        'melanesianDeathRates'
    ),
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')
)

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        changing = 'poissonMean',
        admix = '../Data/o.output_2'
    )
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

# checks
if (!args$changing %in% variables$paramNames) {
    print(paste(args$changing, 'is not in the parameter list'))
    stop('not a valid parameter')
}

# loads data
imports = list(
    admix = read.csv(args$admix)[c('Island', 'randomSeed', variables$paramNames, variables$summaryNames)],
    order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')],
    real = read.csv(args$real)[c('Island', variables$summaryNames)]
)

# checks
if (dim(unique(imports$admix[args$changing]))[1] == 1) {
    print(paste(args$changing, 'always has value of', imports$admix[1, args$changing]))
    stop('analysis can\'t be done if parameter doesn\'t change')
}
tab = table(imports$admix['randomSeed'])
if (min(tab) != max(tab)) {
    print('repeated randomSeeds:')
    print(names(tab[tab == max(tab)]))
    stop('found at least one repeated random seed')
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
    width = 900, height = 1200
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
    xlimit = c(1, length(unlist(unique(merged[args$changing]))))
}
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = args$changing, ylab = 'difference XChr Auto',
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
        data = aggregated$mean[aggregated$mean$Island == island,],
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
    'topright', ncol = 4,
    pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
    legend = levels(merged$Island),
    col = 1:length(levels(merged$Island))
)

aggregated$stddev = aggregate(
    as.formula(paste0('diff ~', args$changing, '+ Island')),
    data = merged,
    FUN = sd
)

plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = args$changing, ylab = 'standard error difference XChr Auto',
    xlim = xlimit,
    ylim = c(
        min(aggregated$stddev$diff),
        max(aggregated$stddev$diff) + (max(aggregated$stddev$diff) - min(aggregated$stddev$diff)) * 0.2
    ),
    main = paste('standard deviation of difference of XChr and Auto admixture depending on', args$changing)
)
i = 0

for(island in levels(aggregated$stddev$Island)) {
    i = i + 1

    points(
        as.formula(paste('diff ~', args$changing)),
        data = aggregated$stddev[aggregated$stddev$Island == island,],
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
    'topright', ncol = 4,
    pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
    legend = levels(merged$Island),
    col = 1:length(levels(merged$Island))
)

mtext(args$admix, side = 1, outer = TRUE)
graphics.off()
