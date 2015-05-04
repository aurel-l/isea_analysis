#!/usr/bin/Rscript
suppressMessages(library(argparse))
suppressMessages(library(lattice))
suppressMessages(library(vegan))
suppressMessages(library(SpatialTools))

#variables
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
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S'),
    permutations = 99
)

if (variables$debug) {
    args = list(
        order = '',
        real = '',
        changing = 'migrationProb',
        admix = ''
    )
    # makes debug tests faster
    variables$permutations = 9
} else {
    #CLI arguments
    parser = ArgumentParser(
        description = 'Analyse the sensitivity of a set of parameters with one changing parameter'
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

#load data
imports = list(
    admix = read.csv(args$admix),
    order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')],
    real  = read.csv(args$real)[c('Island', 'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture')]
)
imports$real = imports$real[!is.na(imports$real$AutosomeAdmixture),]

variables$comparedIslands = imports$real[, 'Island']

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

# merges admix and order
merged = merge(
    imports$admix,
    imports$order,
    by = 'Island',
)
merged = merged[order(merged$order),]
merged$Island = factor(merged$Island)

# aggregates
summaryData = list()
summaryData$means = aggregate(
    as.formula(paste('. ~ ', paste('Island + randomSeed +', args$changing))),
    data = merged,
    FUN = mean,
    na.action = NULL
)

summaryData$stddev = aggregate(
    . ~ Island,
    data = summaryData$means,
    FUN = sd
)

summaryData$stddev = merge(
    summaryData$stddev[c(variables$summaryNames, 'Island')],
    imports$order,
    by = 'Island'
)
summaryData$stddev = summaryData$stddev[order(summaryData$stddev['order']),]

#plots
png(
    paste0('sensitivity-', args$changing, '-', variables$now, '.png'),
    width = 1000, height = 1200
)
par(mfrow = c(2, 1), oma = c(1, 0, 0, 0))
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = 'Zone', ylab = 'Admixture standard deviation',
    xlim = c(1, length(summaryData$stddev$Island)),
    ylim = c(0, max(summaryData$stddev[variables$summaryNames])),
    main = paste('Sensitivity of admixture to', args$changing, 'by zone')
)
i = 0
for (adm in variables$summaryNames) {
    i = i + 1
    points(
        as.vector(unlist(summaryData$stddev[adm])),
        col = i, pch = i
    )
}
legend(
    x = 'topleft',
    legend = variables$summaryNames,
    col = 1:i, pch = 1:i
)
axis(1, at = 1:length(summaryData$stddev$Island), labels = summaryData$stddev$Island)


# aggregate2
summaryData2 = list()
summaryData2$means = aggregate(
    . ~ Island + randomSeed,
    data = summaryData$means,
    FUN = mean
)

# overwrite hardcoded, we don't have Mito and YChr real data (yet?)
variables$summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture'
)

summaryData2$SquaredD = summaryData2$means[summaryData2$means$Island %in% variables$comparedIslands,]

for (i in 1:nrow(summaryData2$SquaredD)) {
    island = summaryData2$SquaredD[i, 'Island']
    squaredD = (
        summaryData2$SquaredD[i, variables$summaryNames]
        - imports$real[grepl(island, imports$real$Island), variables$summaryNames]
    ) ^ 2
    summaryData2$SquaredD[i, variables$summaryNames] = squaredD
}

summaryData2$SquaredDSum = aggregate(
    as.formula(paste0('cbind(', paste(variables$summaryNames, collapse=','), ') ~ randomSeed')),
    data = summaryData2$SquaredD,
    sum
)

summaryData2$SquaredDSum = merge(
    summaryData2$SquaredDSum,
    imports$admix[c('randomSeed', args$changing)],
    by = 'randomSeed'
)

values = merge(
    imports$real,
    imports$order,
    by = 'Island'
)
admix = list()
admix$AutosomeAdmixture = values$AutosomeAdmixture
admix$XChrAdmixture = values$XChrAdmixture

summaryData3 = list(
    AutosomeAdmixture = data.frame(matrix(
        nrow = length(tab) / length(variables$comparedIslands),
        ncol = 2
    )),
    XChrAdmixture = data.frame(matrix(
        nrow = length(tab) / length(variables$comparedIslands),
        ncol = 2
    ))
)
colnames(summaryData3$AutosomeAdmixture) = colnames(summaryData3$XChrAdmixture) = c(args$changing, 'mantel')

count = 1
tmp = list(
    admix = list(
        AutosomeAdmixture = dist(admix$AutosomeAdmixture),
        XChrAdmixture = dist(admix$XChrAdmixture)
    ),
    pos = dist1(matrix(cbind(values$longitude, values$latitude), ncol = 2))
)

for(seed in names(tab)) {
    set = summaryData$means[summaryData$means$randomSeed == seed,]
    set = set[order(set$order),]
    set = merge(
        set,
        imports$real['Island'],
        by = 'Island'
    )
    res = mantel.partial(
        tmp$admix$AutosomeAdmixture,
        dist(set$AutosomeAdmixture),
        tmp$pos,
        permutations = variables$permutations
    )
    summaryData3$AutosomeAdmixture[count,] = c(
        as.character(set[1, args$changing]),
        res$statistic
    )
    res = mantel.partial(
        tmp$admix$XChrAdmixture,
        dist(set$XChrAdmixture),
        tmp$pos,
        permutations = variables$permutations
    )
    summaryData3$XChrAdmixture[count,] = c(
        as.character(set[1, args$changing]),
        res$statistic
    )
    count = count + 1
}

summaryData3$AutosomeAdmixture[, args$changing] = as.factor(summaryData3$AutosomeAdmixture[, args$changing])
summaryData3$AutosomeAdmixture$mantel = as.numeric(summaryData3$AutosomeAdmixture$mantel)
summaryData3$XChrAdmixture[, args$changing] = as.factor(summaryData3$XChrAdmixture[, args$changing])
summaryData3$XChrAdmixture$mantel = as.numeric(summaryData3$XChrAdmixture$mantel)

freq = as.data.frame(table(summaryData3$Auto[, args$changing]))
plot(
    freq,
    main = paste('counts of every different', args$changing),
    ylim = c(0, max(freq$Freq)),
    ylab = 'count', xlab = args$changing, xaxt = 'n'
)
axis(
    1,
    at = summaryData3$AutosomeAdmixture[, args$changing],
    labels = gsub(
        '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
        summaryData3$AutosomeAdmixture[, args$changing]
    )
)

mtext(args$admix, side = 1, outer = TRUE)
graphics.off()

#plot
isNum = is.numeric(unlist(summaryData2$SquaredDSum[args$changing]))
png(
    paste0('comparisons-', args$changing, '-', variables$now, '.png'),
    width = 1400, height = 1000
)
par(mfcol = c(length(summaryData3), length(summaryData3)), oma = c(1, 0, 0, 0))

for (i in 1:length(summaryData3)) {
    boxplot(
        as.formula(paste(names(summaryData3)[i], '~', args$changing)),
        data = summaryData2$SquaredDSum,
        main = paste('Sum of squared distances of', names(summaryData3)[i], 'in relation to', args$changing),
        ylab = 'Sum of squared distances of admixture',
        ylim = c(0, max(summaryData2$SquaredDSum[names(summaryData3)[i]])),
        xlab = args$changing, xaxt = 'n'
    )
    axis(
        1,
        at = summaryData3[[i]][, args$changing],
        #las = 2, # vertical label
        labels=gsub(
            '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
            summaryData3[[i]][, args$changing]
        )
    )

    boxplot(
        as.formula(paste('mantel ~', args$changing)),
        data = summaryData3[[i]],
        main = paste('matrix correlation (partial Mantel test) between real and simulated', names(summaryData3)[i], 'data'),
        ylab = 'Correlation', ylim = c(-1, 1),
        xlab = args$changing, xaxt = 'n'
    )
    axis(
        1,
        at = summaryData3[[i]][, args$changing],
        labels=gsub(
            '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
            summaryData3[[i]][, args$changing]
        )
    )
}

mtext(args$admix, side = 1, outer = TRUE)
graphics.off()
