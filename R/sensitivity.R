#!/usr/bin/Rscript
suppressMessages(library(argparse))
suppressMessages(library(lattice))
suppressMessages(library(vegan))
suppressMessages(library(SpatialTools))

#variables
debug = FALSE
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
    'MitoAdmixture', 'YChrAdmixture'
)
variables.paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile',
    'melanesianDeathRates'
)
variables.now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

if (debug) {
    args$order = ''
    args$real = ''
    args$changing = 'migrationProb'
    args$admix = ''
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

if (!args$changing %in% variables.paramNames) {
    stop('not a valid parameter')
}


#load data
imports.admix = read.csv(args$admix)
imports.order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')]
imports.real  = read.csv(args$real)[c('Island', 'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture')]

variables.comparedIslands = unlist(imports.real[!is.na(imports.real$DnaAdmixture),]['Island'])

# checks
if (dim(unique(imports.admix[args$changing]))[1] == 1) {
    stop('analysis can\'t be done if parameter doesn\'t change')
}
randomSeeds = imports.admix['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports.admix['Island']))[1] != dim(unique(randomSeeds))[1]) {
    stop('found a repeated random seed')
}

# merges admix and order
merged = merge(
    #merged,
    imports.admix,
    imports.order,
    by = 'Island',
)
merged = merged[order(merged$order),]

# aggregates
summaryData.means = aggregate(
    as.formula(paste('. ~ ', paste('Island + randomSeed +', args$changing))),
    data = merged,
    FUN = mean,
    na.action = NULL
)

summaryData.SD = aggregate(
    . ~ Island,
    data = summaryData.means,
    FUN = sd
)

summaryData.var = aggregate(
    . ~ Island,
    data = summaryData.means,
    FUN = var
)
summaryData.var = merge(
    summaryData.var[c(variables.summaryNames, 'Island')],
    imports.order,
    by = 'Island'
)
summaryData.var = summaryData.var[order(summaryData.var['order']),]

summaryData.meanSD = colMeans(
    subset(summaryData.SD, select = variables.summaryNames)
)

#plots
png(
    paste0('sensitivity-', args$changing, '-', variables.now, '.png'),
    width = 1000, height = 1200
)
par(mfrow = c(2, 1))
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = 'Zone', ylab = 'Admixture variance',
    xlim = c(1, length(summaryData.var$Island)),
    ylim = c(0, max(summaryData.var[variables.summaryNames])),
    main = paste('Sensitivity of admixture to', args$changing, 'by zone')
)
i = 0
for (adm in variables.summaryNames) {
    i = i + 1
    points(
        as.vector(unlist(summaryData.var[adm])),
        col = i, pch = i
    )
}
legend(
    x = 'topleft',
    legend = variables.summaryNames,
    col = 1:i, pch = 1:i
)
axis(1, at = 1:length(summaryData.var$Island), labels = unique(merged$Island))


#aggregate2
summaryData2.means = aggregate(
    . ~ Island + randomSeed,
    data = merged,
    FUN = mean
)

#hardcoded, we don't have Mito and YChr real data (yet?)
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture'
)

summaryData2.SquaredD = summaryData2.means[summaryData2.means$Island %in% unlist(variables.comparedIslands),]

for (i in 1:nrow(summaryData2.SquaredD)) {
    island = summaryData2.SquaredD[i, 'Island']
    squaredD = (
        summaryData2.SquaredD[i, variables.summaryNames]
        - imports.real[grepl(island, imports.real$Island), variables.summaryNames]
    ) ^ 2
    summaryData2.SquaredD[i, variables.summaryNames] = squaredD
}

summaryData2.SquaredDSum = aggregate(
    as.formula(paste0('cbind(', paste(variables.summaryNames, collapse=','), ') ~ randomSeed')),
    data = summaryData2.SquaredD,
    sum
)

summaryData2.SquaredDSum = merge(
    summaryData2.SquaredDSum,
    imports.admix[c('randomSeed', args$changing)],
    by = 'randomSeed'
)

values = merge(
    imports.real[!is.na(imports.real$DnaAdmixture),],
    imports.order,
    by = 'Island'
)
admix = values$DnaAdmixture

summaryData3 = matrix(
    nrow = length(summaryData.means$randomSeed) / length(unique(summaryData.means$Island)),
    ncol = 2
)
colnames(summaryData3) = c(args$changing, 'mantel')
count = 1
tmp.admix = dist(admix)
tmp.pos = dist1(matrix(cbind(values$longitude, values$latitude), ncol = 2))

for(seed in unique(summaryData.means$randomSeed)) {
    set = summaryData.means[summaryData.means$randomSeed == seed,]
    set = set[order(set$order),]
    set = merge(
        set, imports.real[!is.na(imports.real$DnaAdmixture),]['Island'],
        by = 'Island'
    )
    res = mantel.partial(
        tmp.admix,
        dist(set$DnaAdmixture),
        tmp.pos,
        permutations = 99
    )
    summaryData3[count,] = c(
        set[1, args$changing],
        res$statistic
    )
    count = count + 1
}

plot(
    as.data.frame(table(summaryData3[, args$changing])),
    main = paste('counts of every different', args$changing),
    ylab = 'count', xlab = args$changing, xaxt = 'n'
)
axis(
    1,
    at = 1:length(unlist(unique(summaryData2.SquaredDSum[args$changing]))),
    labels = sub(
        'starting_distribution_', '',
        unlist(unique(summaryData2.SquaredDSum[args$changing]))
    )
)

graphics.off()

#plot
isNum = is.numeric(unlist(summaryData2.SquaredDSum[args$changing]))
png(
    paste0('comparisons-', args$changing, '-', variables.now, '.png'),
    width = 1000, height = 1200
)
par(mfrow = c(2, 1))
if (isNum) {
    plot(
        NA,
        xlab = args$changing, ylab = 'SquaredDistance of admixture',
        xlim = c(0, max(summaryData2.SquaredDSum[args$changing])),
        ylim = c(0, max(summaryData2.SquaredDSum[variables.summaryNames])),
        main = paste('Sum of squared distances of admixture in relation to', args$changing)
    )
    i = 0
    for (adm in variables.summaryNames) {
        i = i + 1
        points(
            as.vector(unlist(summaryData2.SquaredDSum[args$changing])),
            as.vector(unlist(summaryData2.SquaredDSum[adm])),
            col = i, pch = i
        )
    }
} else {
    plot(
        NA,
        xlab = args$changing, ylab = 'SquaredDistance of admixture',
        xlim = c(0, length(unlist(unique(summaryData2.SquaredDSum[args$changing])))),
        xaxt = "n",
        ylim = c(0, max(summaryData2.SquaredDSum[variables.summaryNames])),
        main = paste('Sum of squared distances of admixture in relation to', args$changing)
    )
    p = -0.3
    for (param in unlist(unique(summaryData2.SquaredDSum[args$changing]))) {
        p = p + 0.7
        i = 0
        for (adm in variables.summaryNames) {
            i = i + 1
            v = unlist(summaryData2.SquaredDSum[summaryData2.SquaredDSum[args$changing] == param,adm])
            points(
                rep(p, length(v)),
                as.vector(v),
                col = i, pch = i
            )
            p = p + 0.1
        }
    }
    axis(
        1,
        at = (1:length(unlist(unique(summaryData2.SquaredDSum[args$changing]))))-0.5,
        labels = sub(
            'starting_distribution_', '',
            unlist(unique(summaryData2.SquaredDSum[args$changing]))
        )
    )
}

legend(
    x = 'bottomleft',
    legend = variables.summaryNames,
    col = 1:i, pch = 1:i
)
# summaryData2.SquaredDSum[c(variables.summaryNames, 'run')] = aggregate(
#     . ~ run,
#     data = summaryData2.SquaredD[c(variables.summaryNames, 'run')],
#     FUN = sum
# )

#levelplots

#codes = values$code
#long = values$longitude

# mAdmi = matrix(
#     rep(admix, length(admix)), dimnames = list(codes, codes),
#     nrow = length(admix), ncol = length(admix)
# )
# mDist = (mAdmi - t(mAdmi)) ^ 2
# mLong = matrix(
#     rep(long, length(long)), dimnames = list(codes, codes),
#     nrow = length(long), ncol = length(long)
# )
# mDistLong = (mLong - t(mLong)) ^ 2
# mDistLong = 1 - ((mDistLong - min(mDistLong)) / (max(mDistLong) - min(mDistLong)))

#rgb.palette = colorRampPalette(c('blue', 'red'), space = 'rgb')
#levelplot(mDist, col.regions = rgb.palette(255), at = seq(0, 1, 0.01), alpha.regions = mDistLong)

# mantel.partial(
#     dist(admix),
#     dist(admix),
#     dist1(matrix(cbind(values$longitude, values$latitude), ncol = 2))
# )

boxplot(
    as.formula(paste('mantel ~', args$changing)),
    data = summaryData3,
    main = paste('matrix correlation (partial Mantel test) between real and simulated DNA Admixture data'),
    ylab = 'Correlation', ylim = c(-1, 1),
    xlab = args$changing, xaxt = 'n'
)
axis(
    1,
    at = 1:length(unlist(unique(summaryData2.SquaredDSum[args$changing]))),
    labels = sub(
        'starting_distribution_', '',
        unlist(unique(summaryData2.SquaredDSum[args$changing]))
    )
)

graphics.off()

