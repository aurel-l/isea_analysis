#!/usr/bin/Rscript
library(lattice)
library(vegan)
library(SpatialTools)

#variables
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
    'MitoAdmixture', 'YChrAdmixture'
)
variables.paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile'
)
# variables.infoNames = c('Label', 'Island')
variables.now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.orderFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.realFile = arguments[2]
        if (length(arguments) >= 3) {
            variables.changingParam = arguments[3]
            if (length(arguments) >= 4) {
                variables.admixFile = arguments[4]
            } else {
                variables.admixFile = file('stdin')
            }
        }
    }
}
if (!variables.changingParam %in% variables.paramNames) {
    stop('not a valid parameter')
}

# variables.paramFile = sub(".txt", ".batch_param_map.txt", variables.admixFile)

#load data
imports.admix = read.csv(variables.admixFile)
# imports.param = read.csv(variables.paramFile)
imports.order = read.csv(variables.orderFile)[c('Island', 'order', 'longitude', 'latitude')]
imports.real  = read.csv(variables.realFile)[c('Island', 'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture')]
# names(imports.real)[names(imports.real) == 'all_admixture'] = 'DnaAdmixture'
# names(imports.real)[names(imports.real) == 'a_admixture'] = 'AutosomeAdmixture'
# names(imports.real)[names(imports.real) == 'x_admixture'] = 'XChrAdmixture'

variables.comparedIslands = unlist(imports.real[!is.na(imports.real$DnaAdmixture),]['Island'])

#checks
if (dim(unique(imports.admix[variables.changingParam]))[1] == 1) {
    stop('analysis can\'t be done if parameter doesn\'t change')
}
randomSeeds = imports.admix['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports.admix['Island']))[1] != dim(unique(randomSeeds))[1]) {
    stop('found a repeated random seed')
}

#adds island information
# imports.admix['Island'] = lapply(
#     imports.admix['Label'],
#     function(x) sub('\\d+(_src)?', '', x)
# )

#merge imports
# merged = merge(
#     subset(imports.admix, select = c(variables.summaryNames, variables.infoNames, 'run')),
#     subset(imports.param, select = c(variables.paramNames, 'run')),
#     by = 'run'
# )
merged = merge(
    #merged,
    imports.admix,
    imports.order,
    by = 'Island',
)
merged = merged[order(merged$order),]

#aggregate
summaryData.means = aggregate(
    as.formula(paste('. ~ ', paste('Island + randomSeed +', variables.changingParam))),
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
    paste0('sensitivity-', variables.changingParam, '-', variables.now, '.png'),
    width = 900
)
plot(
    NA,
    col = 1, pch = 1, xaxt = 'n',
    xlab = 'Zone', ylab = 'Admixture variance',
    xlim = c(1, length(summaryData.var$Island)),
    ylim = c(0, max(summaryData.var[variables.summaryNames])),
    main = paste('Sensitivity of admixture to', variables.changingParam, 'by zone')
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
graphics.off()

#aggregate2
summaryData2.means = aggregate(
    . ~ Island + randomSeed,
    data = merged,
    FUN = mean
)

#hardcoded, we don't have Mito and YChr real data yet
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture'
)

summaryData2.SquaredD = summaryData2.means[summaryData2.means$Island %in% unlist(variables.comparedIslands),]
# f = function(row) {
#     print(as.vector(row['Island']))
#     return(
#         row[variables.summaryNames]
#         - imports.real[grep(row['Island'], imports.real['code']), variables.summaryNames]
#     ) ^ 2
# }
for (i in 1:nrow(summaryData2.SquaredD)) {
    island = summaryData2.SquaredD[i, 'Island']
    squaredD = (
        summaryData2.SquaredD[i, variables.summaryNames]
        - imports.real[grepl(island, imports.real$Island), variables.summaryNames]
    ) ^ 2
    summaryData2.SquaredD[i, variables.summaryNames] = squaredD
}
#apply(summaryData2.SquaredD, 1, f)

summaryData2.SquaredDSum = aggregate(
    as.formula(paste0('cbind(', paste(variables.summaryNames, collapse=','), ') ~ randomSeed')),
    data = summaryData2.SquaredD,
    sum
)

summaryData2.SquaredDSum = merge(
    summaryData2.SquaredDSum,
    imports.admix[c('randomSeed', variables.changingParam)],
    by = 'randomSeed'
)

#plot
isNum = is.numeric(unlist(summaryData2.SquaredDSum[variables.changingParam]))
png(
    paste0('comparisons-', variables.changingParam, '-', variables.now, '.png'),
    width = 1000, height = 1400
)
par(mfrow = c(3, 1))
if (isNum) {
    plot(
        NA,
        xlab = variables.changingParam, ylab = 'SquaredDistance of admixture',
        xlim = c(0, max(summaryData2.SquaredDSum[variables.changingParam])),
        ylim = c(0, max(summaryData2.SquaredDSum[variables.summaryNames])),
        main = paste('Sum of squared distances of admixture in relation to', variables.changingParam)
    )
    i = 0
    for (adm in variables.summaryNames) {
        i = i + 1
        points(
            as.vector(unlist(summaryData2.SquaredDSum[variables.changingParam])),
            as.vector(unlist(summaryData2.SquaredDSum[adm])),
            col = i, pch = i
        )
    }
} else {
    plot(
        NA,
        xlab = variables.changingParam, ylab = 'SquaredDistance of admixture',
        xlim = c(0, length(unlist(unique(summaryData2.SquaredDSum[variables.changingParam])))),
        xaxt = "n",
        ylim = c(0, max(summaryData2.SquaredDSum[variables.summaryNames])),
        main = paste('Sum of squared distances of admixture in relation to', variables.changingParam)
    )
    p = -0.3
    for (param in unlist(unique(summaryData2.SquaredDSum[variables.changingParam]))) {
        p = p + 0.7
        i = 0
        for (adm in variables.summaryNames) {
            i = i + 1
            v = unlist(summaryData2.SquaredDSum[summaryData2.SquaredDSum[variables.changingParam] == param,adm])
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
        at = (1:length(unlist(unique(summaryData2.SquaredDSum[variables.changingParam]))))-0.5,
        labels = sub(
            'starting_distribution_', '',
            unlist(unique(summaryData2.SquaredDSum[variables.changingParam]))
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
values = merge(
    imports.real[!is.na(imports.real$DnaAdmixture),],
    imports.order,
    by = 'Island'
)
admix = values$DnaAdmixture
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


summaryData3 = matrix(
    nrow = length(summaryData.means$randomSeed) / length(unique(summaryData.means$Island)),
    ncol = 2
)
colnames(summaryData3) = c(variables.changingParam, 'mantel')
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
        set[1, variables.changingParam],
        res$statistic
    )
    count = count + 1
}

boxplot(
    as.formula(paste('mantel ~', variables.changingParam)),
    data = summaryData3,
    main = paste('matrix correlation (partial Mantel test) between real and simulated DNA Admixture data'),
    ylab = 'Correlation', ylim = c(-1, 1),
    xlab = variables.changingParam, xaxt = 'n'
)
axis(
    1,
    at = 1:length(unlist(unique(summaryData2.SquaredDSum[variables.changingParam]))),
    labels = sub(
        'starting_distribution_', '',
        unlist(unique(summaryData2.SquaredDSum[variables.changingParam]))
    )
)

hist(
    summaryData3[, variables.changingParam],
    main = paste('counts of every different', variables.changingParam),
    ylab = 'count', xlab = variables.changingParam
)
graphics.off()

