#!/usr/bin/Rscript
suppressMessages(library(argparse))
suppressMessages(library(lattice))
suppressMessages(library(vegan))
suppressMessages(library(SpatialTools))
suppressMessages(library(reshape2))
suppressMessages(library(gridExtra))
suppressMessages(library(ggplot2))

#variables
variables = list(
    debug = FALSE,
    summaryNames = c(
        'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
        'MitoAdmixture', 'YChrAdmixture'
    ),
    paramNames = c(
        'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
        'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile'
    ),
    discreteParams = c('startingDistributionFile', 'graphFile'),
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S'),
    permutations = 99
)

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        changing = 'poissonMean',
        admix = 'test4.csv'
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

variables$numerical = !(args$changing %in% variables$discreteParams)

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

# plots

melted = melt(summaryData$stddev[, c('Island', variables$summaryNames)], id = 'Island')
melted$Island = factor(melted$Island, unique(melted$Island))

summaryData$stddev$Island = factor(summaryData$stddev$Island, levels=summaryData$stddev$Island)
p1 = ggplot(
    melted,
    aes(x = Island, y = value, colour = variable)
)
p1 = p1 + theme(text = element_text(size = 25))
p1 = p1 + geom_errorbar(aes(y = value, ymin = value, ymax = value))
p1 = p1 + scale_color_discrete(
    name = 'type of admixture',
    labels = c(
        'Whole DNA (52 markers)', 'Autosomal (25 markers)', 'X Chromosomal (25 markers)',
        'Mitochondrial (1 marker)', 'Y Chromosomal (1 marker)'
    )
)
p1 = p1 + scale_y_continuous(name = 'standard deviation of admixture')
p1 = p1 + ggtitle(paste('Sensitivity of admixture to', args$changing, 'by island'))
if (variables$debug) {
    p1
}

# png(
#     paste0('sensitivity-', args$changing, '-', variables$now, '.png'),
#     width = 1000, height = 1200
# )
# par(mfrow = c(2, 1), oma = c(1, 0, 0, 0))
# plot(
#     NA,
#     col = 1, pch = 1, xaxt = 'n',
#     xlab = 'Zone', ylab = 'Admixture standard standard deviation',
#     xlim = c(1, length(summaryData$stddev$Island)),
#     ylim = c(0, max(summaryData$stddev[variables$summaryNames])),
#     main = paste('Sensitivity of admixture to', args$changing, 'by zone')
# )
# i = 0
# for (adm in variables$summaryNames) {
#     i = i + 1
#     points(
#         as.vector(unlist(summaryData$stddev[adm])),
#         col = i, pch = i
#     )
# }
# legend(
#     x = 'topleft',
#     legend = variables$summaryNames,
#     col = 1:i, pch = 1:i
# )
# axis(1, at = 1:length(summaryData$stddev$Island), labels = summaryData$stddev$Island)


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

summaryData2$SquaredDMean = aggregate(
    as.formula(paste0('cbind(', paste(variables$summaryNames, collapse=','), ') ~ randomSeed')),
    data = summaryData2$SquaredD,
    mean
)

summaryData2$SquaredDMean = merge(
    summaryData2$SquaredDMean,
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

# freq = as.data.frame(table(summaryData3$Auto[, args$changing]))
# plot(
#     freq,
#     main = paste('counts of every different', args$changing),
#     ylim = c(0, max(freq$Freq)),
#     ylab = 'count', xlab = args$changing, xaxt = 'n'
# )
# axis(
#     1,
#     at = summaryData3$AutosomeAdmixture[, args$changing],
#     labels = gsub(
#         '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
#         summaryData3$AutosomeAdmixture[, args$changing]
#     )
# )
p2 = ggplot(summaryData3$Auto, aes(summaryData3$Auto[, args$changing]))
p2 = p2 + theme(text = element_text(size = 25))
p2 = p2 + geom_bar(fill = 'white', colour = 'black', width = 0.5)
p2 = p2 + scale_x_discrete(name = args$changing)
p2 = p2 + scale_y_continuous(expand = c(0, 0))
p2 = p2 + ggtitle(paste('Count of simulations for every different', args$changing))
if (variables$debug) {
    p2
}

png(
    paste0('sensitivity-', args$changing, '-', variables$now, '.png'),
    width = 1754, height = 1240
)
grid.arrange(p1, p2, nrow = 2)
graphics.off()
# ggsave(
#     paste0('sensitivity-', args$changing, '-', variables$now, '.png'),
#     width = 11.69, height = 8.27, dpi = 150, plot = grob
# )

# mtext(args$admix, side = 1, outer = TRUE)


#plot

melted2 = melt(summaryData2$SquaredDMean[c(args$changing, 'AutosomeAdmixture', 'XChrAdmixture')], id = args$changing)
melted2[args$changing] = as.factor(melted2[, args$changing])
p3 = ggplot(
    melted2,
    aes(x = melted2[, args$changing], y = value, fill = variable)
)
p3 = p3 + theme(text = element_text(size = 25))
p3 = p3 + geom_boxplot(notch = TRUE)
p3 = p3 + scale_x_discrete(name = args$changing)
p3 = p3 + scale_y_continuous(limits = c(0, 1))
p3 = p3 + ggtitle(paste('Mean of squared distances of admixtures in relation to', args$changing))
if (variables$debug) {
    p3
}

summaryData3$AutosomeAdmixture$variable = 'AutosomeAdmixture'
summaryData3$XChrAdmixture$variable = 'XChrAdmixture'
summaryData3$melted = rbind(summaryData3$AutosomeAdmixture, summaryData3$XChrAdmixture)
summaryData3$melted$variable = as.factor(summaryData3$melted$variable)
summaryData3$melted[, args$changing] = as.factor(summaryData3$melted[, args$changing])

# melted3 = melt(summaryData3[c(args$changing, 'AutosomeAdmixture', 'XChrAdmixture')], id = args$changing)
# melted3[args$changing] = as.factor(melted3[, args$changing])
p4 = ggplot(
    summaryData3$melted,
    aes(x = summaryData3$melted[, args$changing], y = mantel, fill = variable)
)
p4 = p4 + theme(text = element_text(size = 25))
p4 = p4 + geom_boxplot(notch = TRUE)
p4 = p4 + scale_x_discrete(name = args$changing)
p4 = p4 + scale_y_continuous(limits = c(-1, 1))
p4 = p4 + ggtitle(paste('Mean of squared distances of admixtures in relation to', args$changing))
if (variables$debug) {
    p4
}

png(
    paste0('comparisons-', args$changing, '-', variables$now, '.png'),
    width = 1754, height = 1240
)
grid.arrange(p3, p4, nrow = 2)

# par(mfcol = c(length(summaryData3), length(summaryData3)), oma = c(1, 0, 0, 0))
#
# for (i in 1:length(summaryData3)) {
#     boxplot(
#         as.formula(paste(names(summaryData3)[i], '~', args$changing)),
#         data = summaryData2$SquaredDSum,
#         main = paste('Sum of squared distances of', names(summaryData3)[i], 'in relation to', args$changing),
#         ylab = 'Sum of squared distances of admixture',
#         ylim = c(0, max(summaryData2$SquaredDSum[names(summaryData3)])),
#         xlab = args$changing, xaxt = 'n'
#     )
#     axis(
#         1,
#         at = summaryData3[[i]][, args$changing],
#         #las = 2, # vertical label
#         labels=gsub(
#             '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
#             summaryData3[[i]][, args$changing]
#         )
#     )
#
#     boxplot(
#         as.formula(paste('mantel ~', args$changing)),
#         data = summaryData3[[i]],
#         main = paste('matrix correlation (partial Mantel test) between real and simulated', names(summaryData3)[i], 'data'),
#         ylab = 'Correlation', ylim = c(-1, 1),
#         xlab = args$changing, xaxt = 'n'
#     )
#     axis(
#         1,
#         at = summaryData3[[i]][, args$changing],
#         labels=gsub(
#             '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
#             summaryData3[[i]][, args$changing]
#         )
#     )
# }
#
# mtext(args$admix, side = 1, outer = TRUE)
graphics.off()
