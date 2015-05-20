#!/usr/bin/Rscript

# source script that is common to all scripts
tryCatch({
    # look for common.R in the same folder than the script
    source(paste0(
        dirname(sub('--file=','',commandArgs(trailingOnly=F)[grep('--file=',commandArgs(trailingOnly=F))])),
        '/common.R'
    ))
}, warning = function(a) {
    # when not run from CLI, assumes common.R is in the working directory
    source('common.R')
})
suppressMessages(library(lattice))
suppressMessages(library(vegan))
suppressMessages(library(SpatialTools))

# variables overwrite
#variables$debug = TRUE

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        admix = '../test/test150518-sub.csv'
    )
    # makes debug tests faster
    variables$permutations = 1
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
        'admix', type = 'character', nargs = '?', default = 'stdin',
        help = 'path to the admix file, defaults to stdin'
    )

    args = parser$parse_args()
}

if (variables$debug) {
    cat('executing in debug mode, ignoring any command line argument\n')
}

# load data
imports = list(
    admix = read.csv(args$admix),
    order = read.csv(args$order)[c('Island', 'order', 'longitude', 'latitude')],
    real  = read.csv(args$real)[c('Island', 'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture')]
)
imports$real = imports$real[!is.na(imports$real$AutosomeAdmixture), ]
imports$real = imports$real[order(imports$order$order), ]

variables$comparedIslands = imports$real[, 'Island']

variables$params = sweepParams(imports$admix)

# checks
if (length(variables$params$names) > 2) {
    cat(paste(variables$params$changing, 'changing parameters:\n'))
    print(variables$params$names)
    stop('analysis can\'t be done if too many parameters change')
}
tab = table(imports$admix['randomSeed'])
if (min(tab) != max(tab)) {
    cat('repeated randomSeeds:\n')
    print(names(tab[tab == max(tab)]))
    stop('found at least one repeated random seed')
}

# info
cat(
    paste0(
        'performing ',
        if (!variables$params$changing) 'stability' else 'sensitivity',
        ' analysis on ',
        variables$params$changing,
        ' changing variable',
        if (variables$params$changing > 1) 's\n' else '\n'
    )
)
toXMLFile(imports$admix, variables$debug)

# merges admix and order
merged = merge(
    imports$admix,
    imports$order,
    by = 'Island',
)
merged = merged[order(merged$order), ]
merged$Island = factor(merged$Island)

# aggregates
# aggregated = list()
# aggregated$stddev = aggregate(
#     . ~ Island,
#     data = merged,
#     FUN = sd
# )
# aggregated$stddev = merge(
#     aggregated$stddev[, c(variables$summaryNames, 'Island')],
#     imports$order,
#     by = 'Island'
# )
# aggregated$stddev = aggregated$stddev[order(aggregated$stddev['order']), ]
# aggregated$stddev$Island = factor(aggregated$stddev$Island, levels=aggregated$stddev$Island)
#
# melted = melt(
#     aggregated$stddev[, c('Island', variables$summaryNames)],
#     id = 'Island'
# )
# melted$Island = factor(melted$Island, unique(melted$Island))
#
# p1 = ggplot(
#     melted,
#     aes(x = Island, y = value, colour = variable)
# )
# p1 = p1 + theme(text = element_text(size = variables$textSize))
# p1 = p1 + geom_errorbar(aes(y = value, ymin = value, ymax = value))
# p1 = p1 + scale_color_discrete(
#     name = 'type of admixture',
#     labels = c(
#         'Whole DNA (52m)', 'Auto (25m)', 'X Chr (25m)',
#         'Mito (1m)', 'Y Chr (1m)'
#     )
# )
# p1 = p1 + scale_x_discrete(name = 'zone')
# p1 = p1 + scale_y_continuous(name = 'standard deviation of admixture')
# p1 = p1 + ggtitle(
#     paste(
#         'Sensitivity of admixture to',
#         toString(variables$params$names),
#         'by zone'
#     )
# )
p1 = stability(merged, variables$params$names)

if (variables$debug) {
    p1
}

# counts
# counts = aggregate(
#     randomSeed ~ .,
#     merged[, c('randomSeed', variables$params$names)],
#     FUN = function(x) length(unique(x))
# )
#
# p2 = ggplot(
#     counts,
#     aes(
#         x = factor(counts[, variables$params$names[2]]),
#         y = factor(counts[, variables$params$names[1]])
#     )
# )
# p2 = p2 + theme(text = element_text(size = variables$textSize))
# p2 = p2 + geom_tile(aes(fill = randomSeed))
# p2 = p2 + scale_x_discrete(name = variables$params$names[2], expand = c(0, 0))
# p2 = p2 + scale_y_discrete(name = variables$params$names[1], expand = c(0, 0))
# p2 = p2 + scale_fill_gradient(
#     name = 'count', limits = c(0, max(counts$randomSeed)),
#     guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
# )
# p2 = p2 + ggtitle(
#     paste(
#         'Count of simulations for every different',
#         toString(variables$params$names)
#     )
# )
p2 = counts(merged, variables$params$names)

if (variables$debug) {
    p2
}

if (!variables$debug) {
    png(
        paste0(
            variables$now,
            '-sensitivity-',
            sub(', ', '-',toString(variables$params$names)),
            '.png'),
        width = 1754, height = 1240
    )
    grid.arrange(p1, p2, nrow = 2)
    graphics.off()
}

# Summary Statistics
SS = list(
    values = data.frame(
        matrix(0, length(names(tab)), 4 + variables$params$changing)
    )
)
colnames(SS$values) = c(
    'AutosomeMSD', 'AutosomeCor', 'XChrMSD', 'XChrCor', variables$params$names
)

SS$real = list(
    A = imports$real[
        imports$real$Island %in% variables$observedIslands,
    ]$AutosomeAdmixture,
    X = imports$real[
        imports$real$Island %in% variables$observedIslands,
    ]$XChrAdmixture
)
SS$realDist = list(
    A = dist(SS$real$A),
    X = dist(SS$real$X)
)
SS$geoDist = dist1(matrix(cbind(
    imports$order[imports$order$Island %in% variables$observedIslands, ]$longitude,
    imports$order[imports$order$Island %in% variables$observedIslands, ]$latitude
),ncol = 2))

i = 1
for (r in names(tab)) {
    simu = merged[
        merged$randomSeed == r,
        c('Island', 'AutosomeAdmixture', 'XChrAdmixture', variables$params$names)
    ]
    SS$values[i, variables$params$names] = simu[1, variables$params$names]
    simu = simu[
        simu$Island %in% variables$observedIslands,
        c('AutosomeAdmixture', 'XChrAdmixture')
    ]

    SS$values[i, 'AutoMSD'] = mean((SS$real$A - simu$AutosomeAdmixture) ^ 2)
    SS$values[i, 'XChrMSD'] = mean((SS$real$X - simu$XChrAdmixture) ^ 2)

    SS$values[i, 'AutoCor'] = mantel.partial(
        SS$realDist$A,
        dist(simu$AutosomeAdmixture),
        SS$geoDist,
        permutations = 1
    )$statistic
    SS$values[i, 'XChrCor'] = mantel.partial(
        SS$realDist$X,
        dist(simu$XChrAdmixture),
        SS$geoDist,
        permutations = 1
    )$statistic

    i = i + 1
}

for (p in variables$params$names) {
    SS$values[, p] = factor(SS$values[, p])
}

SS$values = list(
    mean = aggregate(
        as.formula(paste('. ~', sub(', ', ' + ', toString(variables$params$names)))),
        data = SS$values,
        FUN = mean
    ),
    stddev = aggregate(
        as.formula(paste('. ~', sub(', ', ' + ', toString(variables$params$names)))),
        data = SS$values,
        FUN = sd
    )
)

plots = sensitivity(SS$values, variables$params$names)
if (!variables$debug) {
    png(
        paste0(
            variables$now,
            '-comparisons-',
            sub(', ', '-',toString(variables$params$names)),
            '.png'),
        width = 1240, height = 1754
    )
}
do.call(grid.arrange, plots)
if (!variables$debug) {
    graphics.off()
}

# plots = sensitivity(SS$values$X, variables$params$names, 'X-Chromosome')
# if (!variables$debug) {
#     png(
#         paste0(
#             variables$now,
#             '-comparisons-xchromosome-',
#             sub(', ', '-',toString(variables$params$names)),
#             '.png'),
#         width = 1240, height = 1754
#     )
# }
# do.call(grid.arrange, plots)
# if (!variables$debug) {
#     graphics.off()
# }

# p3 = ggplot(
#     SS$values$A$mean,
#     aes(
#         x = SS$values$A$mean[, variables$params$names[1]],
#         y = SS$values$A$mean[, variables$params$names[2]]
#     )
# )
# p3 = p3 + theme(text = element_text(size = variables$textSize))
# p3 = p3 + geom_tile(aes(fill = MSD))
# p3 = p3 + scale_x_discrete(
#     name = variables$params$names[1], expand = c(0, 0),
#     labels = clean(levels(factor(merged[, variables$params$names[1]])))
# )
# p3 = p3 + scale_y_discrete(
#     name = variables$params$names[2], expand = c(0, 0),
#     labels = clean(levels(merged[, variables$params$names[2]]))
# )
# p3 = p3 + scale_fill_gradientn(
#     name = 'MSD\n',
#     colours = c('green', 'red', 'red', 'red', 'black'), limits = c(0, 1),
#     guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
# )
# p3 = p3 + ggtitle(
#     paste(
#         'Mean of squared distances of admixtures in relation to',
#         toString(variables$params$names)
#     )
# )
#
# if (variables$debug) {
#     p3
# }
#
# p4 = ggplot(
#     SS$values$A$mean,
#     aes(
#         x = SS$values$A$mean[, variables$params$names[1]],
#         y = SS$values$A$mean[, variables$params$names[2]]
#     )
# )
# p4 = p4 + theme(text = element_text(size = variables$textSize))
# p4 = p4 + geom_tile(aes(fill = cor))
# p4 = p4 + scale_x_discrete(
#     name = variables$params$names[1], expand = c(0, 0),
#     labels = clean(levels(factor(merged[, variables$params$names[1]])))
# )
# p4 = p4 + scale_y_discrete(
#     name = variables$params$names[2], expand = c(0, 0),
#     labels = clean(levels(merged[, variables$params$names[2]]))
# )
# p4 = p4 + scale_fill_gradientn(
#     name = 'Partial Mantel\ncorrelation\n',
#     colours = c('blue', 'red', 'green'), limits = c(-1, 1),
#     guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
# )
# p4 = p4 + ggtitle(
#     paste(
#         'Partial Mantel correlation test in relation to',
#         toString(variables$params$names)
#     )
# )

# if (variables$debug) {
#     p4
# }

# if (!variables$debug) {
#     png(
#         paste0(
#             variables$now,
#             '-comparisons-',
#             sub(', ', '-',toString(variables$params$names)),
#             '.png'),
#         width = 1754, height = 1240
#     )
#     grid.arrange(p3, p4, nrow = 2)
#     graphics.off()
# }
