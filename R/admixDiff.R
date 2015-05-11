#!/usr/bin/Rscript

# source script that is common to all scripts
source(paste0(
    dirname(sub('--file=','',commandArgs(trailingOnly=F)[grep('--file=',commandArgs(trailingOnly=F))])),
    '/common.R'
))

# variables overwrite
#variables$debug = TRUE
variables$summaryNames = c(
    'AutosomeAdmixture', 'XChrAdmixture'
)

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        real = '../Data/isea_admixture_data_for_comparison_2.csv',
        changing = 'poissonMean',
        admix = 'test4.csv'
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

variables$numerical = !(args$changing %in% variables$discreteParams)

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

# creates parameter info file
toXMLFile(merged)

aggregated = aggregate(
    as.formula(paste('diff ~ Island +', args$changing)),
    data = merged,
    FUN = mean
)
aggregated$stddev = aggregate(
    as.formula(paste('diff ~ Island +', args$changing)),
    data = merged,
    FUN = sd
)$diff

# png(
#     paste0('admixDiff-sensitivity-', args$changing, '-', variables$now, '.png'),
#     width = 900, height = 1200
# )
# par(mfrow = c(2, 1), oma = c(1, 0, 0, 0))
#
# if (is.numeric(unlist(merged[args$changing]))) {
#     xlimit = c(min(merged[args$changing]), max(merged[args$changing]))
# } else {
#     xlimit = c(1, length(unlist(unique(merged[args$changing]))))
# }
# plot(
#     NA,
#     col = 1, pch = 1, xaxt = 'n',
#     xlab = args$changing, ylab = 'difference XChr Auto',
#     xlim = xlimit,
#     ylim = c(
#         min(-0.25, min(aggregated$diff)),
#         max(0.25, max(aggregated$diff) + (max(aggregated$diff) - min(aggregated$diff)) * 0.2)
#     ),
#     main = paste('difference of XChr and Auto admixture depending on', args$changing)
# )
# i = 0
# for(island in levels(aggregated$Island)) {
#     i = i + 1
#
#     points(
#         as.formula(paste('diff ~', args$changing)),
#         data = aggregated[aggregated$Island == island,],
#         col = i, pch = ceiling(i / 4) + 14, type = 'b'
#     )
# }
# axis(
#     1,
#     at = unlist(unique(merged[args$changing])),
#     labels=gsub(
#         '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
#         unlist(unique(merged[args$changing]))
#     )
# )
# legend(
#     'topright', ncol = 4,
#     pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
#     legend = levels(merged$Island),
#     col = 1:length(levels(merged$Island))
# )
#
# plot(
#     NA,
#     col = 1, pch = 1, xaxt = 'n',
#     xlab = args$changing, ylab = 'standard deviation of difference XChr Auto',
#     xlim = xlimit,
#     ylim = c(
#         min(aggregated$stddev),
#         max(aggregated$stddev) + (max(aggregated$stddev) - min(aggregated$stddev)) * 0.2
#     ),
#     main = paste('standard deviation of difference of XChr and Auto admixture depending on', args$changing)
# )
# i = 0
#
# for(island in levels(aggregated$Island)) {
#     i = i + 1
#
#     points(
#         as.formula(paste('stddev ~', args$changing)),
#         data = aggregated[aggregated$Island == island,],
#         col = i, pch = ceiling(i / 4) + 14, type = 'b'
#     )
# }
# axis(
#     1,
#     at = unlist(unique(merged[args$changing])),
#     labels=gsub(
#         '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
#         unlist(unique(merged[args$changing]))
#     )
# )
# legend(
#     'topright', ncol = 4,
#     pch = rep(15:(14 + length(merged$Island) / 4), each = 4),
#     legend = levels(merged$Island),
#     col = 1:length(levels(merged$Island))
# )
#
# mtext(args$admix, side = 1, outer = TRUE)
# graphics.off()

if (variables$numerical) {
    minInterval = min(dist(unique(aggregated[, args$changing])))
    pd = position_dodge(0.4 * minInterval)
    width = 1 * minInterval
} else {
    pd = position_dodge(0.4)
    width = 1
}

p = ggplot(
    aggregated,
    aes(
        x = aggregated[, args$changing],
        y = diff, ymax = max(diff + stddev),
        colour = Island
    )
)
p = p + theme(text = element_text(size = 25))
p = p + geom_errorbar(
    aes(ymin = diff - stddev, ymax = diff + stddev),
    width = width, position = pd, alpha = 0.35
)
p = p + geom_point(aes(shape = Island), position = pd, size = 5)
p = p + scale_shape_manual(values = rep(15:18, 4))
if (variables$numerical) {
    p = p + geom_smooth(method = 'loess', se = FALSE)
    p = p + scale_x_continuous(
        breaks = unique(aggregated[, args$changing]),
        name = args$changing
    )
} else {
    p = p + scale_x_discrete(
        name = args$changing,
        labels=gsub(
            '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
            unlist(unique(merged[args$changing]))
        )
    )
}
p = p + scale_y_continuous(
    name = 'XChr - Autosomal admixture',
    limits = c(0, 1), breaks = seq(0, 1, by = 0.1)
)
p = p + ggtitle(
    paste('Difference of admixture between XChr and Autosome in relation to changes in', args$changing)
)

png(
    paste0(variables$now, '-admixDiff-sensitivity-', args$changing, '.png'),
    width = 1754, height = 1240
)
p
graphics.off()
