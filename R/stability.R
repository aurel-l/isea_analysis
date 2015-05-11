#!/usr/bin/Rscript
suppressMessages(library(reshape2))

# source script that is common to all scripts
source(paste0(
    dirname(sub('--file=','',commandArgs(trailingOnly=F)[grep('--file=',commandArgs(trailingOnly=F))])),
    '/common.R'
))

# variables overwrite
#variables$debug = TRUE
variables$summaryNames = c(
    'MitoAdmixture', 'XChrAdmixture',
    'AutosomeAdmixture', 'YChrAdmixture'
)

if (variables$debug) {
    args = list(
        order = '../Data/isea_admixture_data_for_comparison_2.csv',
        admix = 'test5.csv'
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
merged$Island = factor(merged$Island, unique(merged$Island))

# creates parameter info file
toXMLFile(merged)

# aggregate
summaryData.means = aggregate(
    .~Island+run,
    data = merged,
    FUN = mean
)

# summaryData.SD = aggregate(
#     .~Island,
#     data = summaryData.means,
#     FUN = sd
# )
# summaryData.meanSD = colMeans(
#     subset(summaryData.SD, select = variables$summaryNames)
# )

# plots
melted = melt(
    summaryData.means[, c('Island', variables$summaryNames)],
    id = 'Island'
)

p1 = ggplot(
    melted,
    aes(x = Island, y = value, colour = variable)
)
p1 = p1 + theme(text = element_text(size = 25))
p1 = p1 + geom_boxplot(notch = TRUE)
p1 = p1 + facet_wrap(~ variable, ncol = 2)
p1 = p1 + scale_y_continuous(name = 'admixture', limits = c(0, 1))
p1 = p1 + guides(colour = FALSE)
p1 = p1 + ggtitle(paste('Admixture by zone for', length(tab), 'runs'))

png(
    paste0('stability-', variables$now, '.png'),
    width = 1754, height = 1240
)
p1
graphics.off()

aggregated = aggregate(
    . ~ Island + variable,
    data = melted,
    FUN = mean
)
aggregated$stddev = aggregate(
    . ~ Island + variable,
    data = melted,
    FUN = sd
)$value

pd = position_dodge(0.2)
p2 = ggplot(
    aggregated,
    aes(x = variable, y = value, ymax = 1.0, colour = Island)
)
p2 = p2 + theme(text = element_text(size = 25))
p2 = p2 + geom_point(aes(shape = Island), position = pd, size = 5)
p2 = p2 + scale_shape_manual(values = c(rep(15:18, 4), 15:17))
p2 = p2 + geom_errorbar(
    aes(ymin = value - stddev, ymax = value + stddev),
    position = pd, alpha = 0.35
)
p2 = p2 + geom_line(aes(group = Island), alpha = 0.35)
p2 = p2 + scale_x_discrete(name = 'admixture type')
p2 = p2 + scale_y_continuous(name = 'admixture value', breaks = seq(0, 1, by = 0.1))
p2 = p2 + ggtitle(paste('Admixture by type for', length(tab), 'runs'))

png(
    paste0(variables$now, '-stability-admixGradient.png'),
    width = 1754, height = 1240
)
p2
graphics.off()

# par(mfrow = c(2, 2), oma = c(1, 0, 0, 0))
# boxplot(
#     AutosomeAdmixture~factor(Island, levels = unique(merged$Island)),
#     data = summaryData.means,
#     main = paste('Autosome Admixture by zone for', length(tab), 'runs'),
#     ylab = 'Autosome Admixture',
#     xlab = 'Zone'
# )
# boxplot(
#     XChrAdmixture~factor(Island, levels = unique(merged$Island)),
#     data = summaryData.means,
#     main = paste('X Chromosome Admixture by zone for', length(tab), 'runs'),
#     ylab = 'X Chromosome Admixture',
#     xlab = 'Zone'
# )
# boxplot(
#     MitoAdmixture~factor(Island, levels = unique(merged$Island)),
#     data = summaryData.means,
#     main = paste('Mitochondrial Admixture by zone for', length(tab), 'runs'),
#     ylab = 'Mitochondrial Admixture',
#     xlab = 'Zone'
# )
# boxplot(
#     YChrAdmixture~factor(Island, levels = unique(merged$Island)),
#     data = summaryData.means,
#     main = paste('Y Chromosome Admixture by zone for', length(tab), 'runs'),
#     ylab = 'Y Chromosome Admixture',
#     xlab = 'Zone'
# )
# mtext(args$admix, side = 1, outer = TRUE)
# graphics.off()
