
##### sensitivity plot
# plot
p1 = ggplot(
    summary$sensit$aggr,
    aes_string(x = 'Island', y = 'value', colour = 'variable')
)
p1 = p1 + geom_errorbar(
    aes_string(y = 'value', ymin = 'value', ymax = 'value'),
    size = 1L
)
# color axis
p1 = p1 + scale_color_discrete(
    name = 'type of admixture\n(No of markers)',
    labels = c(
        'DNA (52)', 'Auto (25)', 'X Chr (25)',
        'Mito (1)', 'Y Chr (1)'
    )
)
# x axis
p1 = p1 + scale_x_discrete(name = 'Island')
# y axis
p1 = p1 + scale_y_continuous(name = 'standard deviation of admixture')
# theme
p1 = p1 + theme(text = element_text(size = variables$textSize))
# title
p1 = p1 + ggtitle(paste(
    'Sensitivity of admixture to',
    changing[1L], 'by zone'
))

if (variables$debug) {
    print(p1)
}

##### plot counts
# histogram
p2 = ggplot(summary$counts$df, aes_string(x = changing[1L], y = 'count'))
p2 = p2 + geom_bar(
    fill = 'white', colour = 'black',
    stat='identity', width = 0.5
)
# x axis
p2 = p2 + scale_x_discrete(name = changing[1L])
# y axis
p2 = p2 + scale_y_continuous(
    name = 'simulation count', expand = c(0L, 0L),
    breaks = c(
        0L, ceiling(maxCount / 5), ceiling(2 * maxCount / 5),
        ceiling(3 * maxCount / 5), ceiling(4 * maxCount / 5), maxCount
    )
)
# theme
p2 = p2 + theme(text = element_text(size = variables$textSize))
# title
p2 = p2 + ggtitle(paste(
    'Count of simulations for every different',
    args$changing[1L]
))

if (variables$debug) {
    print(p2)
}

if (!variables$debug) {
    filename = paste0(variables$now, '-sensitivity-', changing[1L])
    for (ext in c('png', 'pdf')) {
        ggsave(
            plot = arrangeGrob(p1, p2, nrow = 2L),
            filename = paste(filename, ext, sep = '.'),
            # portrait
            width = variables$short, height = variables$long, units = 'mm'
        )
    }
}

##### plot comparisons
# boxplot MSD
p3 = ggplot(
    summary$comp$all[summary$comp$all$comparison == 'MSD', ],
    aes_string(x = changing[1L], y = 'value', fill = 'admixture')
)
p3 = p3 + geom_boxplot(notch = TRUE)
# x axis
p3 = p3 + scale_x_discrete(name = changing[1L])
# y axis
p3 = p3 + scale_y_continuous(
    name = 'MSD', limits = c(0L, 1L),
    breaks = seq(0L, 1L, by = 0.1)
)
# fill axis
p3 = p3 + scale_fill_discrete(labels = c('Auto', 'X-Chr'))
# theme
p3 = p3 + theme(text = element_text(size = variables$textSize))
# title
p3 = p3 + ggtitle(paste(
    'Mean of squared distances of admixtures for every different',
    changing[1L]
))
# compare cases
p3 = p3 + geom_hline(
    aes_string(
        yintercept = 'value', colour = 'admixture',
        linetype = 'reference_case'
    ),
    variables$cases[variables$cases$comparison == 'MSD', ],
    labels = clean(variables$cases$reference_case),
    show_guide = TRUE
)
p3 = p3 + scale_linetype_discrete(
    labels = clean(levels(variables$cases$reference_case)),
    name = 'reference'
)
p3 = p3 + guides(colour = FALSE)

if (variables$debug) {
    print(p3)
}

# boxplot correlation
p4 = ggplot(
    summary$comp$all[summary$comp$all$comparison == 'cor', ],
    aes_string(x = changing[1L], y = 'value', fill = 'admixture')
)
p4 = p4 + geom_boxplot(notch = TRUE)
# x axis
p4 = p4 + scale_x_discrete(name = changing[1L])
# y axis
p4 = p4 + scale_y_continuous(
    name = 'Partial Mantel correlation', limits = c(-1L, 1L),
    breaks = seq(-1L, 1L, by = 0.2)
)
# fill axis
p4 = p4 + scale_fill_discrete(labels = c('Auto', 'X-Chr'))
# theme
p4 = p4 + theme(text = element_text(size = variables$textSize))
# title
p4 = p4 + ggtitle(paste(
    'Partial Mantel correlation test for every different',
    changing[1L]
))
# compare cases
p4 = p4 + geom_hline(
    aes_string(
        yintercept = 'value', colour = 'admixture',
        linetype = 'reference_case'
    ),
    variables$cases[variables$cases$comparison == 'cor', ],
    show_guide = TRUE
)
p4 = p4 + scale_linetype_discrete(
    labels = clean(levels(variables$cases$reference_case)),
    name = 'reference'
)
p4 = p4 + guides(colour = FALSE)

if (variables$debug) {
    print(p4)
}

if (!variables$debug) {
    filename = paste0(variables$now, '-comparisons-', changing[1L])
    for (ext in c('png', 'pdf')) {
        ggsave(
            plot = suppressMessages(arrangeGrob(p3, p4, nrow = 2L)),
            filename = paste(filename, ext, sep = '.'),
            # portrait
            width = variables$short, height = variables$long, units = 'mm'
        )
    }
}

##### X Chromosome - Autosome
pd = position_dodge(0.4)
# plot with error bars
p5 = ggplot(
    summary$diff$aggr,
    aes_string(
        x = changing[1L],
        y = 'diffXAuto', ymax = 'diffXAuto + stddev',
        colour = 'Island',
        group = 'Island'
    )
)
# points
p5 = p5 + geom_point(
    aes_string(shape = 'Island'),
    position = pd, size = variables$pointSize)
if (changing[1L] %in% variables$continuousParams) {
    p5 = p5 + geom_line()
}
# error bars
p5 = p5 + geom_errorbar(
    aes_string(ymin = 'diffXAuto - stddev', ymax = 'diffXAuto + stddev'),
    width = 1L, position = pd, alpha = 0.35
)
# x axis
p5 = p5 + scale_x_discrete(
    name = args$changing,
    labels=clean(as.character(unique(summary$diff$aggr[, changing[1L]])))
)
# y axis
p5 = p5 + scale_y_continuous(
    name = 'XChr - Autosomal admixture',
    limits = c(
        min(-0.2, min(summary$diff$aggr$diffXAuto - summary$diff$aggr$stddev)),
        max(0.2, max(summary$diff$aggr$diffXAuto + summary$diff$aggr$stddev))
    ),
    breaks = seq(-1L, 1L, by = 0.1)
)
# island shapes scale
p5 = p5 + scale_shape_manual(values = c(rep(15L:18L, 5L), 15L))
# theme
p5 = p5 + theme(text = element_text(size = variables$textSize))
# title
p5 = p5 + ggtitle(paste(
    'Difference of admixture between XChr and Autosome',
    'in relation to changes in',
    changing[1L]
))

if (variables$debug) {
    print(p5)
}

if (!variables$debug) {
    filename = paste0(variables$now, '-admixDiff-', changing[1L])
    for (ext in c('png', 'pdf')) {
        ggsave(
            plot = p5,
            filename = paste(filename, ext, sep = '.'),
            # landscape
            width = variables$long, height = variables$short, units = 'mm'
        )
    }
}
# annoying bug, arrangeGrob always opens a Rplots.pdf file
# removes it
unlink('Rplots.pdf')
