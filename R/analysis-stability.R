
# drop DnaAdmixture information
summary$sensit$all = summary$sensit$all[
    summary$sensit$all$variable != 'DnaAdmixture',
]
summary$sensit$aggr = summary$sensit$aggr[
    summary$sensit$aggr$variable != 'DnaAdmixture',
]

##### sensitivity plot
summary$sensit$all$variable = factor(
    summary$sensit$all$variable,
    levels = variables$orderedAdmixtures
)
# boxplots
p1 = ggplot(
    summary$sensit$all,
    aes_string(x = 'Island', y = 'value', colour = 'variable')
)
p1 = p1 + geom_boxplot(notch = TRUE)
# facets
p1 = p1 + facet_wrap(~ variable, ncol = 2L)
# x axis
p1 = p1 + scale_x_discrete(name = 'Island')
# y axis
p1 = p1 + scale_y_continuous(name = 'admixture', limits = c(0L, 1L))
# theme
p1 = p1 + theme(text = element_text(size = variables$textSize))
p1 = p1 + guides(colour = FALSE)
# title
p1 = p1 + ggtitle(
    paste('Admixture by Island for', maxCount, 'simulations')
)

if (variables$debug) {
    print(p1)
}
if (!variables$debug) {
    filename = paste0(variables$now, '-stability.png')
    for (ext in c('png', 'pdf')) {
        ggsave(
            plot = p1,
            filename = paste(filename, ext, sep = '.'),
            # landscape
            width = variables$long, height = variables$short, units = 'mm'
        )
    }
}

##### admix gradient plot (long chair)
summary$sensit$aggr = aggregate(
    . ~ Island + variable,
    data = summary$sensit$all,
    FUN = mean
)
summary$sensit$aggr$variable = factor(
    summary$sensit$aggr$variable,
    levels = variables$orderedAdmixtures
)
summary$sensit$aggr$stddev = aggregate(
    . ~ Island + variable,
    data = summary$sensit$all,
    FUN = sd
)$value
pd = position_dodge(0.2)
# plot
p2 = ggplot(
    summary$sensit$aggr,
    aes_string(
        x = 'variable',
        y = 'value', ymax = 'value + stddev',
        colour = 'Island'
    )
)
p2 = p2 + geom_point(aes_string(shape = 'Island'), position = pd, size = 5L)
p2 = p2 + geom_errorbar(
    aes_string(ymin = 'value - stddev', ymax = 'value + stddev'),
    position = pd, alpha = 0.35
)
p2 = p2 + geom_line(aes_string(group = 'Island'), alpha = 0.35)
p2 = p2 + scale_shape_manual(values = c(rep(15L:18L, 5L), 15L))
# x axis
p2 = p2 + scale_x_discrete(name = 'admixture type')
# y axis
p2 = p2 + scale_y_continuous(
    name = 'admixture value',
    breaks = seq(0L, 1L, by = 0.1)
)
# theme
p2 = p2 + theme(text = element_text(size = variables$textSize))
# title
p2 = p2 + ggtitle(
    paste('Admixture by type for', maxCount, 'simulations')
)

if (variables$debug) {
    print(p2)
}
if (!variables$debug) {
    filename = paste0(variables$now, '-stability-admixGradient.png')
    for (ext in c('png', 'pdf')) {
        ggsave(
            plot = p2,
            filename = paste(filename, ext, sep = '.'),
            # landscape
            width = variables$long, height = variables$short, units = 'mm'
        )
    }
}
# annoying bug, arrangeGrob always opens a Rplots.pdf file
# removes it
unlink('Rplots.pdf')
