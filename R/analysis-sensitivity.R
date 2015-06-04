

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
    name = 'type of admixture',
    labels = c(
        'Whole DNA (52m)', 'Auto (25m)', 'X Chr (25m)',
        'Mito (1m)', 'Y Chr (1m)'
    )
)
# x axis
p1 = p1 + scale_x_discrete(name = 'Island')
# y axis
p1 = p1 + scale_y_continuous(name = 'standard deviation of admixture')
# theme
p1 = p1 + theme(text = element_text(size = variables$textSize))
# title
p1 = p1 + ggtitle(paste('Sensitivity of admixture to', changing[1L], 'by zone'))

if (variables$debug) {
    print(p1)
}

##### plot counts
# histogram
p2 = ggplot(summary$counts$df, aes_string(x = changing[1L], y = 'count'))
p2 = p2 + geom_bar(fill = 'white', colour = 'black', stat='identity', width = 0.5)
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
p2 = p2 + ggtitle(paste('Count of simulations for every different', args$changing[1L]))

if (variables$debug) {
    print(p2)
}

if (!variables$debug) {
    png(
        paste0(
            variables$now,
            '-sensitivity-',
            changing[1L],
            '.png'),
        width = 1754L, height = 1240L
    )
    grid.arrange(p1, p2, nrow = 2L)
    graphics.off()
}

