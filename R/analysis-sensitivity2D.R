
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
p1 = p1 + ggtitle(
    paste(
        'Sensitivity of admixture to',
        changing[2L], 'and', changing[1L],
        'by zone'
    )
)

if (variables$debug) {
    print(p1)
}

##### plot counts
# heat map
p2 = ggplot(
    summary$counts$df,
    aes_string(
        x = changing[1L],
        y = changing[2L],
        fill = 'count'
    )
)
p2 = p2 + geom_raster()
p2 = p2 + geom_text(aes_string(label = 'count'))
# "z" axis (color)
p2 = p2 + scale_fill_gradient(
    name = 'count', limits = c(0L, maxCount),
    breaks = c(
        0L, ceiling(maxCount / 5), ceiling(2 * maxCount / 5),
        ceiling(3 * maxCount / 5), ceiling(4 * maxCount / 5), maxCount
    ),
    guide = guide_colorbar(
        draw.ulim = FALSE, draw.llim = FALSE, barheight = 18L
    )
)
# x axis
p2 = p2 + scale_x_discrete(
    name = changing[1], expand = c(0L, 0L)#,
    #limits = c(min(summary$counts$df[, changing[1]]), max(summary$counts$df[, changing[1]])),
    #labels = clean(levels(summary$counts$df[, changing[1]]))
)
# y axis
p2 = p2 + scale_y_discrete(
    name = changing[2], expand = c(0L, 0L)#,
    #labels = clean(levels(summary$counts$df[, changing[2]]))
)
# theme
p2 = p2 + theme(text = element_text(size = variables$textSize))
# title
p2 = p2 + ggtitle('Count of simulations for every changing parameter')

if (variables$debug) {
    print(p2)
}

if (!variables$debug) {
    filename = paste0(
        variables$now, '-sensitivity-',
        changing[1], '-', changing[2L]
    )
    png(
        paste0(filename, '.png'),
        width = 1240L, height = 1754L
    )
    grid.arrange(p1, p2, nrow = 2L)
    graphics.off()
    postscript(
        paste0(filename, '.ps'),
        width = 12L, height = 17L
    )
    grid.arrange(p1, p2, nrow = 2L)
    graphics.off()
}

##### plot comparisons
# heat map MSD means
p3 = ggplot(
    summary$comp$aggr[summary$comp$aggr$comparison == 'MSD', ],
    aes_string(
        x = changing[1L],
        y = changing[2L],
        fill = 'value'
    )
)
p3 = p3 + geom_raster()
# facets
p3 = p3 + facet_wrap(~ admixture)
# "z" axis (color)
p3 = p3 + scale_fill_gradientn(
    name = 'MSD\n',
    colours = c('green', 'red', 'red', 'red', 'black'), limits = c(0L, 1L),
    guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
)
# x axis
p3 = p3 + scale_x_discrete(
    name = changing[1], expand = c(0L, 0L)#,
    #limits = c(min(summary$counts$df[, changing[1]]), max(summary$counts$df[, changing[1]])),
    #labels = clean(levels(summary$counts$df[, changing[1]]))
)
# y axis
p3 = p3 + scale_y_discrete(
    name = changing[2], expand = c(0L, 0L)#,
    #labels = clean(levels(summary$counts$df[, changing[2]]))
)
# theme
p3 = p3 + theme(text = element_text(size = variables$textSize))
# title
p3 = p3 + ggtitle(paste0(
    'Mean of squared distances of admixtures in relation to\n',
    changing[1L], ' and ', changing[2L]
))

if (variables$debug) {
    print(p3)
}

# heat map MSD standard deviations
p4 = ggplot(
    summary$comp$aggr[summary$comp$aggr$comparison == 'MSD', ],
    aes_string(
        x = changing[1L],
        y = changing[2L],
        fill = 'sd'
    )
)
p4 = p4 + geom_raster()
# facets
p4 = p4 + facet_wrap(~ admixture)
# "z" axis (color)
p4 = p4 + scale_fill_gradient(
    name = 'MSD stddev\n', high = 'red', low = 'green'
)
# x axis
p4 = p4 + scale_x_discrete(
    name = changing[1], expand = c(0L, 0L)#,
    #limits = c(min(summary$counts$df[, changing[1]]), max(summary$counts$df[, changing[1]])),
    #labels = clean(levels(summary$counts$df[, changing[1]]))
)
# y axis
p4 = p4 + scale_y_discrete(
    name = changing[2], expand = c(0L, 0L)#,
    #labels = clean(levels(summary$counts$df[, changing[2]]))
)
# theme
p4 = p4 + theme(text = element_text(size = variables$textSize))
# title
p4 = p4 + ggtitle(paste0(
    'Standard deviation of MSD of admixtures in relation to\n',
    changing[1L], ' and ', changing[2L]
))

if (variables$debug) {
    print(p4)
}

# heat map correlation means
p5 = ggplot(
    summary$comp$aggr[summary$comp$aggr$comparison == 'cor', ],
    aes_string(
        x = changing[1L],
        y = changing[2L],
        fill = 'value'
    )
)
p5 = p5 + geom_raster()
# facets
p5 = p5 + facet_wrap(~ admixture)
# "z" axis (color)
p5 = p5 + scale_fill_gradientn(
    name = 'Partial Mantel\ncorrelation\n',
    colours = c('blue', 'red', 'green'), limits = c(-1, 1),
    guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
)
# x axis
p5 = p5 + scale_x_discrete(
    name = changing[1], expand = c(0L, 0L)#,
    #limits = c(min(summary$counts$df[, changing[1]]), max(summary$counts$df[, changing[1]])),
    #labels = clean(levels(summary$counts$df[, changing[1]]))
)
# y axis
p5 = p5 + scale_y_discrete(
    name = changing[2], expand = c(0L, 0L)#,
    #labels = clean(levels(summary$counts$df[, changing[2]]))
)
# theme
p5 = p5 + theme(text = element_text(size = variables$textSize))
# title
p5 = p5 + ggtitle(paste0(
    'Partial Mantel correlation test in relation to\n',
    changing[1L], ' and ', changing[2L]
))

if (variables$debug) {
    print(p5)
}

# heat map correlation standard deviations
p6 = ggplot(
    summary$comp$aggr[summary$comp$aggr$comparison == 'cor', ],
    aes_string(
        x = changing[1L],
        y = changing[2L],
        fill = 'sd'
    )
)
p6 = p6 + geom_raster()
# facets
p6 = p6 + facet_wrap(~ admixture)
# "z" axis (color)
p6 = p6 + scale_fill_gradient(
    name = 'MSD stddev\n', high = 'red', low = 'green'
)
# x axis
p6 = p6 + scale_x_discrete(
    name = changing[1], expand = c(0L, 0L)#,
    #limits = c(min(summary$counts$df[, changing[1]]), max(summary$counts$df[, changing[1]])),
    #labels = clean(levels(summary$counts$df[, changing[1]]))
)
# y axis
p6 = p6 + scale_y_discrete(
    name = changing[2], expand = c(0L, 0L)#,
    #labels = clean(levels(summary$counts$df[, changing[2]]))
)
# theme
p6 = p6 + theme(text = element_text(size = variables$textSize))
# title
p6 = p6 + ggtitle(paste0(
    'Standard deviation of Partial Mantel correlation test in relation to\n',
    changing[1L], ' and ', changing[2L]
))

if (variables$debug) {
    print(p6)
}

if (!variables$debug) {
    filename = paste0(
        variables$now, '-comparisons-',
        changing[1L], '-', changing[2L]
    )
    png(
        paste0(filename, '.png'),
        width = 1240L, height = 1754L
    )
    grid.arrange(p3, p4, p5, p6, nrow = 4L)
    graphics.off()
    svg(
        paste0(filename, '.svg'),
        width = 12L, height = 17L,
        onefile = TRUE
    )
    grid.arrange(p3, p4, p5, p6, nrow = 4L)
    graphics.off()
}
