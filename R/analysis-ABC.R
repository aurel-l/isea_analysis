
#### ABC analysis

uniqueValues = apply(
    summary$comp$all[, variables$continuousParams],
    MARGIN = 2, FUN = function(c) length(unique(c))
)
changingParams = names(uniqueValues[uniqueValues != 1])

abcData = summary$comp$all[
    ,
    c('randomSeed', changingParams, variables$ABCsummary)
]

# transforms correlation values for the ABC
# [-1, 1] -> [0, 1] (clamping everything below 0 to 0)
abcData$Autosome_cor[abcData$Autosome_cor < 0] = 0
abcData$XChr_cor[abcData$XChr_cor < 0] = 0

# transforms distance values for the ABC
# [0, 1] -> [1, 0] (inverting values, good: 1, bad: 0)
abcData$Autosome_MSD = 1 - abcData$Autosome_MSD
abcData$XChr_MSD = 1 - abcData$XChr_MSD

# compute scores (means)
# only correlation
abcData$score_cor = rowMeans(abcData[, variables$ABC_cor])
# only distance
abcData$score_MSD = rowMeans(abcData[, variables$ABC_MSD])
# correlation AND distance
abcData$score = rowMeans(abcData[, c('score_cor', 'score_MSD')])

# for optimized image ratio, depending on the number of graphs by page
width = if (length(changingParams) > 4) variables$long else variables$short
height = if (length(changingParams) > 4) variables$short else variables$long

nValues = length(abcData$score)

# loops on every kind of score
for (score in c('score_cor', 'score_MSD', 'score')) {
    ordered = abcData[
        order(abcData[, score], decreasing = TRUE),
        c('randomSeed', score, changingParams)
    ]

    # for every tolerance or threshold value
    p1 = list()
    for (i in 1:length(variables$ABC$tVector)) {

        t = variables$ABC$tVector[i]
        if (variables$ABC$type == 'tolerance') {
            nAccepted = round(nValues * t)
            cuttingScore = ordered[nAccepted, score]
            accepted = ordered[, score] > cuttingScore
        } else {
            cuttingScore = 1 - t
            accepted = ordered[, score] > cuttingScore
            nAccepted = sum(accepted)
        }
        accepted = factor(accepted, levels = c(FALSE, TRUE))
        ordered$accepted = accepted

        ## density of scores plot
        p1[[i]] = ggplot(ordered, aes_string(x = score))
        # density line
        p1[[i]] = p1[[i]] + geom_density(fill = 'grey30', colour = 'grey30')
        # accepted part
        p1[[i]] = p1[[i]] + annotate(
            'rect', xmin = cuttingScore, xmax = 1, ymin = 0, ymax = Inf,
            fill = 'green', alpha = 0.1
        )
        # rejected part
        p1[[i]] = p1[[i]] + annotate(
            'rect', xmin = 0, xmax = cuttingScore, ymin = 0, ymax = Inf,
            fill = 'red', alpha = 0.1
        )
        # x scale
        p1[[i]] = p1[[i]] + scale_x_continuous(
            limits = c(0, 1), name = clean(score)
        )
        # title
        p1[[i]] = p1[[i]] + ggtitle(paste0(
            'distribution of ', score, ' (', nAccepted,
            ' accepted simulations. ', variables$ABC$type, '=', t, ')'
        ))


        # density priors and posteriors
        p2 = list()
        # for every changing parameter
        for (i in 1L:length(changingParams)) {
            p2[[i]] = ggplot(
                ordered,
                aes_string(
                    x = changingParams[i], y = '..scaled..'
                )
            )
            # priors
            p2[[i]] = p2[[i]] + geom_density(
                aes(fill = 'prior'),
                alpha = 0.5, adjust = 0.5
            )
            # posteriors
            if (nAccepted > 0) {
                p2[[i]] = p2[[i]] + geom_density(
                    aes(fill = 'posterior'),
                    data = ordered[ordered$accepted == TRUE, ],
                    alpha = 0.5, adjust = 0.5
                )
            }
            # fill colour scale
            p2[[i]] = p2[[i]] + scale_fill_manual(
                name = 'distribution',
                values = c(prior = 'red', posterior = 'green')
            )
            # title
            p2[[i]] = p2[[i]] + ggtitle(paste0(
                'parameter distributions - ', score, '\n(', nAccepted,
                ' accepted simulations. ', variables$ABC$type,'=', t, ')'
            ))
        }
        g2 = do.call('arrangeGrob', c(p2, ncol = 2L))
        # save
        if (variables$debug) {
            print(g2)
        } else {
            filename = paste(
                variables$now, 'distributions', 'densities',
                score, t, sep = '-'
            )
            for (ext in c('png')) {
                ggsave(
                    plot = g2,
                    filename = paste(filename, ext, sep = '.'),
                    # landscape
                    width = variables$long, height = variables$short,
                    units = 'mm'
                )
            }
        }

        # histogram priors and posteriors
        p2 = list()
        # for every changing parameter
        for (i in 1L:length(changingParams)) {
            p2[[i]] = ggplot(
                ordered,
                aes_string(x = changingParams[i], fill = 'accepted'),
            )
            # histogram bars
            p2[[i]] = p2[[i]] + stat_bin(
                breaks = seq(
                    min(ordered[changingParams[i]]),
                    max(ordered[changingParams[i]]),
                    length.out = 30
                ),
                alpha = 0.5
            )
            # fill colour scale
            p2[[i]] = p2[[i]] + scale_fill_manual(
                name = 'distribution',
                labels = c('prior', 'posterior'),
                values = c('red', 'green')
            )
            # title
            p2[[i]] = p2[[i]] + ggtitle(paste0(
                'parameter distributions - ', score, '\n(', nAccepted,
                ' accepted simulations. ', variables$ABC$type, '=', t, ')'
            ))
        }
        g2 = do.call('arrangeGrob', c(p2, ncol = 2L))
        # save
        if (variables$debug) {
            print(g2)
        } else {
            filename = paste(
                variables$now, 'distributions', 'histograms',
                score, t, sep = '-'
            )
            for (ext in c('png')) {
                ggsave(
                    plot = g2,
                    filename = paste(filename, ext, sep = '.'),
                    # landscape
                    width = variables$long, height = variables$short,
                    units = 'mm'
                )
            }
        }

        # exports accepted simulations to csv
        write.table(
            ordered[
                ordered$accepted == TRUE,
                c('randomSeed', score, changingParams)
            ],
            sep = ',',
            file = paste0(
                variables$now, '-accepted-', score, '-', t, '.csv'
            ),
            row.names = FALSE
        )
    }

    # density of scores (dependent on tolerence/threshold values)
    g1 = do.call('arrangeGrob', c(p1, ncol = 2L))
    if (variables$debug) {
        print(g1)
    } else {
        filename = paste0(variables$now, '-accepted-distribution-', score)
        for (ext in c('png')) {
            ggsave(
                plot = g1,
                filename = paste(filename, ext, sep = '.'),
                # landscape
                width = variables$long, height = variables$short, units = 'mm'
            )
        }
    }

    # scatter plot of scores (not dependent on tolerence/threshold values)
    p3 = list()
    # for every changing parameter
    for (i in 1L:length(changingParams)) {
        # for every *next* changing parameter
        for (j in i:length(changingParams)) {
            if (i == j) next# avoids parameter vs same parameter
            # ie: 'param1-param2'
            params = paste(changingParams[i], changingParams[j], sep = '-')
            p3[[params]] = ggplot(
                ordered,
                aes_string(
                    x = changingParams[i], y = changingParams[j], color = score
                )
            )
            # points of the scatter plot
            p3[[params]] = p3[[params]] + geom_point(size = 0.75)
            # colour scale according to type of score (tolerance or threshold)
            if (variables$ABC$type == 'tolerance') {
                p3[[params]] = p3[[params]] + scale_colour_continuous(
                    low = 'white', high = 'black'
                )
            } else {# threshold
                p3[[params]] = p3[[params]] + scale_colour_continuous(
                    low = 'white', high = 'black',
                    limits = c(0, 1)
                )
            }
        }
    }
    g3 = do.call('arrangeGrob', c(p3, ncol = 2L))
    # save
    if (variables$debug) {
        print(g3)
    } else {
        filename = paste0(variables$now, '-scatter-', score)
        #for (ext in c('png', 'pdf')) {
        for (ext in c('png')) {
            ggsave(
                plot = g3,
                filename = paste(filename, ext, sep = '.'),
                # landscape
                width = variables$long, height = variables$short, units = 'mm'
            )
        }
    }
}

# annoying bug, arrangeGrob always opens a Rplots.pdf file
# removes it
unlink('Rplots.pdf')
