
#suppressMessages(library(abctools))
#library(GGally)

uniqueValues = apply(
    summary$comp$all[, variables$continuousParams],
    MARGIN = 2, FUN = function(c) length(unique(c))
)
changingParams = names(uniqueValues[uniqueValues != 1])

abcData = summary$comp$all[
    ,
    #c(changingParams, variables$ABCsummary)
    c('randomSeed', changingParams, variables$ABCsummary)
]

# transforms correlation values for the ABC
# [-1, 1] -> [0, 1] (clamping everything below 0 to 0)
# [0, 1] -> [1, 0] (inverting values, good: 0, bad: 1)
abcData$Autosome_cor[abcData$Autosome_cor < 0] = 0
abcData$Autosome_cor = 1 - abcData$Autosome_cor
abcData$XChr_cor[abcData$XChr_cor < 0] = 0
abcData$XChr_cor = 1 - abcData$XChr_cor
# compute scores (means)
abcData$score_cor = rowMeans(abcData[, variables$ABC_cor])
abcData$score_MSD = rowMeans(abcData[, variables$ABC_MSD])
abcData$score = rowMeans(abcData[, c('score_cor', 'score_MSD')])

# for image ratio
width = if (length(changingParams) > 4) variables$long else variables$short
height = if (length(changingParams) > 4) variables$short else variables$long

nValues = length(abcData$score)

for (score in c('score_cor', 'score_MSD', 'score')) {
#for (score in c('score')) {
    ordered = abcData[
        order(abcData[, score], decreasing = TRUE),
        c('randomSeed', score, changingParams)
    ]

    p1 = list()
    for (i in 1:length(variables$ABC$taus)) {

        tau = variables$ABC$taus[i]
        if (variables$ABC$type == 'relative') {
            nAccepted = round(nValues * tau)
            cuttingScore = ordered[nValues - nAccepted, score]
            accepted = c(rep(FALSE, nValues - nAccepted), rep(TRUE, nAccepted))
        } else {
            cuttingScore = tau
            accepted = ordered[, score] < tau
            nAccepted = sum(accepted)
        }
        accepted = factor(accepted, levels = c(FALSE, TRUE))
        ordered$accepted = accepted

        # density of scores
        p1[[i]] = ggplot(ordered, aes_string(x = score))
        p1[[i]] = p1[[i]] + geom_density(fill = 'grey30', colour = 'grey30')
        p1[[i]] = p1[[i]] + annotate(
            'rect', xmin = 0, xmax = cuttingScore, ymin = 0, ymax = Inf,
            fill = 'green', alpha = 0.1
        )
        p1[[i]] = p1[[i]] + annotate(
            'rect', xmin = cuttingScore, xmax = 1, ymin = 0, ymax = Inf,
            fill = 'red', alpha = 0.1
        )
        p1[[i]] = p1[[i]] + scale_x_continuous(
            limits = c(0, 1), name = clean(score)
        )
        p1[[i]] = p1[[i]] + ggtitle(paste0(
            'distribution of ', score, ' (', nAccepted,
            ' accepted simulations. τ=', tau, '. ', variables$ABC$type, ')'
        ))


        # density priors and posteriors
        p2 = list()
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
            p2[[i]] = p2[[i]] + geom_density(
                aes(fill = 'posterior'),
                data = ordered[ordered$accepted == TRUE, ],
                alpha = 0.5, adjust = 0.5
            )
            p2[[i]] = p2[[i]] + scale_fill_manual(
                name = 'distribution',
                values = c(prior = 'red', posterior = 'green')
            )
            p2[[i]] = p2[[i]] + ggtitle(paste0(
                'parameter distributions - ', score, '\n(', nAccepted,
                ' accepted simulations. τ=', tau, '. ', variables$ABC$type, ')'
            ))
        }
        g2 = do.call('arrangeGrob', c(p2, ncol = 2L))
        if (variables$debug) {
            print(g2)
        } else {
            filename = paste(
                variables$now, 'distributions', score, tau, sep = '-'
            )
            #for (ext in c('png', 'pdf')) {
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
                variables$now, '-accepted-', score, '-', tau, '.csv'
            ),
            row.names = FALSE
        )
    }

    # density of scores
    g1 = do.call('arrangeGrob', c(p1, ncol = 2L))
    if (variables$debug) {
        print(g1)
    } else {
        filename = paste0(variables$now, '-accepted-distribution-', score)
        #for (ext in c('png', 'pdf')) {
        for (ext in c('png')) {
            ggsave(
                plot = g1,
                filename = paste(filename, ext, sep = '.'),
                # landscape
                width = variables$long, height = variables$short, units = 'mm'
            )
        }
    }

    p3 = list()
    for (i in 1L:length(changingParams)) {
        for (j in i:length(changingParams)) {
            if (i == j) next
            params = paste(changingParams[i], changingParams[j], sep = '-')
            p3[[params]] = ggplot(
                ordered,
                aes_string(
                    x = changingParams[i], y = changingParams[j]
                )
            )
            p3[[params]] = p3[[params]] + geom_point(
                aes_string(colour = score), size = 0.01
            )
            if (variables$ABC$type == 'relative') {
                p3[[params]] = p3[[params]] + scale_colour_continuous(
                    low = 'black', high = 'white'
                )
            } else {# absolute
                p3[[params]] = p3[[params]] + scale_colour_continuous(
                    low = 'black', high = 'white',
                    limits = c(0, 1)
                )
            }
        }
    }
    g3 = do.call('arrangeGrob', c(p3, ncol = 2L))
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

# nSteps = 5
# #steps = seq(1.0, 1.0 / nSteps, -(1.0 / nSteps))
# # steps = c(1.0, 0.5, 0.25, 0.10, 0.05)
# # nSteps = length(steps)
# unadj = list()
# # for (i in length(steps):1) {
# #     result = abc(
# #         target, param, sumstat, tol = steps[i], method = 'rejection'
# #     )
# #     unadj[[i]] = data.frame(result$unadj.values)
# #     unadj[[i]]$fill = rgb(steps[i], 1 - steps[i] + (1.0 / nSteps), 0)
# #     unadj[[i]]$alpha = 1 - steps[i] + (1.0 / nSteps)
# # }
# result = abc(
#     target, param, sumstat, tol = 1, method = 'rejection'
# )
# unadj[[1]] = data.frame(result$unadj.values)
# unadj[[1]]$type = 'prior'
#
# result = abc(
#     target, param, sumstat, tol = 0.05, method = 'rejection'
# )
# unadj[[2]] = data.frame(result$unadj.values)
# unadj[[2]]$type = 'posterior (5%)'
#
# unadj = do.call('rbind', unadj)
# unadj$type = factor(unadj$type)
#
# # subsetParams = result$unadj.values[, changingParams]
# # posterior = data.frame(subsetParams)
# # posterior$id = rownames(subsetParams)
# # melted = melt(posterior, id.vars = 'id')
#
# # plots = list()
# # for (i in 1:length(changingParams)) {
# #     xMin = min(param[, changingParams[i]])
# #     xMax = max(param[, changingParams[i]])
# #     plots[[i]] = ggplot(
# #         unadj[, c(changingParams[i], 'type')],
# #         aes_string(
# #             # x = changingParams[i], y = '(..count..)/sum(..count..)',
# #             x = changingParams[i], y = '..count..',# fill = 'fill',
# #             xmin = xMin, xmax = xMax, alpha = 'alpha'
# #         )
# #     )
# #     plots[[i]] = plots[[i]] + geom_histogram(
# #         position = 'identity',
# #         #alpha = .2,
# #         breaks = seq(xMin, xMax, length.out = 21)
# #     )
# #     #plots[[i]] = plots[[i]] + geom_density(alpha = .2)
# #     plots[[i]] = plots[[i]] + scale_fill_manual(
# #         name = 'accepted',
# #         values = c('green', 'red'),
# #         labels = c('5%', '100%')
# #     )
# #     plots[[i]] = plots[[i]] + scale_alpha_continuous(
# #         guide = 'none'
# #     )
# #     for (j in 1:length(unadj)) {
# #         plots[[i]] = plots[[i]] + geom_histogram(
# #             data = unadj[[j]][changingParams[i]],
# #             #alpha = 1 - steps[j] + (1.0 / nSteps),
# #             fill = rgb(steps[j], 1 - steps[j] + (1.0 / nSteps), 0),
# #             breaks = seq(xMin, xMax, length.out = 20)
# #         )
# #     }
#     # plots[[i]] = plots[[i]] + geom_density(kernel = 'epanechnikov')
# #}
# # maxY = max(unlist(
# #     lapply(plots, FUN = function(p) max(ggplot_build(p)$data[[1]]$ymax))
# # ))
# # for (i in 1:length(plots)) {
# #     plots[[i]] = plots[[i]] + ylim(0, maxY)
# # }
# # do.call('grid.arrange', c(plots, ncol = 2))
# p = ggpairs(
#     unadj[, c(changingParams, 'type')], color = 'type',
#     params = list(notch = TRUE)
# )
# if (variables$debug) {
#     print(p)
# }
#
# if (!variables$debug) {
#     filename = paste0(variables$now, '-abc')
#     for (ext in c('png', 'pdf')) {
#         ggsave(
#             plot = p,
#             filename = paste(filename, ext, sep = '.'),
#             # landscape
#             width = variables$long, height = variables$short, units = 'mm'
#         )
#     }
# }
