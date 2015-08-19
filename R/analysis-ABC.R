
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

nValues = length(sumstat$score)

#for (score in c('score_cor', 'score_MSD', 'score')) {
for (score in c('score')) {
    ordered = abcData[
        order(abcData[, score], decreasing = TRUE),
        c(score, changingParams)
    ]

    if (rejectionType == 'relative') {
        nAccepted = round(nValues * tau)
        cuttingScore = ordered[nAccepted, score]
        accepted = c(rep(TRUE, nAccepted), rep(FALSE, nValues - nAccepted))
    } else {
        cuttingScore = tau
        accepted = ordered[, score] < tau
        nAccepted = sum(accepted)
    }
    accepted = factor(accepted, levels = c(FALSE, TRUE))
    ordered$accepted = accepted

    # density of scores
    p1 = ggplot(ordered, aes_string(x = score))
    p1 = p1 + geom_density(fill = 'grey30', colour = 'grey30')
    p1 = p1 + annotate(
        'rect', xmin = 0, xmax = cuttingScore, ymin = 0, ymax = Inf,
        fill = 'green', alpha = 0.1
    )
    p1 = p1 + annotate(
        'rect', xmin = cuttingScore, xmax = 1, ymin = 0, ymax = Inf,
        fill = 'red', alpha = 0.1
    )
    p1 = p1 + scale_x_continuous(limits = c(0, 1), name = clean(score))
    p1 = p1 + ggtitle(paste0(
        'distribution of ', score, ' (', nAccepted,
        ' accepted simulations. τ=', tau, '. ', rejectionType, ')'
    ))
    if (variables$debug) {
        print(p1)
    } else {
        filename = paste0(variables$now, '-accepted-distribution-', score)
        for (ext in c('png', 'pdf')) {
            ggsave(
                plot = p1,
                filename = paste(filename, ext, sep = '.'),
                # landscape
                width = variables$long, height = variables$short, units = 'mm'
            )
        }
    }

    # density priors and posteriors
    p2 = list()
    for (i in 1L:length(changingParams)) {
        p2[[i]] = ggplot(ordered, aes_string(x = changingParams[i], y = '..density..', fill = 'accepted'))
        p2[[i]] = p2[[i]] + geom_density(position = 'identity')
        #p2[[i]] = p2[[i]] + geom_density(
        #    data = ordered$param[accepted, ]
        #)
    }
    g2 = do.call('arrangeGrob', c(p2, ncol = 2L))

    if (variables$debug) {
        print(g2)
    } else {
        filename = paste0(variables$now, '-distributions-', score)
        for (ext in c('png', 'pdf')) {
            ggsave(
                plot = g2,
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
                aes_string(alpha = score), size = 1, colour = 'black'
            )
            if (variables$ABC$type == 'relative') {
                p3[[params]] = p3[[params]] + scale_alpha_continuous(
                    range = c(1, 0)
                )
            } else {# absolute
                p3[[params]] = p3[[params]] + scale_alpha_continuous(
                    low = 'black', high = 'white',
                    limits = c(0, 1)
                )
            }
#             if (variables$ABC$type == 'relative') {
#                 p3[[params]] = p3[[params]] + scale_colour_continuous(
#                     low = 'black', high = 'white'
#                 )
#             } else {# absolute
#                 p3[[params]] = p3[[params]] + scale_colour_continuous(
#                     low = 'black', high = 'white',
#                     limits = c(0, 1)
#                 )
#             }
        }
    }
    g3 = do.call('arrangeGrob', c(p3, ncol = 2L))
    #g3 = ggplot(melted, aes(score, score))
    #g3 = g3 + geom_point(aes_string(colour = score))
    #g3 = g3 + facet_grid(variable ~ variable)
    if (variables$debug) {
        print(g3)
    } else {
        filename = paste0(variables$now, '-scatter-', score)
        for (ext in c('png', 'pdf')) {
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
