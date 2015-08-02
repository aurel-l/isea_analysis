
suppressMessages(library(abctools))

target = c(0, 0, 1, 1)
param = summary$comp$all[, variables$continuousParams]
#for (p in variables$discreteParams) {
#    param[, p] = as.numeric(factor(param[, p]))
#}
sumstat = summary$comp$all[, variables$ABCsummary]

uniqueValues = apply(param, MARGIN = 2, FUN = function(c) length(unique(c)))
changingParams = names(uniqueValues[uniqueValues != 1])

nSteps = 5
#steps = seq(1.0, 1.0 / nSteps, -(1.0 / nSteps))
# steps = c(1.0, 0.5, 0.25, 0.10, 0.05)
# nSteps = length(steps)
unadj = list()
# for (i in length(steps):1) {
#     result = abc(
#         target, param, sumstat, tol = steps[i], method = 'rejection'
#     )
#     unadj[[i]] = data.frame(result$unadj.values)
#     unadj[[i]]$fill = rgb(steps[i], 1 - steps[i] + (1.0 / nSteps), 0)
#     unadj[[i]]$alpha = 1 - steps[i] + (1.0 / nSteps)
# }
result = abc(
    target, param, sumstat, tol = 1, method = 'rejection'
)
unadj[[1]] = data.frame(result$unadj.values)
unadj[[1]]$type = 'prior'

result = abc(
    target, param, sumstat, tol = 0.05, method = 'rejection'
)
unadj[[2]] = data.frame(result$unadj.values)
unadj[[2]]$type = 'posterior (5%)'

unadj = do.call('rbind', unadj)
unadj$type = factor(unadj$type)

# subsetParams = result$unadj.values[, changingParams]
# posterior = data.frame(subsetParams)
# posterior$id = rownames(subsetParams)
# melted = melt(posterior, id.vars = 'id')

plots = list()
for (i in 1:length(changingParams)) {
    xMin = min(param[, changingParams[i]])
    xMax = max(param[, changingParams[i]])
    plots[[i]] = ggplot(
        unadj[, c(changingParams[i], 'posterior')],
        aes_string(
            # x = changingParams[i], y = '(..count..)/sum(..count..)',
            x = changingParams[i], y = '..count..', fill = 'fill',
            xmin = xMin, xmax = xMax, alpha = 'alpha'
        )
    )
    plots[[i]] = plots[[i]] + geom_histogram(
        position = 'identity',
        #alpha = .2,
        breaks = seq(xMin, xMax, length.out = 21)
    )
    #plots[[i]] = plots[[i]] + geom_density(alpha = .2)
    plots[[i]] = plots[[i]] + scale_fill_manual(
        name = 'accepted',
        values = c('green', 'red'),
        labels = c('5%', '100%')
    )
    plots[[i]] = plots[[i]] + scale_alpha_continuous(
        guide = 'none'
    )
#     for (j in 1:length(unadj)) {
#         plots[[i]] = plots[[i]] + geom_histogram(
#             data = unadj[[j]][changingParams[i]],
#             #alpha = 1 - steps[j] + (1.0 / nSteps),
#             fill = rgb(steps[j], 1 - steps[j] + (1.0 / nSteps), 0),
#             breaks = seq(xMin, xMax, length.out = 20)
#         )
#     }
    # plots[[i]] = plots[[i]] + geom_density(kernel = 'epanechnikov')
}
maxY = max(unlist(
    lapply(plots, FUN = function(p) max(ggplot_build(p)$data[[1]]$ymax))
))
for (i in 1:length(plots)) {
    plots[[i]] = plots[[i]] + ylim(0, maxY)
}
do.call('grid.arrange', c(plots, ncol = 2))
