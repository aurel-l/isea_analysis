# module to be sourced

# libraries
suppressMessages(library(argparse))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
#suppressMessages(library(matrixStats))
suppressMessages(library(parallel))
#suppressMessages(library(Rcpp))
suppressMessages(library(reshape2))
suppressMessages(library(XML))

#variables
variables = new.env()
variables$debug = FALSE
variables$observedIslands = c(
        'Chin', 'Viet', 'Mala', 'Taiw', 'Phil', 'Suma', 'Java', 'Born',
        'Bali', 'Sula', 'Sumb', 'Halm', 'Flor', 'Timo', 'Alor', 'Papu'
    )
variables$summaryNames = c(
        'DnaAdmixture',
        'AutosomeAdmixture', 'XChrAdmixture',
        'MitoAdmixture', 'YChrAdmixture'
    )
variables$continuousParams = c(
        'migrationProb', 'poissonMean', 'marriageThres',
        'growthRate', 'initialDemeAgentNumber'
    )
variables$discreteParams = c(
        'startingDistributionFile', 'graphFile'
    )
variables$now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')
variables$cores = detectCores()
variables$nDemes = 118L
variables$nIslands = 21L
variables$toleratedPopRatio = 0.1
variables$toleratedDeadDemes = 0.25
variables$permutations = 1L
variables$textSize = 30L

# do not change
variables$paramNames = c(variables$continuousParams, variables$discreteParams)

# add column types here
variables$types = new.env()
variables$types$DnaAdmixture = 'numeric'
variables$types$AutosomeAdmixture = 'numeric'
variables$types$XChrAdmixture = 'numeric'
variables$types$MitoAdmixture = 'numeric'
variables$types$YChrAdmixture = 'numeric'
variables$types$DemeSize = 'integer'
variables$types$migrationProb = 'numeric'
variables$types$poissonMean = 'numeric'
variables$types$marriageThres = 'numeric'
variables$types$growthRate = 'numeric'
variables$types$asianDefinition = 'numeric'
variables$types$initialDemeAgentNumber = 'integer'
variables$types$startingDistributionFile = 'character'
variables$types$graphFile = 'character'
variables$types$tick = 'numeric'
variables$types$Label = 'character'
variables$types$Island = 'character'
variables$types$run = 'integer'
variables$types$randomSeed = 'integer'
# end of column types

sweepParams = function(df) {
    outTable = data.frame(
        values = rep(0, length(variables$paramNames)),
        discrete = rep(FALSE, length(variables$paramNames))
    )
    rownames(outTable) = variables$paramNames

    outCounts = c()
    outNames = c()
    for (p in variables$paramNames) {
        uniqueValues = unique(df[, p])
        outTable[p, 'values'] = length(uniqueValues)
        outTable[p, 'discrete'] = (
            p %in% variables$discreteParams
        )
        if (length(uniqueValues) > 1) {
            outCounts = c(outCounts, length(uniqueValues))
            outNames = c(outNames, p)
        }
    }

    return(list(
        table = outTable,
        names = outNames[order(-outCounts)],
        changing = length(outNames),
        sets = prod(outTable$values)
    ))
}

toXMLFile = function(df, debug = FALSE) {
    root = newXMLNode('sweep')
    sets = 1
    for (p in variables$paramNames) {
        values = sort(unique(df[, p]))
        newXMLNode(
            'parameter',
            attrs = c(
                'name' = p,
                'n' = length(values),
                'values' = toString(values)
            ),
            parent = root
        )
        sets = sets * length(values)
    }
    addAttributes(root, sets = sets)
    if (debug) {
        print(root)
    } else {
        invisible(saveXML(root, paste0(variables$now, '-parameters.xml')))
    }
}

# cleans the vector of names of useless information
clean = compiler::cmpfun(function(names) {
    out = gsub(
        '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
        names
    )
    out = gsub(
        '_', '\n',
        out
    )
    return(out)
})

counts = function(df, changing) {
    countFrame = aggregate(
        randomSeed ~ .,
        df[, c('randomSeed', changing)],
        FUN = function(x) length(unique(x))
    )
    #print(summary(countFrame))
    for (p in changing) {
        countFrame[, p] = factor(countFrame[, p])
    }

    if (length(changing) == 1) {
        p = ggplot(
            countFrame,
            aes_string(x = changing, y = 'randomSeed')
        )
        p = p + geom_bar(fill = 'white', colour = 'black', width = 0.5)
        p = p + scale_y_continuous(name = 'simulation count', expand = c(0, 0))
    } else {# 2 changing parameters
        p = ggplot(
            countFrame,
            aes_string(
                x = changing[1],
                y = changing[2]
            )
        )
        p = p + geom_tile(aes_string(fill = 'randomSeed'))
        p = p + scale_fill_gradient(
            name = 'count', limits = c(0, max(countFrame$randomSeed)),
            guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
        )
        #if (changing[2] %in% variables$discreteParams) {
        p = p + scale_y_discrete(
            name = changing[2], expand = c(0, 0),
            labels = clean(levels(countFrame[, changing[2]]))
        )
        #} else {
        #    p = p + scale_y_continuous(name = changing[2], expand = c(0, 0))
        #}
    }
    #if (changing[1] %in% variables$discreteParams) {
    p = p + scale_x_discrete(
        name = changing[1], expand = c(0, 0),
        labels = clean(levels(countFrame[, changing[1]]))
    )
    #} else {
    #    p = p + scale_x_continuous(name = changing[1], expand = c(0, 0))
    #}
    #p = p + scale_x_continuous(name = changing[1], expand = c(0, 0))
    p = p + theme(text = element_text(size = variables$textSize))
    p = p + ggtitle('Count of simulations for every changing parameter')

    return(p)
}

stability = function(df, changing) {
    aggregated = aggregate(
        . ~ Island,
        data = df,
        FUN = sd
    )
    aggregated = merge(
        aggregated[, c(variables$summaryNames, 'Island')],
        imports$order,
        by = 'Island'
    )
    aggregated = aggregated[order(aggregated['order']), ]
    aggregated$Island = factor(aggregated$Island, levels=aggregated$Island)

    melted = melt(
        aggregated[, c('Island', variables$summaryNames)],
        id = 'Island'
    )
    melted$Island = factor(melted$Island, unique(melted$Island))

    p = ggplot(
        melted,
        aes(x = Island, y = value, colour = variable)
    )
    p = p + theme(text = element_text(size = variables$textSize))
    p = p + geom_errorbar(aes(y = value, ymin = value, ymax = value))
    p = p + scale_color_discrete(
        name = 'type of admixture',
        labels = c(
            'Whole DNA (52m)', 'Auto (25m)', 'X Chr (25m)',
            'Mito (1m)', 'Y Chr (1m)'
        )
    )
    p = p + scale_x_discrete(name = 'zone')
    p = p + scale_y_continuous(name = 'standard deviation of admixture')
    p = p + ggtitle(
        paste(
            'Sensitivity of admixture to',
            toString(changing),
            'by zone'
        )
    )

    return(p)
}

# append = compiler::cmpfun(function(l, item) {
#     l[[length(l) + 1]] = item
#     return(l)
# })

sensitivity = function(df, changing) {
    melted = list(
        mean = list(
            MSD = melt(
                df$mean[, c('AutoMSD', 'XChrMSD', changing)],
                id = changing
            ),
            cor = melt(
                df$mean[, c('AutoCor', 'XChrCor', changing)],
                id = changing
            )
        ),
        stddev = list(
            MSD = melt(
                df$stddev[, c('AutoMSD', 'XChrMSD', changing)],
                id = changing
            ),
            cor = melt(
                df$stddev[, c('AutoCor', 'XChrCor', changing)],
                id = changing
            )
        )
    )

    if (length(changing) == 1) {

    } else {# at least 2 parameters changing
        p1 = ggplot(
            melted$mean$MSD,
            aes_string(x = changing[1], y = changing[2])
        )
        p1 = p1 + theme(text = element_text(size = variables$textSize))
        p1 = p1 + geom_tile(aes_string(fill = 'value'))
        p1 = p1 + facet_wrap(~ variable)
        p1 = p1 + scale_x_discrete(
            name = changing[1], expand = c(0, 0),
            labels = clean(levels(factor(merged[, changing[1]])))
        )
        p1 = p1 + scale_y_discrete(
            name = changing[2], expand = c(0, 0),
            labels = clean(levels(merged[, changing[2]]))
        )
        p1 = p1 + scale_fill_gradientn(
            name = 'MSD\n',
            colours = c('green', 'red', 'red', 'red', 'black'), limits = c(0, 1),
            guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
        )
        p1 = p1 + ggtitle(
            paste(
                'Mean of squared distances of admixtures in relation to\n',
                toString(changing)
            )
        )
        p3 = ggplot(
            melted$stddev$MSD,
            aes_string(x = changing[1], y = changing[2])
        )
        p3 = p3 + theme(text = element_text(size = variables$textSize))
        p3 = p3 + geom_tile(aes_string(fill = 'value'))
        p3 = p3 + facet_wrap(~ variable)
        p3 = p3 + scale_x_discrete(
            name = changing[1], expand = c(0, 0),
            labels = clean(levels(factor(merged[, changing[1]])))
        )
        p3 = p3 + scale_y_discrete(
            name = changing[2], expand = c(0, 0),
            labels = clean(levels(merged[, changing[2]]))
        )
        p3 = p3 + scale_fill_gradient(
            name = 'MSD stddev\n', high = 'red', low = 'green'
        )
        p3 = p3 + ggtitle(
            paste(
                'Standard deviation of MSD of admixtures in relation to\n',
                toString(changing)
            )
        )

        p2 = ggplot(
            melted$mean$cor,
            aes_string(x = changing[1], y = changing[2])
        )
        p2 = p2 + theme(text = element_text(size = variables$textSize))
        p2 = p2 + geom_tile(aes_string(fill = 'value'))
        p2 = p2 + facet_wrap(~ variable)
        p2 = p2 + scale_x_discrete(
            name = changing[1], expand = c(0, 0),
            labels = clean(levels(factor(merged[, changing[1]])))
        )
        p2 = p2 + scale_y_discrete(
            name = changing[2], expand = c(0, 0),
            labels = clean(levels(merged[, changing[2]]))
        )
        p2 = p2 + scale_fill_gradientn(
            name = 'Partial Mantel\ncorrelation\n',
            colours = c('blue', 'red', 'green'), limits = c(-1, 1),
            guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE)
        )
        p2 = p2 + ggtitle(
            paste(
                'Partial Mantel correlation test in relation to\n',
                toString(changing)
            )
        )
        p4 = ggplot(
            melted$stddev$cor,
            aes_string(x = changing[1], y = changing[2])
        )
        p4 = p4 + theme(text = element_text(size = variables$textSize))
        p4 = p4 + geom_tile(aes_string(fill = 'value'))
        p4 = p4 + facet_wrap(~ variable)
        p4 = p4 + scale_x_discrete(
            name = changing[1], expand = c(0, 0),
            labels = clean(levels(factor(merged[, changing[1]])))
        )
        p4 = p4 + scale_y_discrete(
            name = changing[2], expand = c(0, 0),
            labels = clean(levels(merged[, changing[2]]))
        )
        p4 = p4 + scale_fill_gradient(
            name = 'MSD stddev\n', high = 'red', low = 'green'
        )
        p4 = p4 + ggtitle(
            paste(
                'Standard deviation of Partial Mantel correlation test in relation to\n',
                toString(changing)
            )
        )

        return(list(p1, p3, p2, p4, nrow = 4))
    }
}

colWeightedMeans = compiler::cmpfun(function(m, w) {
    W = matrix(rep(w, ncol(m)), nrow = length(w))
    W[is.na(m)] = 0L
    M = m * W
    MColSum = colSums(M, na.rm = TRUE)
    WColSum = colSums(W)
    return(MColSum / WColSum)
})

demesToIslands = compiler::cmpfun(function(df, summaryNames, islands) {
    output = data.frame(
        run = df$run[1],
        Island = islands
    )
    output[, summaryNames] = 0.0
    for (i in islands) {
        s = df[df$Island == i, ]
        output[output$Island == i, summaryNames] = colWeightedMeans(
            s[, summaryNames],
            s$DemeSize
        )
    }
    return(output)
})

successfulRuns = compiler::cmpfun(function(df, popRatio, deadDemes, debug) {
    uniqueIslands = unique(df$Island)
    failedRuns = c()
    for (r in unique(df$run)) {
        simu = df[df$run == r, ]
        max = max(simu$DemeSize)
        for (i in uniqueIslands) {
            empty = simu[simu$Island == i, 'DemeSize'] < popRatio
            # if too many empty (or nearly empty) demes on an island
            if (length(empty) * deadDemes <= sum(empty)) {
                failedRuns = c(failedRuns, r)
                break
            }
        }
    }

    # removes failed runs
    if (debug) {
        cat(paste('removing', length(failedRuns), 'failed simulations\n'))
    }

    return(df[!df$run %in% failedRuns, ])
})

hasFailedWithDemeInfo = compiler::cmpfun(
    function(df, popRatio, deadDemes, islands, summaryNames) {
        max = max(df$DemeSize)
        for (i in islands) {
            empty = df[df$Island == i, 'DemeSize'] < popRatio * max
            # if too many empty (or nearly empty) demes on an island
            if(length(empty) * deadDemes <= sum(empty)) {
                return(TRUE)
            }
        }
        return(FALSE)
    }
)

hasFailedWithoutDemeInfo = compiler::cmpfun(
    function(df, popRatio, deadDemes, islands, summaryNames) {
        for (i in islands) {
            islandDF = df[df$Island == i, ]
            # if too many NaN values in demes on an island
            if(sum(floor(colMeans(is.na(islandDF[, summaryNames])))) > 0) {
            #if(is.na(sum(apply(islandDF[, summaryNames], 2, sum)))) {
                return(TRUE)
            }
        }
        return(FALSE)
    }
)

addToCounts = compiler::cmpfun(function(current, simulation, params) {
    if (nrow(current$df) == 0L) {
        current$df = simulation
        current$df$count = 1L
    } else {
        found = FALSE
        for (i in 1L:nrow(current$df)) {
            if (all(current$df[i, params] == simulation)) {
                current$df$count[i] = current$df$count[i] + 1L
                found = TRUE
            }
        }
        if (!found) {
            tmp = cbind(simulation, 1L)
            colnames(tmp) = c(colnames(simulation), 'count')
            current$df = rbind(current$df, tmp)
            for (p in params) {
                current$changing[p] = length(unique(current$df[, p]))
            }
        }
    }
    return(current)
})

analysisType = compiler::cmpfun(function(changing) {
    if (changing == 0) {
        output = 'stability'
    } else if (changing == 1) {
        output = 'sensitivity'
    } else if (changing == 2) {
        output = 'sensitivity'
    } else {
        stop(paste('cannot perform analysis on', changing, 'changing parameters'))
    }
    return(output)
})

addToSensit = compiler::cmpfun(function(current, simulation) {
    melted = melt(simulation, id = 'Island')
    if (nrow(current) == 0L) {
        return(melted)
    } else {
        return(rbind(current, melted))
    }
})
