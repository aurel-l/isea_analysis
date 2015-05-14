# module to be sourced

# libraries
suppressMessages(library(argparse))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(reshape2))
suppressMessages(library(XML))

#variables
variables = list(
    debug = FALSE,
    observedIslands = c(
        'Chin', 'Viet', 'Mala', 'Taiw', 'Phil', 'Suma', 'Java', 'Born',
        'Bali', 'Sula', 'Sumb', 'Halm', 'Flor', 'Timo', 'Alor', 'Papu'
    ),
    summaryNames = c(
        'DnaAdmixture',
        'AutosomeAdmixture', 'XChrAdmixture',
        'MitoAdmixture', 'YChrAdmixture'
    ),
    continuousParams = c(
        'migrationProb', 'poissonMean', 'marriageThres',
        'growthRate', 'initialDemeAgentNumber'
    ),
    discreteParams = c(
        'startingDistributionFile', 'graphFile'
    ),
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S'),
    permutations = 99,
    textSize = 30
)
variables$paramNames = c(variables$continuous, variables$discreteNames)

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
        names = outNames[order(outCounts)],
        changing = length(outNames),
        sets = prod(outTable$values)
    ))
}

toXMLFile = function(df) {
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
    invisible(saveXML(root, paste0(variables$now, '-parameters.xml')))
}

counts = function(df, changing) {
    countFrame = aggregate(
        randomSeed ~ .,
        df,
        FUN = function(x) length(unique(x))
    )

    if (length(changing) == 1) {
        p = ggplot(
            countFrame,
            aes_string(x = countFrame[, changing], y = countFrame$randomSeed)
        )
        p = p + geom_bar(fill = 'white', colour = 'black', width = 0.5)
        p = p + scale_y_continuous(name = 'simulation count', expand = c(0, 0))
    } else {# 2 changing parameters
        p = ggplot(
            countFrame,
            aes_string(
                x = countFrame[, changing[1]],
                y = countFrame[, changing[2]]
            )
        )
        p = p + geom_tile(aes_string(fill = countFrame[, 'randomSeed']))
        if (changing[2] %in% variables$discreteParams) {
            p = p + scale_y_discrete(name = changing[2], expand = c(0, 0))
        } else {
            p = p + scale_y_continuous(name = changing[2], expand = c(0, 0))
        }
        #p = p + scale_fill_gradient(name = 'simulation count')
    }
    p = p + scale_x_continuous(name = changing[1], expand = c(0, 0))
    p = p + ggtitle('Count of simulations for every changing parameter')

    return(p)
}
