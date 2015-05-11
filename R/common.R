# module to be sourced

# libraries
suppressMessages(library(argparse))
suppressMessages(library(ggplot2))
suppressMessages(library(XML))

#variables
variables = list(
    debug = FALSE,
    summaryNames = c(
        'DnaAdmixture',
        'AutosomeAdmixture', 'XChrAdmixture',
        'MitoAdmixture', 'YChrAdmixture'
    ),
    paramNames = c(
        'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
        'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile'
    ),
    discreteParams = c('startingDistributionFile', 'graphFile'),
    now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S'),
    permutations = 99
)

toXMLFile = function(df) {
    root = newXMLNode('sweep')
    sets = 1
    for (p in variables$paramNames) {
        values = unique(df[, p])
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
