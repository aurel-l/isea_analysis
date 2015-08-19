# module to be sourced

# libraries
# used for parsing command line arguments
suppressMessages(library(argparse))
# mainly used for the 'melt' function
suppressMessages(library(reshape2))
# used for mantel.partial
# !!!high memory use!!!
##suppressMessages(library(vegan))# moved to the beginning of 'analysis.R'
# used to generate the parameter sweep xml
##suppressMessages(library(XML))# moved to the end of 'analysis.R'

#variables, default values, might be overwritten later in the code
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
        'growthRate', 'initialDemeAgentNumber',
        'poissonMeanRatio', 'migrationProbRatio'
    )
variables$discreteParams = c(
        'startingDistributionFile', 'graphFile'
    )
variables$orderedAdmixtures = c(
    'MitoAdmixture', 'XChrAdmixture', 'AutosomeAdmixture', 'YChrAdmixture'
)
variables$ABC_MSD = c('Autosome_MSD', 'XChr_MSD')
variables$ABC_cor = c('Autosome_cor', 'XChr_cor')

variables$nDemes = 118L
variables$nIslands = 21L

# tolerance values for failed runs
variables$toleratedPopRatio = 0.1
variables$toleratedDeadDemes = 0.25

# graph themes and sizes
variables$textSize = 12L
variables$pointSize = 3L
# sizes of A4 page
variables$long = 297L
variables$short = 210L

# current time, for file naming
variables$now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

# not to be changed! change continuousParams or discreteParams if need be
variables$paramNames = c(variables$continuousParams, variables$discreteParams)
# not to be changed! change ABC_MSD or ABC_cor if need be
variables$ABCsummary = c(variables$ABC_MSD, variables$ABC_cor)

# add column types here
# used for optimisations, required
variables$types = new.env()
variables$types$DnaAdmixture = 'numeric'
variables$types$AutosomeAdmixture = 'numeric'
variables$types$XChrAdmixture = 'numeric'
variables$types$MitoAdmixture = 'numeric'
variables$types$YChrAdmixture = 'numeric'
variables$types$DemeSize = 'integer'
variables$types$migrationProb = 'numeric'
variables$types$poissonMean = 'numeric'
variables$types$migrationProbRatio = 'numeric'
variables$types$poissonMeanRatio = 'numeric'
variables$types$marriageThres = 'numeric'
variables$types$growthRate = 'numeric'
variables$types$asianDefinition = 'numeric'
variables$types$initialDemeAgentNumber = 'integer'
variables$types$startingDistributionFile = 'factor'
variables$types$graphFile = 'factor'
variables$types$tick = 'numeric'
variables$types$Label = 'character'
variables$types$Island = 'factor'
variables$types$run = 'integer'
variables$types$randomSeed = 'integer'
# end of column types

# comparison cases (hardcoded)
cases = c('worst_case', 'all_asian', 'all_melanesian')
variables$cases = data.frame(
    reference_case = rep(cases, each = 4),
    value = numeric(length(cases) * 4),
    comparison = rep(rep(c('MSD', 'cor'), each = 2), length(cases)),
    admixture = rep(rep(c('Autosome', 'XChr'), 2), length(cases))
)
variables$cases$reference_case = factor(
    variables$cases$reference_case,
    levels = cases
)
variables$cases$admixture = factor(variables$cases$admixture)
variables$cases$value[variables$cases$reference_case == 'worst_case'] = c(
    0.7716625, 0.8238437, 0.6880700, 0.7416792
)
variables$cases$value[variables$cases$reference_case == 'all_asian'] = c(
    0.09666250, 0.07509375, NA, NA
)
variables$cases$value[variables$cases$reference_case == 'all_melanesian'] = c(
    0.7241625, 0.7813438, NA, NA
)
variables$cases = variables$cases[!is.na(variables$cases$value), ]


# cleans the vector of names of useless information
clean = compiler::cmpfun(function(names) {
    # removes extensions, and common patterns
    out = gsub(
        '(.*/)|(starting_distribution_)|(death_rates_)|(\\.csv)', '',
        names
    )
    # changes underscores by spaces
    out = gsub(
        '_', ' ',
        out
    )
    # proposition: change variables names to 'nicer' ones?
    #out = sub('', '', out)
    return(out)
})
