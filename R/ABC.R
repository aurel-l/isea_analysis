#!/usr/bin/Rscript
library(SpatialTools)
library(abctools)

# variables
variables.paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber'#, 'startingDistributionFile', 'graphFile'
)
variables.excludedSS = c(
    'Chin', 'Viet', 'Laos', 'Thai', 'Mala'
)
variables.admixNames = c('DnaAdmixture')
variables.now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

# CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.realFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.dataFile = arguments[2]
    } else {
        variables.dataFile = file('stdin')
    }
}

# load data
imports.real = read.csv(variables.realFile)
imports.data = read.csv(variables.dataFile)

# check random seeds are unique
randomSeeds = imports.data['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports.data['Island']))[1] != dim(unique(randomSeeds))[1]) {
    c = count(randomSeeds)
    dup = c[c$freq != min(c$freq), 1]
    print(dup)
    stop('found a repeated random seed')
}

imports.real = imports.real[!is.na(imports.real$DnaAdmixture),]
merged = merge(
    imports.data,
    imports.real[c('Island', 'order')],
    by = 'Island'
)
merged = merged[order(merged$randomSeed, merged$order),]
variables.islands = unique(imports.real$Island)
variables.distances = dist1(matrix(cbind(imports.real$longitude, imports.real$latitude), ncol = 2))
dimnames(variables.distances) = list(variables.islands, variables.islands)

fillDF = function(input) {
    df = data.frame()
    n = length(input$Island) / length(variables.islands)

    for (i1 in variables.islands) {
        if (!any(i1 == variables.excludedSS)) {
            df[1:n, i1] = input[input$Island == i1, 'DnaAdmixture']
        }

        for (i2 in variables.islands) {
            ignore = any(i1 == variables.excludedSS)
            ignore = ignore | any(i2 == variables.excludedSS)
            ignore = ignore | grep(i2, variables.islands) <= grep(i1, variables.islands)
            ignore = ignore | variables.distances[i1, i2] > (max(variables.distances) / 6)
            if (!ignore) {
                diff = input[input$Island == i1, 'DnaAdmixture'] - input[input$Island == i2, 'DnaAdmixture']
                df[1:n, paste0(i1, 'Minus', i2)] = diff
            }
        }
    }

    return(df)
}
#
ABCData.obs = fillDF(imports.real)
ABCData.sim = fillDF(merged)
ABCData.par = unique(merged[c('randomSeed', variables.paramNames)])

print(ABCData.obs)
summary(ABCData.sim)
summary(ABCData.par)
dim(ABCData.sim)
ABCData.sim[0,]

ret = selectsumm(
    ABCData.obs, ABCData.par, ABCData.sim,
    ssmethod = AS.select,# limit = round(dim(ABCData.obs)[2] / 3),
    tol = 0.01, method = 'rejection',
    allow.none = F, inturn = T, hcorr = T
)
colnames(ret$best) = colnames(ABCData.obs)
print(ret)
