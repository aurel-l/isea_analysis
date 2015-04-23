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
# arguments = commandArgs(trailingOnly = TRUE)
# if (length(arguments) >= 1) {
#     variables.realFile = arguments[1]
#     if (length(arguments) >= 2) {
#         variables.dataFile = arguments[2]
#     } else {
#         variables.dataFile = file('stdin')
#     }
# }
variables.realFile = '../Data/isea_admixture_data_for_comparison_2.csv'
variables.dataFiles = c(
    'all_melanesian', 'wave1', 'wave2', 'wave3', 'wave4', 'wave5', 'all_asian',
    'final'
)
variables.span = 0.2
variables.rep = 250
#variables.rep = 2

# load data
imports.real = read.csv(variables.realFile)
imports.dataList = list()
for (i in 1:length(variables.dataFiles)) {
    imports.dataList[[i]] = read.csv(paste0('../Data/extremes/', variables.dataFiles[i], '.csv'))
}

# check random seeds are unique
# randomSeeds = imports.data['randomSeed']
# if (dim(randomSeeds)[1] / dim(unique(imports.data['Island']))[1] != dim(unique(randomSeeds))[1]) {
#     c = count(randomSeeds)
#     dup = c[c$freq != min(c$freq), 1]
#     print(dup)
#     stop('found a repeated random seed')
# }

imports.real = imports.real[!is.na(imports.real$DnaAdmixture),]

df = data.frame(matrix(
    0,
    (length(variables.dataFiles) + length(unique(imports.real$Island)) + 2) *
    (length(variables.paramNames) + length(variables.admixNames)) * variables.rep,
    ncol = length(variables.paramNames) + length(variables.admixNames) + 2
))
colnames(df) = c(variables.paramNames, 'randomSeed', 'Island', variables.admixNames)
i = 1
islands = unique(imports.real$Island)
for (r in 1:variables.rep) {
    for (j in 1:length(imports.dataList)) {
        rs = runif(1)
        for (island in islands) {
            df[i, 'Island'] = island
            df[i, 'randomSeed'] = rs
            df[i, variables.paramNames] = runif(
                length(variables.admixNames),
                -(variables.span / 2),
                variables.span / 2
            ) + j
            tmp = runif(
                length(variables.admixNames),
                -(variables.span / 2),
                variables.span / 2
            ) + imports.dataList[[j]][imports.dataList[[1]]$Island == island, variables.admixNames]
            tmp[tmp > 1] = 1
            tmp[tmp < 0] = 0
            df[i, variables.admixNames] = tmp
            i = i + 1
        }
    }
}
merged = merge(
    df,
    imports.real[c('Island', 'order')],
    by = 'Island'
)
merged = merged[order(merged$poissonMean, merged$order),]
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
            ignore = ignore | variables.distances[i1, i2] > (max(variables.distances) / 8)
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
ABCData.ssn = colnames(ABCData.obs)
ABCData.obs = as.numeric(ABCData.obs[1,])
ABCData.sim = fillDF(merged)
ABCData.par = merged[!duplicated(merged$randomSeed), variables.paramNames]

print(ABCData.obs)
summary(ABCData.sim)
summary(ABCData.par)
dim(ABCData.sim)
ABCData.sim[0,]

ret = selectsumm(
    ABCData.obs, ABCData.par, ABCData.sim,
    ssmethod = AS.select,# limit = round(dim(ABCData.obs)[2] / 5),
    tol = 0.2, method = 'rejection',
    allow.none = F, inturn = T, hcorr = T
)
colnames(ret$best) = ABCData.ssn
print(ret)

