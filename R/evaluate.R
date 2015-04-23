#!/usr/bin/Rscript
library(ade4)
library(lattice)
library(vegan)
library(SpatialTools)
library(ade4)

#variables
variables.summaryNames = c(
    'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
    'MitoAdmixture', 'YChrAdmixture'
)
variables.paramNames = c(
    'migrationProb', 'poissonMean', 'marriageThres', 'growthRate',
    'initialDemeAgentNumber', 'startingDistributionFile', 'graphFile'
)
variables.admixFile = '../../isea/isea/output/admixturebynode.2015.Mar.26.13_01_14.txt'
variables.orderFile = '../Data/isea_admixture_data_for_comparison_2.csv'
variables.now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

#CLI arguments
arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) >= 1) {
    variables.orderFile = arguments[1]
    if (length(arguments) >= 2) {
        variables.admixFile = arguments[2]
    } else {
        variables.admixFile = file('stdin')
    }
}


#load data
imports.admix = read.csv(variables.admixFile)
imports.order = read.csv(variables.orderFile)[c('Island', 'order', 'longitude', 'latitude')]

#checks
randomSeeds = imports.admix['randomSeed']
if (dim(randomSeeds)[1] / dim(unique(imports.admix['Island']))[1] != dim(unique(randomSeeds))[1]) {
    stop('found a repeated random seed')
}

randomSeeds = unlist(unique(randomSeeds))
paramComb = aggregate(
    .~randomSeed+startingDistributionFile+graphFile, imports.admix, FUN = mean
)

mantelV = merge(
    imports.admix,
    imports.order,
    by = 'Island'
)
mantelP = dist1(matrix(cbind(imports.order$longitude, imports.order$latitude), ncol = 2))

n = length(randomSeeds)
n = n * (n-1) / 2
df = data.frame(
    Same = logical(n),
    Mantel = double(n), Mantel2 = double(n),
    SquaredDistDna = double(n), SquaredDistAutosome = double(n), SquaredDistXChr = double(n),
    SquaredDistMito = double(n), SquaredDistYChr = double(n)
)
k = 1
pb = txtProgressBar(k, n, style = 3)
for (i in 1:(length(randomSeeds)-1)) {
    for (j in (i+1):length(randomSeeds)) {
        df$Same[k] = all(
            paramComb[paramComb$randomSeed == randomSeeds[i], variables.paramNames] ==
            paramComb[paramComb$randomSeed == randomSeeds[j], variables.paramNames]
        )

        #partial mantel
        set1 = mantelV[paramComb$randomSeed == randomSeeds[i], ]
        set1 = set1[order(set1$order),]
        set2 = mantelV[paramComb$randomSeed == randomSeeds[j], ]
        set2 = set2[order(set2$order),]
        mantelStat = mantel.partial(
            dist(set1$DnaAdmixture),
            dist(set2$DnaAdmixture),
            mantelP,
            permutations = 50
        )
        df$Mantel[k] = mantelStat$statistic

        #mantel
        #set1 = mantelV[paramComb$randomSeed == randomSeeds[i], ]
        #set1 = set1[order(set1$order),]
        #set2 = mantelV[paramComb$randomSeed == randomSeeds[j], ]
        #set2 = set2[order(set2$order),]
        mantelStat = mantel(
            dist(set1$DnaAdmixture),
            dist(set2$DnaAdmixture),
            permutations = 50
        )
        df$Mantel2[k] = mantelStat$statistic

        #squaredDist
        sqd = (
            imports.admix[paramComb$randomSeed == randomSeeds[i], variables.summaryNames] -
            imports.admix[paramComb$randomSeed == randomSeeds[j], variables.summaryNames]
        ) ^ 2
        sqd = colSums(sqd)

        df$SquaredDistDna[k] = sqd['DnaAdmixture']
        df$SquaredDistAutosome[k] = sqd['AutosomeAdmixture']
        df$SquaredDistXChr[k] = sqd['XChrAdmixture']
        df$SquaredDistMito[k] = sqd['MitoAdmixture']
        df$SquaredDistYChr[k] = sqd['YChrAdmixture']

        setTxtProgressBar(pb, k)
        k = k+1
    }
}
print(summary(df[df$Same == TRUE,]))
print(summary(df[df$Same == FALSE,]))
png(paste0('evaluation-squaredDist-', variables.now, '.png'))
plot(
    df$Same - 0.1,
    df$SquaredDistDna,
    #SquaredDist ~ Same,
    #data = df,
    xlim = c(-0.2, 1.2),
    col = 'blue', pch = '.'
)
points(
    df$Same - 0.05,
    df$SquaredDistAutosome,
    #SquaredDist2 ~ Same,
    #data = df,
    col = 'green', pch = '.'
)
points(
    df$Same,
    df$SquaredDistXChr,
    #SquaredDist3 ~ Same,
    #data = df,
    col = 'grey', pch = '.'
)
points(
    df$Same + 0.05,
    df$SquaredDistMito,
    #SquaredDist3 ~ Same,
    #data = df,
    col = 'grey', pch = '.'
)
points(
    df$Same + 0.1,
    df$SquaredDistYChr,
    #SquaredDist3 ~ Same,
    #data = df,
    col = 'grey', pch = '.'
)
graphics.off()
png(paste0('evaluation-mantel-', variables.now, '.png'))
plot(
    df$Same - 0.05,
    df$Mantel,
    #Mantel ~ Same,
    #data = df,
    xlim = c(-0.2, 1.2),
    col = 'red', pch = 1
)
points(
    df$Same,
    df$Mantel2,
    #Mantel2 ~ Same,
    #data = df,
    col = 'yellow', pch = 2
)
graphics.off()

