#!/usr/bin/Rscript
library(ade4)
library(lattice)
library(vegan)
library(SpatialTools)
library(gclus)

names = list('all_asian', 'all_melanesian', 'real', 'inverted', 'runif1', 'runif2')
admix = list()

nperms = 10
nrands = 200

for (i in 1:length(names)) {
    admix[[i]] = read.csv(paste0('../Data/extremes/', names[[i]], '.csv'))
}
coordinates = read.csv('../Data/extremes/coordinates.csv')

customDiff = function(a, b = a) {
    mata = matrix(
        rep(a, length(b)),
        nrow = length(b),
        ncol = length(a)
    )
    matb = matrix(
        rep(b, length(a)),
        nrow = length(a),
        ncol = length(b)
    )
    return(mata - t(matb))
}

funNames = list(
    'Standard error',
    'Mean of Squared Distances',
    'Sum of Squared Distances',
    'Standard error on admixture distance matrices',
    'Mean of Squared Distances on admixture distance matrices',
    'Weighted mean of Squared Distances on admixture distance matrices - 1',
    'Weighted mean of Squared Distances on admixture distance matrices - 2',
    'Mantel test on admixture distance matrices (cor Pearson)',
    'Partial Mantel test on admixture distance matrices (cor Pearson)',
    'Mantel test on admixture distance matrices (cor Spearman)',
    'Partial Mantel test on admixture distance matrices (cor Spearman)',
    'Mantel test on admixture distance matrices (cor Kendall)',
    'Partial Mantel test on admixture distance matrices (cor Kendall)',
    'Coefficient of vectorial correlation on admixture distance matrices',
    'Coinertia analysis on admixture distance matrices',
    'Procustes analysis on admixture distance matrices'
)
funCodes = list(
    'StdErrnotMat', 'MSDnotMat', 'SSDnotMat', 'StdErr', 'MSD', 'WMSD_1', 'WMSD_2',
    'MantP', 'PMantP', 'MantS', 'PMantS', 'MantK', 'PMantK', 'VectCorr', 'coinertia', 'Procustes'
)

funs = list(
    function(a, b, c) {
        return(sqrt(mean((a$DnaAdmixture - b$DnaAdmixture) ^ 2)))
    },
    function(a, b, c) {
        return(mean((a$DnaAdmixture - b$DnaAdmixture) ^ 2))
    },
    function(a, b, c) {
        return(sum((a$DnaAdmixture - b$DnaAdmixture) ^ 2))
    },
    function(a, b, c) {
        return(sqrt(mean((customDiff(a$DnaAdmixture) - customDiff(b$DnaAdmixture)) ^ 2)))
    },
    function(a, b, c) {
        return(mean((customDiff(a$DnaAdmixture) - customDiff(b$DnaAdmixture)) ^ 2))
    },
    function(a, b, c) {
        geoDist = dist1(matrix(cbind(coordinates$longitude, coordinates$latitude), ncol = 2))
        vectGeoDist = as.vector(as.matrix(geoDist))
        maxGeoDist = max(vectGeoDist)
        weights = -vectGeoDist + maxGeoDist + 1

        dista = customDiff(a$DnaAdmixture)
        distb = customDiff(b$DnaAdmixture)
        vectDist = as.vector(as.matrix((dista - distb) ^ 2))

        return(weighted.mean(vectDist, weights))
    },
    function(a, b, c) {
        geoDist = dist1(matrix(cbind(coordinates$longitude, coordinates$latitude), ncol = 2))
        diag(geoDist) = max(geoDist)
        vectGeoDist = as.vector(as.matrix(geoDist))
        maxGeoDist = max(vectGeoDist)
        weights = -vectGeoDist + maxGeoDist + 1

        dista = customDiff(a$DnaAdmixture)
        distb = customDiff(b$DnaAdmixture)
        vectDist = as.vector(as.matrix((dista - distb) ^ 2))

        return(weighted.mean(vectDist, weights))
    },
    function(a, b, c) {
        return(mantel(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture),
            method = 'pearson', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        geoDist = dist1(matrix(cbind(coordinates$longitude, coordinates$latitude), ncol = 2))
        return(mantel.partial(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture), geoDist,
            method = 'pearson', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        return(mantel(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture),
            method = 'spearman', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        geoDist = dist1(matrix(cbind(coordinates$longitude, coordinates$latitude), ncol = 2))
        return(mantel.partial(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture), geoDist,
            method = 'spearman', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        return(mantel(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture),
            method = 'kendall', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        geoDist = dist1(matrix(cbind(coordinates$longitude, coordinates$latitude), ncol = 2))
        return(mantel.partial(
            customDiff(a$DnaAdmixture), customDiff(b$DnaAdmixture), geoDist,
            method = 'kendall', permutations = nperms
        )$statistic)
    },
    function(a, b, c) {
        dista = as.dist(customDiff(a$DnaAdmixture))
        distb = as.dist(customDiff(b$DnaAdmixture))
        if (isTRUE(is.euclid(dista)) & isTRUE(is.euclid(distb))) {
            return(RVdist.randtest(dista, distb, nrepet = nperms)$obs)
        } else {
            return(NA)
        }
    },
    function(a, b, c) {
        dista = as.dist(customDiff(a$DnaAdmixture))
        distb = as.dist(customDiff(b$DnaAdmixture))
        if (isTRUE(is.euclid(dista)) & isTRUE(is.euclid(distb))) {
            pco1 = dudi.pco(dista, nf = 2, scannf = FALSE)
            pco2 = dudi.pco(distb, nf = 2, scannf = FALSE)
            return(coinertia(dudiX = pco1, dudiY = pco2, nf = 2, scannf = FALSE)$RV)
        } else {
            return(NA)
        }
    },
    function(a, b, c) {
        dista = as.dist(customDiff(a$DnaAdmixture))
        distb = as.dist(customDiff(b$DnaAdmixture))
        if (isTRUE(is.euclid(dista)) & isTRUE(is.euclid(distb))) {
            pco1 = dudi.pco(dista, nf = 2, scannf = FALSE)
            pco2 = dudi.pco(distb, nf = 2, scannf = FALSE)
            return(procuste.randtest(pco1$tab, pco2$tab)$obs)
        } else {
            return(NA)
        }
    }
)

pb = txtProgressBar(0, length(funs), style = 3)
summaryResults = list()
for (i in 1:length(funs)) {
    df = matrix(ncol = length(names), nrow = length(names), dimnames = list(names, names))
    for (j in 1:length(admix)) {
        for (k in 1:length(admix)) {
            df[j, k] = funs[[i]](admix[[j]], admix[[k]], coordinates)
        }
    }
    setTxtProgressBar(pb, i)

    summaryResults[[i]] = list(name = funNames[[i]], results = df)
}

print(summaryResults)

rands = list(nrands)
for (i in 1:nrands) {
    rands[[i]] = list(DnaAdmixture = runif(length(admix[[1]]$DnaAdmixture)))
}
#nbattles = (nrands * (nrands - 1)) / 2
nbattles = nrands ^ 2 - ((nrands * (nrands - 1)) / 2)

battles = matrix(nrow = nbattles, ncol = length(funs))
colnames(battles) = funCodes
k = 1
pb = txtProgressBar(k, nbattles, style = 3)
for (i in 1:nrands) {
    for (j in i:nrands) {
        for (f in 1:length(funs)) {
            battles[k, f] = funs[[f]](rands[[i]], rands[[j]], coordinates)
        }

        setTxtProgressBar(pb, k)
        k = k + 1
    }
}


battles.cor = cor(battles)
customColors = function(x) {
    return(hsv(ifelse(x > 0, 0.4, 1.0), abs(x), abs(x)))
}
battles.col = battles.cor
for (i in 1:nrow(battles.cor)) {
    for (j in 1:ncol(battles.cor)) {
        battles.col[i, j] = customColors(battles.cor[i, j])
    }
}

cpairs(
    battles, panel.colors = battles.col,
    pch = '.', gap = 0.5,
    #ylim = 0:1, xlim = 0:1,
    #upper.panel = panel.smooth,
    lm = TRUE
)
