#!/usr/bin/Rscript
suppressMessages(library(ggplot2))
now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

debug = FALSE
#debug = TRUE

cat('Performance benchmark\n')

if (debug) {
    input = 'parsed.csv'
    warning('running in debug mode')
} else {
    input = 'stdin'
}

df = read.csv(
    input, colClasses = rep('numeric', 6),
    row.names = 1
)
df$failed = as.logical(df$failed)
df = df[!df$failed, ]

cat(nrow(df), 'analyses used\n')

# Time benchmark

p1 = ggplot(df, aes(nSimulations, time))
p1 = p1 + geom_point()
p1 = p1 + geom_smooth(method = 'lm', formula = y ~ poly(x, 2))
p1 = p1 + scale_x_continuous('number of analysed simulations')
p1 = p1 + scale_y_continuous('analysis time (seconds)', limits = c(0, max(df$time)))
p1 = p1 + ggtitle('time used by number of analysed simulations')
p1 = p1 + theme(text = element_text(size = 20))

if (!debug) {
    filename = paste0(now, '-timeByNSimulations.png')
    png(filename, width = 877L, height = 620L)
}
print(p1)
if (!debug) {
    graphics.off()
    cat(paste0('time scatter plot in ', filename, '\n'))
}

## Text report
time.lm = lm(df$time ~ poly(df$nSimulations, 2, raw = T))
cat('Time use report:\n')
cat('\tbase time use: ')
cat(round(time.lm$coefficients[1], digits = 2), 's\n', sep = '')

cat('\tcoefficients of the polynomial regression (')
cat(time.lm$rank - 1, 'degrees):\n')
for (i in 1:length(time.lm$coefficients)) {
    cat('\t\t', letters[i], ': ', time.lm$coefficients[i], '\n', sep = '')
}

# Memory benchmark

p2 = ggplot(df, aes(nSimulations, maxMem))
p2 = p2 + geom_point()
p2 = p2 + geom_smooth(method = lm)
p2 = p2 + scale_x_continuous('number of analysed simulations')
p2 = p2 + scale_y_continuous('maximum memory use (MiB)', limits = c(0, max(df$maxMem)))
p2 = p2 + ggtitle('maximum memory used by number of analysed simulations')
p2 = p2 + theme(text = element_text(size = 20))

if (!debug) {
    filename = paste0(now, '-maxMemByNSimulations.png')
    png(filename, width = 877L, height = 620L)
}
print(p2)
if (!debug) {
    graphics.off()
    cat(paste0('memory scatter plot in ', filename, '\n'))
}

## Text report
memory.lm1 = lm(maxMem ~ inputFileSize, data = df)
cat('Memory use report:\n')
cat(
    '\tmemory used for every 1MiB of serialised input data:',
    paste0(round(memory.lm1$coefficients[2], digits = 2), 'MiB\n')
)
memory.lm2 = lm(maxMem ~ nSimulations, data = df)
cat(
    '\tbase memory use:',
    paste0(round(memory.lm2$coefficients[1], digits = 2), 'MiB\n')
)
cat(
    '\tmemory used for every simulation:',
    paste0(round(memory.lm2$coefficients[2] * 1024, digits = 2), 'KiB\n')
)

