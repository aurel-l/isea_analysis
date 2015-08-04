#!/usr/bin/Rscript
suppressMessages(library(ggplot2))
now = strftime(Sys.time(), '%Y_%m_%d_%H_%M_%S')

debug = FALSE
#debug = TRUE

if (debug) {
    input = 'parsed.csv'
} else {
    input = 'stdin'
}

df = read.csv(
    input, colClasses = rep('numeric', 5),
    row.names = 1
)
df$failed = as.logical(df$failed)
df = df[!df$failed, ]

p1 = ggplot(df, aes(nSimulations, time))
p1 = p1 + geom_point()
#p1 = p1 + geom_smooth(method = lm)
p1 = p1 + scale_x_continuous('number of analysed simulations')
p1 = p1 + scale_y_continuous('analyse time (seconds)', limits = c(0, max(df$time)))
p1 = p1 + ggtitle('time used by number of analysed simulations')
p1 = p1 + theme(text = element_text(size = 20))

if (!debug) {
    png(
        paste0(now, '-timeByNSimulations.png'),
        width = 877L, height = 620L
    )
}
print(p1)
if (!debug) {
    graphics.off()
}

p2 = ggplot(df, aes(nSimulations, maxMem))
p2 = p2 + geom_point()
#p2 = p2 + geom_smooth(method = lm)
p2 = p2 + scale_x_continuous('number of analysed simulations')
p2 = p2 + scale_y_continuous('maximum memory use (MiB)', limits = c(0, max(df$maxMem)))
p2 = p2 + ggtitle('maximum memory used by number of analysed simulations')
p2 = p2 + theme(text = element_text(size = 20))

if (!debug) {
    png(
        paste0(now, '-maxMemByNSimulations.png'),
        width = 877L, height = 620L
    )
}
print(p2)
if (!debug) {
    graphics.off()
}
