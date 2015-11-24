# Quick inspection of ITU mortality and LOS 
# --------------------------------------


rm(list=ls(all=TRUE))
load(file='../data/working.RData')
wdt.original <- wdt
str(wdt)

describe(wdt[, .(mort.itu, los.itu)])
summary(wdt$los.itu)
wdt$los.itu
wdt[, .(
    mort.mean = round(mean(mort.itu)*100,1),
    mort.n    = sum(mort.itu),
    los.q25   = quantile(los.itu, 0.25, na.rm=TRUE),
    los.q50   = quantile(los.itu, 0.50, na.rm=TRUE),
    los.q75   = quantile(los.itu, 0.75, na.rm=TRUE))
    ,
    by=hosp]

# |    | hosp | mort.mean | mort.n | los.q25 | los.q50 | los.q75 |
# |----|------|-----------|--------|---------|---------|---------|
# | 1: | rome |      86.0 |     86 |    8.00 |      14 |   24.25 |
# | 2: | an   |      57.0 |     57 |    3.75 |      11 |   21.25 |
# | 3: | blf  |      36.0 |     36 |    2.75 |       5 |   12.00 |
# | 4: | lee  |      13.0 |     13 |    4.75 |       8 |   19.00 |
# | 5: | rlh  |      37.1 |     33 |    2.00 |       6 |   12.00 |
# | 6: | uclh |      39.3 |     42 |    5.00 |       9 |   16.50 |


# Function to compare two distributions using qplot
qqplot <- function(x,y, data=wdt, n=100) {
    lab.x = x
    lab.y = y
    x <- with(data, get(x))
    x <- sapply(seq(0,1,1/n), function(q) quantile(x, q, na.rm=TRUE))
    y <- with(data, get(y))
    y <- sapply(seq(0,1,1/n), function(q) quantile(y, q, na.rm=TRUE))
    # Get axes symmetrical
    axes.minmax <- c(floor(min(c(x,y))), ceiling(max(c(x,y))))
    # print(axes.minmax)
    qplot(x,y, asp=1, xlab=lab.x, ylab=lab.y) +
        geom_abline(intercept=0,slope=1) +
        coord_cartesian(x=axes.minmax, y=axes.minmax)
}

qqplot('map.1', 'map.24')
qqplot('ne.1', 'ne.24')


# Now facet plot by hospital
axes.minmax <- c(floor(min(c(wdt$ne.1,wdt$ne.24), na.rm=TRUE)), ceiling(max(c(wdt$ne.1,wdt$ne.24), na.rm=TRUE)))
axes.minmax2 <- c(floor(min(c(wdt$ne.1,wdt$ne.24), na.rm=TRUE)), 2)
tdt <- data.table(wdt[, .(
    x = sapply(seq(0,1,1/(100-1)), function(q) quantile(.SD$ne.1, q, na.rm=TRUE)),
    y = sapply(seq(0,1,1/(100-1)), function(q) quantile(.SD$ne.24, q, na.rm=TRUE)),
    f = as.numeric(hosp)
    ), by=hosp])
str(tdt)
qplot(x,y,data=tdt, facets=  ~ f, asp=1,
    xlab="ne.1",
    ylab="ne.24") +
    geom_abline(intercept=0,slope=1) +
    coord_cartesian(x=axes.minmax2, y=axes.minmax2)

