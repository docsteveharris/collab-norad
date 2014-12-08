# author: Steve Harris
# date: 2014-12-08
# subject: Simple modelling 

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2014-12-08
# - file created

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


is.real <- function(x) {
    if (is.na(x) == FALSE &
        is.infinite(x) == FALSE &
        is.numeric(x) == TRUE) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}
is.real(1)

rm(list=ls(all=TRUE))
source(file="load.R")
load(file='../data/cleaned.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==596)

# Simple model - response variable: noradrenaline at t_24
describe(wdt$ne.24)
qplot(ne.24, data=wdt)

head(wdt$ne.24)
setorder(wdt, ne.24)
wdt[, id.sort := seq(1, nrow(wdt))]
tail(wdt[, id.sort, ne.24])
str(wdt)

# Quantile plot
qplot(x=id.sort, y=ne.24, data=wdt)

# Qnorm plot
qqnorm(wdt[!is.na(ne.24) ]$ne.24)
wdt[, x:= NULL]
wdt[, ne.24.qnorm := rnorm( nrow(wdt), mean(ne.24, na.rm=TRUE), sd(ne.24, na.rm=TRUE))]
qqplot("ne.24.qnorm" , "ne.24", data=wdt )
qqnorm(wdt[!is.na(ne.24) & ne.24 != 0]$ne.24.log)

# Qnorm against log transformed
wdt[, ne.24.log := NULL]
wdt[, ne.24.log := log(ne.24)]
qplot(sample=ne.24.log, data=wdt, stat="qq", asp=1)
qqnorm(wdt$ne.24)

wdt[, ne.24.qnorm.log := rnorm( nrow(wdt), mean(ne.24.log, na.rm=TRUE), sd(ne.24.log, na.rm=TRUE))]
wdt[, ne.24.log, ne.24.qnorm.log]

wdt[!is.na(ne.24.log) & !is.infinite(ne.24.log)]
qqplot("ne.24.qnorm.log" , "ne.24.log", data=wdt[!is.na(ne.24.log) & !is.infinite(ne.24.log) & !is.nan(ne.24.log)] )

gg.1 <- ggplot(aes(y=ne.24), data=wdt)
# All patients
gg.1 + geom_jitter(aes(x=all)) + geom_hline(y=mean(ne.24, na.rm=TRUE))

gg.1 + geom_jitter(aes(x=hosp))