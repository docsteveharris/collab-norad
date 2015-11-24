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
# source(file="load.R")
load(file='../data/cleaned.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==736)

# My old approach to data inspection
qplot(ne.24, data=wdt)
ggsave("../slides/src/ne24_hist.png")

# Simple model - response variable: noradrenaline at t_24
describe(wdt$ne.24)
fn <- ecdf(wdt$ne.24)
wdt[, ne.24.f := fn(ne.24)]
wdt[,.(ne.24, ne.24.f)]
qplot(y=ne.24, x=ne.24.f, data=wdt, xlim=c(0,1), ylim=c(-3,3), asp=1) + geom_hline(yintercept=quantile(wdt$ne.24, 0.5, na.rm=TRUE))
ggsave("../slides/src/ne24_qplot.png")


# Log transform
wdt[, ne.24.log := log(ne.24)]
fn <- ecdf(wdt$ne.24.log)
wdt[, ne.24.log.f := fn(ne.24.log)]
wdt[,.(ne.24.log, ne.24.log.f)]
qplot(y=ne.24.log, x=ne.24.log.f, data=wdt, xlim=c(0,1)) + geom_hline(yintercept=quantile(wdt$ne.24.log, 0.5, na.rm=TRUE))
ggsave("../slides/src/ne24log_qplot.png")

# Confirm this is normally distributed with qnorm plot
qqnorm(wdt[!is.na(ne.24) ]$ne.24)
dev.copy2pdf(file="../slides/src/ne24_qnorm.pdf") 
 
qqnorm(wdt[!is.na(ne.24) & is.finite(ne.24.log) ]$ne.24.log)
dev.copy2pdf(file="../slides/src/ne24log_qnorm.pdf") 

# Better transform = scale by 1000, then add 1 then log to handle 0's
wdt[, ne.24.log1 := log(1000*ne.24 + 1)]
qqnorm(wdt[!is.na(ne.24)]$ne.24.log1)
dev.copy2pdf(file="../slides/src/ne24logt_qnorm.pdf") 

# NOTE: 2014-12-08 - [ ] clear discontinuity: will not be able to model with a straightforward log transform
# For now let's work with patient's on noradrenaline at 24h
# else ordinal regression: http://stats.stackexchange.com/a/105329/7746
describe(wdt$ne.24)
wdt[, ne.24.log0 := ifelse(ne.24 > 0, log(1000*ne.24), NA)]
describe(wdt$ne.24.log0)
# 465 patients
qqnorm(wdt[!is.na(ne.24)]$ne.24.log0)


# Now let's facet by site
wdt[, ne.24.hosp := quantile(ne.24, 0.5, na.rm=TRUE), by=hosp]
wdt[, ne.24.hosp := factor(ne.24.hosp, labels=c(LETTERS[1:8]))]
qplot(sample=ne.24.log0, data=wdt, stat="qq")

# Without facets
gg <- ggplot(data=wdt, aes(sample=ne.24.log0))
gg +
    stat_qq() +
    scale_y_continuous(labels=function(y) sprintf("%.2f", exp(y)/1000))
ggsave("../slides/src/ne24log_qnorm_scale.png")

gg +
    stat_qq() +
    facet_wrap(~ ne.24.hosp, nrow=1) +
    scale_y_continuous(labels=function(y) sprintf("%.2f", exp(y)/1000))
ggsave("../slides/src/ne24log_qnorm_scale_hosp.png")

# Now model
m.null <- lm(ne.24.log0 ~ 1, data=wdt)
summary(m.null)

m <- lm(ne.24.log0 ~ ne.24.hosp + 0, data=wdt)
summary(m)
str(m)
# Now build a residual-fit spread plot
describe(m$fitted.values)
fn <- ecdf(m$fitted.values)
tt <- data.table(yhat=m$fitted.values, yhat.c=m$fitted.values - mean(m$fitted.values, na.rm=TRUE), yhat.f=fn(m$fitted.values)  )
fn <- ecdf(m$residuals)
tt$e <- m$residuals
tt$e.f <- fn(m$residuals)
tt
# Fit
gg.m <- ggplot(aes(y=yhat.c, x=yhat.f), data=tt) +
    geom_point(position=position_jitter(width=0.2)) + coord_cartesian(y=c(-3,+3))
# Residuals
gg.e <- ggplot(aes(y=e, x=e.f), data=tt) +
    geom_point() + coord_cartesian(y=c(-3,+3))
# Now plot on the same page using viewports
vp.left <- viewport(width=0.5, height=1, x=0.25, y=0.5)
vp.right <- viewport(width=0.5, height=1, x=0.75, y=0.5)
pdf("../slides/src/rf_plot.pdf", width=8, height=4)
print(gg.m, vp=vp.left)
print(gg.e, vp=vp.right)
dev.off()


qplot(y=yhat, x=yhat.f, data=tt, xlim=c(0,1), geom=c("jitter"), asp=1) + geom_hline(yintercept=quantile(tt$yhat, 0.5, na.rm=TRUE))
ggsave("../slides/src/ne24_qplot.png")

qplot(y=)
plot(m)

str(wdt)
# Mortality by site
with(wdt, tapply(mort.itu, ne.24.hosp,  describe))
wdt[, mort.itu := factor(mort.itu, labels=c("Alive", "Dead"))]
with(wdt, mosaicplot(ne.24.hosp ~ mort.itu, color=c("grey80", "grey0"), border=FALSE))
dev.copy2pdf(file="../slides/src/hospmort_mosaic.pdf") 

str(wdt)
gg.1 <- ggplot(aes(y=ne.24), data=wdt)
# All patients
gg.1 + geom_jitter(aes(x=all)) + geom_hline(y=mean(ne.24, na.rm=TRUE))

gg.1 + geom_jitter(aes(x=hosp))