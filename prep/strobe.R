# rm(list=ls(all=TRUE))
# For debugging and interactive coding
# source(file="../prep/load.R")
# Generate working data via strobe
# load(file='../data/cleaned.Rdata')


wdt[, include := ifelse(age>=18 & ne.1 > 0, 1,0) ]
wdt[, include := ifelse(is.na(include),0,include)]
describe(wdt$include)
wdt <- wdt[include==1]

describe(wdt$los.itu)
describe(wdt$los.ne)
nrow(wdt[los.ne==0])
wdt[, exclude.los.itu.0 := ifelse(los.itu<=0 | los.ne==0,1,0)]
with(wdt[los.itu<=7], table(los.itu, mort.itu))

wdt[, exclude.rx.betablock := ifelse(rx.betablock==FALSE | is.na(rx.betablock),0,1)]

describe(wdt$ne.24)
nrow(wdt[ne.24==0])
wdt[, exclude.ne.24 := ifelse(ne.24 > 0 & !is.na(ne.24),0,1)]

tdt <- wdt[include==1 & exclude.los.itu.0==0 & exclude.rx.betablock==0 & exclude.ne.24 ==0]
