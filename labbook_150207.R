

rm(list=ls(all=TRUE))
source(file="load.R")
load(file='../data/cleaned.Rdata')

wdt.original <- wdt
ls()

describe(wdt$ne.1)
describe(wdt$ne.24)

# Patients with ne.1 > 0.3 at t0 and < 0.3 at 24
wdt[, ne.1.3 := ifelse(ne.1 > 0.3,1,0)]
wdt[, ne.24.lt3 := ifelse(ne.24 < 0.3 & ne.1.3 == 1,1,0)]
with(wdt, table(ne.1.3, ne.24.lt3))

wdt[, ne.1.15 := ifelse(ne.1 > 0.15,1,0)]
wdt[, ne.24.lt15 := ifelse(ne.24 < 0.15 & ne.1.15 == 1,1,0)]
with(wdt, table(ne.1.15, ne.24.lt15))

