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
nrow(wdt[los.itu==0])
with(wdt[los.itu<=7], table(los.itu, mort.itu))
nrow(wdt[los.ne==0])
with(wdt[los.ne<=7], table(los.ne, mort.itu))
wdt[, exclude.los.itu.0 := ifelse(los.itu==0 | los.ne==0,1,0)]
wdt[, exclude.los.itu.0 := ifelse(is.na(exclude.los.itu.0),0,exclude.los.itu.0)]
describe(wdt$exclude.los.itu.0)

wdt[, exclude.rx.betablock := ifelse(rx.betablock==FALSE | is.na(rx.betablock),0,1)]

describe(wdt$ne.24)
nrow(wdt[ne.24==0])
wdt[, exclude.ne.24 := ifelse(ne.24 > 0 & !is.na(ne.24),0,1)]


# Encode the hospitals
(dt.hosp <- data.table(hosp=unique(wdt$hosp), hosp.id=rnorm(8)))
setorder(dt.hosp,hosp.id)
dt.hosp[,hosp.id := LETTERS[.I]]
dt.hosp
setkey(dt.hosp, hosp)
setkey(wdt, hosp)
wdt <- dt.hosp[wdt]
wdt[, hosp.id:=factor(hosp.id)]
setkey(wdt,id)
str(wdt)

# Prepare STROBE data
tdt <- wdt[include==1 & exclude.los.itu.0==0 & exclude.rx.betablock==0 & exclude.ne.24 ==0]
# names(tdt)

# Prepare hospital name sorted by mean mortality
# do this after selection
table(tdt$hosp.id)
m <- tdt[, .(s = mean(mort.itu, na.rm=TRUE)), by=hosp.id]
setorder(m, s)
m[, hosp.id.sort := .I ]
m[, hosp.id.sort := factor(hosp.id.sort, labels=m$hosp.id, ordered=TRUE)]
setkey(tdt,hosp.id)
tdt <- tdt[m[,.(hosp.id,hosp.id.sort)]]
# str(tdt)
