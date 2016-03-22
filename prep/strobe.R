# For debugging and interactive coding
# rm(list=ls(all=TRUE))
# source(file="../prep/load.R")
load(file='../data/cleaned.Rdata')
library(data.table)
library(Hmisc)
wdt.original <- wdt

#  =======================================
#  = Generate working data and subgroups =
#  =======================================


names(wdt.original)
with(wdt.original, table(los.itu,mort.itu))

describe(wdt.original$los.itu)
# LOS post start of Norad (i.e. patients who start Norad in ITU)
describe(wdt.original$los.ne)

# Basic inclusion criteria
# - on Norad
# - adult
# - in ICU for > 24hrs post-onset of septic shock 

# Define inclusion
nrow(wdt.original)
nrow(wdt.original[los.itu==0])
nrow(wdt.original[los.ne==0]) # but lot's of missingness

# Additional logic that makes defines patient's as dead within 24h if missing vars
wdt.original[, dead.by.miss := ifelse(mort.itu==1 & (los.ne <= 2 | is.na(los.ne))  & is.na(hr.24) & is.na(bps.24), 1, 0)]
table(wdt.original$dead.by.miss)

wdt.original[, exclude.los.itu.0 := ifelse(los.itu==0 | los.ne==0 | dead.by.miss == 1,1,0)]
wdt.original[, exclude.los.itu.0 := ifelse(is.na(exclude.los.itu.0),0,exclude.los.itu.0)]
nrow(wdt.original[exclude.los.itu.0==1])
nrow(wdt.original[ne.1==0])
nrow(wdt.original[age<18])
nrow(wdt.original[is.na(ne.1)])
nrow(wdt.original[is.na(age)])
with(wdt.original, table(exclude.los.itu.0, is.na(ne.1)))

wdt.original[, include := ifelse(age>=18 & ne.1 > 0 & los.itu > 0 & exclude.los.itu.0 == 0 , 1, 0) ]
describe(wdt.original$include)
wdt.original[, include := ifelse(is.na(include),0,include)]
describe(wdt.original$include)

# Define post inclusion exclusions
wdt.original[, exclude.rx.betablock := ifelse(rx.betablock==FALSE | is.na(rx.betablock),0,1)]
wdt.original[, exclude.ne.24 := ifelse(ne.24 > 0 & !is.na(ne.24),0,1)]
wdt.original[, exclude.hr.24 := ifelse(hr.24 >= 95 & !is.na(hr.24),0,1)]

nrow(wdt.original)
nrow(wdt.original[include==1])
nrow(wdt.original[exclude.rx.betablock==1])
nrow(wdt.original[exclude.ne.24==1])
nrow(wdt.original[exclude.hr.24==1])

wdt.original[, grp.all := 1]
wdt.original[, grp.ne24 := ifelse(include==1 & exclude.ne.24 == 0, 1, 0)]
wdt.original[, grp.morelli := ifelse(include==1 & exclude.rx.betablock == 0 & exclude.ne.24 == 0 & exclude.hr.24 == 0, 1, 0)]

# Prepare STROBE data
wdt <- wdt.original[include==1]

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

# Prepare hospital name sorted by mean mortality
# do this after selection
table(wdt$hosp.id)
m <- wdt[, .(s = mean(mort.itu, na.rm=TRUE)), by=hosp.id]
setorder(m, s)
m[, hosp.id.sort := .I ]
m[, hosp.id.sort := factor(hosp.id.sort, labels=m$hosp.id, ordered=TRUE)]
setkey(wdt,hosp.id)
wdt <- wdt[m[,.(hosp.id,hosp.id.sort)]]
# str(wdt)

source(file="prep_vars.R")
save(wdt, wdt.original, file='../data/strobe.Rdata')