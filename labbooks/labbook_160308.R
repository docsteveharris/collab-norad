# Scratch work while writing the paper
rm(list=ls(all=TRUE))
source(file="../prep/load.R")
source(file="../share/functions4rmd.R")
load(file='../data/cleaned.Rdata')
library(dsbc)

#  =====================
#  = Define population =
#  =====================
# Patients meeting inclusion
describe(wdt$age)
wdt[, include := ifelse(age>=18, 1,0) ]
describe(wdt$ne.1)
wdt[, include := ifelse(age>=18, 1,0) ]

# Patients to be excluded
describe(wdt$pmh.betablock)

# Exclude patients discharged within 24h
lookfor(los)
describe(wdt$los.itu)
nrow(wdt[los.itu==0])

# Exclude patients not surviving 24 hr
lookfor(mort)
describe(wdt$mort.itu)
with(wdt[los.itu<=7], table(los.itu, mort.itu))
wdt[, exclude.los.itu.0 := ifelse(los.itu<=0,1,0)]

# Exclude patients with beta blockade Rx in 1st 24h
describe(wdt$rx.betablock)
with(wdt, table(hosp, rx.betablock))
wdt$exclude.rx.betablock <- NULL
wdt[, exclude.rx.betablock := ifelse(rx.betablock==TRUE | is.na(rx.betablock),1,0)]
describe(wdt$exclude.rx.betablock)

# Exclude patients off norad by 24h
describe(wdt$ne.24)
wdt[, exclude.ne.24 := ifelse(ne.24 < 0.1 | is.na(ne.24),1,0)]
describe(wdt$exclude.ne.24)

tdt <- wdt[include==1 & exclude.los.itu.0==0 & exclude.rx.betablock==0 & exclude.ne.24 ==0]
nrow(tdt)
describe(tdt$hosp)

#  =======================
#  = Describe population =
#  =======================
