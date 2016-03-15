# Scratch work while writing the paper
rm(list=ls(all=TRUE))
source(file="../prep/load.R")
source(file="../share/functions4rmd.R")
load(file='../data/cleaned.Rdata')
source(file="../prep/prep_vars.R")
source(file="../prep/strobe.R")

library(dsbc)
library(arm)
library(ggplot2)

ls()
wdt.orginal <- wdt
wdt <- tdt
nrow(wdt)
names(wdt)
lookfor(fb)


# Mortality by heart rate
library(gmodels)
with(wdt, CrossTable(mort.itu, hr.high))


# Understand mean arterial pressure by centre
with(wdt, tapply(map.24, hosp.id.sort, mean, na.rm=TRUE))
with(wdt, tapply(map.24, hosp.id.sort, summary, na.rm=TRUE))
with(wdt, tapply(ne.24, hosp.id.sort, mean, na.rm=TRUE))
with(wdt, tapply(ne.24, hosp, mean, na.rm=TRUE))
