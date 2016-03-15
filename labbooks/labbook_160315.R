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