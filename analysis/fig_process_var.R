# author: Steve Harris
# date: 2016-03-09
# subject: Plots to demonstrate variablility in process measures

# Readme
# ======


# Todo
# ====
# - [ ] heart rate
# - [ ] blood pressure
# - [ ] norad dose
# - [ ] first 24 h fluids
# - [ ] sedation
# - [ ] steroids
# - [ ] vadi


# Log
# ===
# 2016-03-09
# - file created

require(Hmisc)
require(data.table)
require(ggplot2)

rm(list=ls(all=TRUE))
load(file='../data/cleaned.RData')

# Generate working data
source("../prep/prep_vars.R")
source("../prep/strobe.R")
# this saves the data to tdt so rename
wdt <- tdt

str(wdt)
p <- ggplot(data=wdt, aes(y=hr.24, x=hosp.id.sort))
p + geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim=c(40,160)) +
	theme_minimal()

p <- ggplot(data=wdt, aes(y=map.24, x=hosp.id.sort))
p + geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim=c(40,120)) +
	theme_minimal()

