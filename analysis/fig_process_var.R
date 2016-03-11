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

# Heart rate
p <- ggplot(data=wdt, aes(y=hr.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(40,160)) +
	ylab("Heart rate (per min)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_hr.pdf", width=3, height=3, scale=2)

# MAP
p <- ggplot(data=wdt, aes(y=map.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(40,100)) +
	ylab("Mean arterial pressure (mmHg)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_map.pdf", width=3, height=3, scale=2)

# Noradrenaline
p <- ggplot(data=wdt, aes(y=ne.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(0,2)) +
	ylab("Noradrenaline dose (mcg/kg/min)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_ne24.pdf", width=3, height=3, scale=2)

# Fluids
p <- ggplot(data=wdt, aes(y=fb.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(-2500,12500)) +
	ylab("Fluid balance (mls)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_fb24.pdf", width=3, height=3, scale=2)

# # Sedation
# p <- ggplot(data=wdt, aes(y=sedation.24, x=hosp.id.sort))
# p + geom_boxplot(notch=TRUE) +
# 	coord_cartesian(ylim=c(0,10)) +
# 	theme_minimal()
	

