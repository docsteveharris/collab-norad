# author: Steve Harris
# date: 2014-10-13
# subject: Plots by site

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2014-10-13
# - file created

#  =====================
#  = Load dependencies =
#  =====================
library(data.table) # NB setnames and setorder only exist in >v1.9
library(ggplot2)
library(reshape2)

rm(list=ls(all=TRUE))
load(file='../data/working.RData')
wdt.original <- wdt
wdt$sample_N <- 1
str(wdt)

# Reshape to long for norad and map
wdt.melt <- melt(wdt[,list(hosp, id.hosp, ne.1, ne.24, map.1, map.24, hr.1, hr.24, sofa.1, sofa.24)], id.vars=c('hosp', 'id.hosp'))
wdt.melt[,variable := as.character(variable)]
str(wdt.melt)
wdt.melt[, var := gsub("([a-z]+)\\.([0-9]+)", "\\1", variable, perl=TRUE)]
str(wdt.melt)
table(wdt.melt$var)
wdt.melt[, time := gsub("(\\w+)\\.(\\d+)", "\\2", variable, perl=TRUE)]
wdt.melt[, variable := NULL]
str(wdt.melt)
table(wdt.melt$time)

wdt2 <- dcast.data.table(wdt.melt, hosp + id.hosp + value + time ~ var)
setorder(wdt2, hosp, id.hosp)
str(wdt2)

# SOFA
g_sofa <- ggplot(wdt2, aes(x=id.hosp, y=sofa))

g_sofa +
	facet_grid(time ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 24)) +
	labs(list(
		title = 'SOFA by patient sequence',
		x = 'Patient ID',
		y = 'SOFA'))

# Noradrenaline doses
g_ne <- ggplot(wdt2, aes(x=id.hosp, y=ne))

g_ne +
	facet_grid(time ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 1.5)) +
	labs(list(
		title = 'Noradrenaline dose by patient sequence',
		x = 'Patient ID',
		y = 'Noradrenaline (mcg/kg/min)'))

# Mean arterial pressure
g_map <- ggplot(wdt2, aes(x=id.hosp, y=map))

g_map +
	facet_grid(time ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(25, 150)) +
	labs(list(
		title = 'Mean Arterial Pressure by patient sequence',
		x = 'Patient ID',
		y = 'MAP'))

# Heart rate
g_hr <- ggplot(wdt2, aes(x=id.hosp, y=hr))

g_hr +
	facet_grid(time ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 200)) +
	labs(list(
		title = 'Heart rate by patient sequence',
		x = 'Patient ID',
		y = 'HR'))


