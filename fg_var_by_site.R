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
library(data.table) # NB setnames and setorder only exist in >v1.9 library(ggplot2)
library(reshape2)
library(ggplot2)

rm(list=ls(all=TRUE))
load(file='../data/working.RData')
wdt.original <- wdt
wdt$sample_N <- 1
str(wdt)

# Reshape to long for norad and map
wdt.melt <- melt(wdt[,list(hosp, id.hosp,
	ne.1, ne.24, map.1, map.24, hr.1, hr.24,
	sedation.1, sedation.24,
	sofa.1, sofa.24, fin.24, fb.24)], id.vars=c('hosp', 'id.hosp'))
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
ggsave(
	filename = '../outputs/figs/site_sofa_seq.png',
	width=8,
	height=4
	)


# Sedation
g_sedation <- ggplot(wdt2, aes(x=id.hosp, y=sedation))

g_sedation +
	facet_grid(time ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 10)) +
	labs(list(
		title = 'Sedation score by patient sequence',
		x = 'Patient ID',
		y = 'Sedation score'))
ggsave(
	filename = '../outputs/figs/site_sedation_seq.png',
	width=8,
	height=4
	)


g_sedation <- ggplot(wdt2, aes(hosp, sedation))
g_sedation + geom_boxplot(notch=TRUE) +
	facet_grid(. ~ time, labeller = label_both) +
	coord_cartesian(ylim = c(0, 10)) +
	labs(list(
		title = 'Distribution of sedation scores',
		x = 'Hospital',
		y = 'Sedation score'))
ggsave(
	filename = '../outputs/figs/site_sedation_dist.png',
	width=8,
	height=4
	)

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
ggsave(
	filename = '../outputs/figs/site_norad_seq.png',
	width=8,
	height=4
	)

g_ne <- ggplot(wdt2, aes(hosp, ne))
g_ne + geom_violin() +
	facet_grid(. ~ time, labeller = label_both) +
	coord_cartesian(ylim = c(0, 1.5)) +
	labs(list(
		title = 'Distribution of Noradrenaline dose',
		x = 'Patient ID',
		y = 'Noradrenaline (mcg/kg/min)'))
ggsave(
	filename = '../outputs/figs/site_norad_dist_violin.png',
	width=8,
	height=4
	)

g_ne <- ggplot(wdt2, aes(hosp, ne))
# g_ne + geom_violin() +
g_ne + geom_boxplot(notch=TRUE) +
	facet_grid(. ~ time, labeller = label_both) +
	coord_cartesian(ylim = c(0, 1.5)) +
	labs(list(
		title = 'Distribution of Noradrenaline dose',
		x = 'Patient ID',
		y = 'Noradrenaline (mcg/kg/min)'))
ggsave(
	filename = '../outputs/figs/site_norad_dist.png',
	width=8,
	height=4
	)

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
ggsave(
	filename = '../outputs/figs/site_map_seq.png',
	width=8,
	height=4
	)

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
ggsave(
	filename = '../outputs/figs/site_hrate_seq.png',
	width=8,
	height=4
	)

# Fluid in 1st 24h
describe(wdt2$fin)
g_fin.24 <- ggplot(wdt2, aes(x=id.hosp, y=fin))
g_fin.24 +
	facet_grid(. ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 15000)) +
	labs(list(
		title = 'Fluid during 1st 24h by patient sequence',
		x = 'Patient ID',
		y = 'Fluid (mls)'))
ggsave(
	filename = '../outputs/figs/site_fin24_seq.png',
	width=8,
	height=3
	)

# FB in 1st 24h
describe(wdt2$fb)
g_fb.24 <- ggplot(wdt2, aes(x=id.hosp, y=fb))
g_fb.24 +
	facet_grid(. ~ hosp, labeller = label_both) +
	geom_point(alpha=0.3, size=1.5) +
	geom_smooth(method="lm") +
	coord_cartesian(xlim = c(0, 110), ylim = c(0, 15000)) +
	labs(list(
		title = 'Fluid BALANCE during 1st 24h by patient sequence',
		x = 'Patient ID',
		y = 'Fluid (mls)'))
ggsave(
	filename = '../outputs/figs/site_fb24_seq.png',
	width=8,
	height=3
	)
