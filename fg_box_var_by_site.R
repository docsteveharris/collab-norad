# author: Steve Harris
# date: 2014-10-13
# subject: Plots by site

# Readme
# ======
# Box plot version


# Todo
# ====


# Log
# ===
# 2014-10-13
# - file created
# 2014-11-29
# - cloned from patient sequence version

rm(list=ls(all=TRUE))
source(file="load.R")
load(file='../data/cleaned.Rdata')


wdt.original <- wdt
wdt$sample_N <- 1
str(wdt)

# Reshape to long for norad and map
# NOTE: 2014-11-29 - [ ] to make melt work with fb.mean you need to append .1
setnames(wdt, 'fb.mean', 'fbmean.1')
wdt.melt <- melt(wdt[,list(hosp, id.hosp,
	ne.1, ne.24, map.1, map.24, hr.1, hr.24,
	sedation.1, sedation.24,
	lac.1, lac.24,
	pf.1, pf.24,
	sofa.1, sofa.24, fin.24, fb.24, fbmean.1)], id.vars=c('hosp', 'id.hosp'))

# TODO: 2014-11-30 - [ ] need to convert melt table back to data.table

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
g_sofa <- ggplot(wdt2, aes(x=hosp, y=sofa))
g_sofa +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 24)) +
	labs(list(
		title = 'SOFA',
		x = 'Patient ID',
		y = 'SOFA'))
ggsave(
	filename = '../outputs/figs/site_sofa_box.png',
	width=4,
	height=8
	)


# Sedation
g_sedation <- ggplot(wdt2, aes(x=hosp, y=sedation))

g_sedation +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 10)) +
	labs(list(
		title = 'Sedation score',
		x = 'Patient ID',
		y = 'Sedation score'))
ggsave(
	filename = '../outputs/figs/site_sedation_box.png',
	width=4,
	height=8
	)


# Noradrenaline doses
g_ne <- ggplot(wdt2, aes(x=hosp, y=ne))
g_ne +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 1.5)) +
	labs(list(
		title = 'Noradrenaline dose',
		x = 'Patient ID',
		y = 'Noradrenaline (mcg/kg/min)'))
ggsave(
	filename = '../outputs/figs/site_norad_box.png',
	width=4,
	height=8
	)

# Mean arterial pressure
g_map <- ggplot(wdt2, aes(x=hosp, y=map))
g_map +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(25, 125)) +
	labs(list(
		title = 'Mean Arterial Pressure',
		x = 'Patient ID',
		y = 'MAP'))
ggsave(
	filename = '../outputs/figs/site_map_box.png',
	width=4,
	height=8
	)

# Heart rate
g_hr <- ggplot(wdt2, aes(x=hosp, y=hr))
g_hr +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(25, 175)) +
	labs(list(
		title = 'Heart rate',
		x = 'Patient ID',
		y = 'HR'))
ggsave(
	filename = '../outputs/figs/site_hrate_box.png',
	width=4,
	height=8
	)

# Fluid in 1st 24h
describe(wdt2$fin)
g_fin.24 <- ggplot(wdt2, aes(x=hosp, y=fin))
g_fin.24 +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 15000)) +
	labs(list(
		title = 'Fluid during 1st 24h',
		x = 'Patient ID',
		y = 'Fluid (mls)'))
ggsave(
	filename = '../outputs/figs/site_fin24_box.png',
	width=4,
	height=6
	)

# FB in 1st 24h
describe(wdt2$fb)
g_fb.24 <- ggplot(wdt2, aes(x=hosp, y=fb))
g_fb.24 +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 15000)) +
	labs(list(
		title = 'Fluid BALANCE during 1st 24h',
		x = 'Patient ID',
		y = 'Fluid (mls)'))
ggsave(
	filename = '../outputs/figs/site_fb24_box.png',
	width=4,
	height=6
	)

# FB mean (cumulative divided by number days)
describe(wdt2$fb)
g_fb.mean <- ggplot(wdt2, aes(x=hosp, y=fbmean))
g_fb.mean +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 5000)) +
	labs(list(
		title = 'Mean fluid balance',
		x = 'Patient ID',
		y = 'Fluid (mls)'))
ggsave(
	filename = '../outputs/figs/site_fbmean_box.png',
	width=4,
	height=6
	)


# Lactate
describe(wdt2$lac)
g_lac <- ggplot(wdt2, aes(x=hosp, y=lac))
g_lac +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 10)) +
	labs(list(
		title = 'Lactate',
		x = 'Patient ID',
		y = 'Lactate (mmol/l)'))
ggsave(
	filename = '../outputs/figs/site_lac_box.png',
	width=8,
	height=3
	)


# PF ratio
describe(wdt2$PF)
g_PF <- ggplot(wdt2, aes(x=hosp, y=pf))
g_PF +
	facet_grid(time ~ ., labeller = label_both) +
	geom_boxplot(notch=TRUE) +
	coord_cartesian(ylim = c(0, 100)) +
	labs(list(
		title = 'PF ratio',
		x = 'Patient ID',
		y = 'PF ratio (kPa)'))
ggsave(
	filename = '../outputs/figs/site_PF_box.png',
	width=8,
	height=3
	)

