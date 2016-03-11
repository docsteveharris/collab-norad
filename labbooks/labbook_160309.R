# Scratch work while writing the paper
rm(list=ls(all=TRUE))
source(file="../prep/load.R")
load(file='../data/cleaned.Rdata')
source(file="../prep/strobe.R")
source(file="../prep/prep_vars.R")
source(file="../share/functions4rmd.R")

library(dsbc)
library(arm)
library(ggplot2)

ls()
wdt.orginal <- wdt
wdt <- tdt
nrow(wdt)
names(wdt)
lookfor(fb)

gg.q <- qplot(x=ne.24, y=mort.itu, data=wdt, geom = c('smooth') )
gg.q 	+
	geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
	geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
	coord_cartesian(x=c(0,2)) +
	theme_minimal()

#  ========================================================
#  = define low/high blood pressure and heart rate groups =
#  ========================================================
wdt[,map.high := ifelse(map.24 >= 75, 1, 0 )]
wdt[,hr.high := ifelse(hr.24 >= 95, 1, 0 )]
with(wdt, table(map.high, hr.high))

#  ====================
#  = Mortality models =
#  ====================

# baseline model
m <- glm(mort.itu ~ 1, data=wdt)
display(m)

# Now build model for outcome
m <- glm(mort.itu ~ map.high, data=wdt)
display(m)
m <- glm(mort.itu ~ hr.high, data=wdt)
display(m)
coefplot(m)
m <- glm(mort.itu ~ map.high + hr.high + map.high:hr.high, data=wdt)
display(m)
coefplot(m)

# Now add in ne.24
m <- glm(mort.itu ~ rescale(ne.24) + map.high + hr.high + map.high:hr.high, data=wdt)
display(m)
coefplot(m)

# and interaction between high heart rate and norad
m <- glm(mort.itu ~ rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high, data=wdt)
display(m)
coefplot(m)

# now control for baseline
names(wdt)
library(Hmisc)
wdt[, sofa.1.cvs := cut2(ne.1, c(0,0.1))]
wdt[, sofa.1.cvs := as.numeric(factor(sofa.1.cvs, labels=c(3,4)))+2]
describe(wdt$sofa.1.cvs)
wdt[, sofa.1.nocvs := sofa.1 - sofa.1.cvs]
describe(wdt$sofa.1.nocvs)

describe(wdt$sepsis.site)
m <- glm(mort.itu ~
	rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high +
	rescale(ne.1) +
	rescale(age) +
	rescale(sofa.1.nocvs) +
	male + sepsis.site
	, data=wdt)
display(m)
coefplot(m)


# fixed effects for site
m <- glm(mort.itu ~
	rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high +
	rescale(ne.1) +
	rescale(age) +
	rescale(sofa.1.nocvs) +
	rescale(fb.24) +
	male + sepsis.site +
	hosp.id
	, data=wdt)
display(m)
coefplot(m)

# random effects
m <- glmer(mort.itu ~
	rescale(ne.24) +
	map.high + hr.high + map.high:hr.high +
	rescale(ne.1) +
	rescale(age) +
	rescale(sofa.1.nocvs) +
	rescale(fb.24) +
	rescale(lac.24) +
	male + sepsis.site +
	(1 | hosp.id)
	, data=wdt, family=binomial(link="logit"))
display(m)
coefplot(m)

# fixed effects for site
describe(wdt$sepsis.site)
m <- glm(mort.itu ~
	rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high +
	rescale(age) + rescale(sofa.1) + male + sepsis.site + hosp.id
	, data=wdt)
display(m)
coefplot(m)

m <- glm(mort.itu ~
	rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high +
	rescale(age) + rescale(sofa.1) + male + sepsis.site + hosp.id + hosp.id:hr.high + rescale(ne.24):hosp.id
	, data=wdt)
display(m)
coefplot(m)

# Now switch to multi-level
# m <- glmer(mort.itu ~ (1) + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
m <- glmer(mort.itu ~
	rescale(ne.24) + rescale(ne.24):hr.high + map.high + hr.high + map.high:hr.high +
	rescale(age) + rescale(sofa.1) + male + sepsis.site +
	(1 | hosp.id)
	, data=wdt, family=binomial(link="logit"))
display(m)
coefplot(m)


