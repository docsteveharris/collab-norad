# author: Steve Harris
# date: 2016-03-11
# subject: model noradrenaline dose at 24 h

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2016-03-11
# - file created
library(dsbc)
library(arm)
library(ggplot2)
library(reshape2)

rm(list=ls(all=TRUE))
source(file="../prep/load.R")
source(file="../share/functions4rmd.R")
load(file='../data/cleaned.Rdata')
source(file="../prep/strobe.R")
wdt.orginal <- wdt
wdt <- tdt
source(file="../prep/prep_vars.R")



# Define a good method for rescaling norad
summary(wdt$ne.1)
summary(wdt$ne.24)
sd(wdt$ne.1)
sd(wdt$ne.24)

wdt.melt <- melt(wdt[,.(id, hosp.id,mort.itu, ne.1, ne.24)], id=c("id","hosp.id","mort.itu"))
head(wdt.melt)

# Interesting completely different direction of effect of norad at two timepoints
gg.q <- qplot(x=log(value), y=mort.itu, data=wdt.melt, geom = c('smooth') )
gg.q 	+
	facet_grid(.~variable) +
	geom_rug(data=wdt.melt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
	geom_rug(data=wdt.melt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
	coord_cartesian(x=c(0,2)) +
	theme_minimal()

# Now model ne.24 dose
# Not unreasonable to log transform this
qplot(wdt$ne.24)
wdt[, ne.24.log := log(ne.24)]
qplot(wdt$ne.24.log)
describe(wdt$ne.24.log)

# Rough working for how to generate model
m <- lm(ne.24.log ~ 1, data=wdt)
display(m)
# exponentiate predictions to return to original scale
yhat <- exp(predict(m, interval="confidence"))
head(yhat)
# can see that the mean prediction is the same in the null model
exp(mean(log(wdt$ne.24)))

# Now model fully
library(lme4)
m <- lmer(ne.24.log ~ 1 + (1 | hosp.id), data=wdt)
display(m)
m <- lmer(ne.24.log ~ rescale(log(ne.1)) + (1 | hosp.id), data=wdt)
display(m)
# view the random effects
coef(m) 
# view the fixed effects
fixef(m)
# view the random effects (which are the same as coef(m))
ranef(m)
ranef(m)$hosp.id[,1] + fixef(m)[1]
# view the standard errors of the random effects
se.ranef(m)

# intra-class correlation
ICC <- function(m) {
	require(lme4)
	v <- VarCorr(m)
	print(v)
	# get the group level standard deviation
	sd.g <- attr(v[[1]], "stddev")
	# get the residual standard deviation
	sd.r <- attr(v, "sc")
	# calculate the ICC
	icc <- (sd.g^2 / (sd.g^2 + sd.r^2))
	# set the name
	names(icc)[1] <- "ICC"
	# calculate the variance ratio
	var.ratio <- sd.g^2 / sd.r^2
	names(var.ratio)[1] <- "Variance ratio"
	return(c(icc,var.ratio))
}

# Add in baseline vars
m <- lmer(ne.24.log ~
	male +
	rescale(age) +
	sepsis.site +
	rescale(sofa.1.nocvs) +
	rescale(ne.1) +
	rescale(fb.24) +
	rescale(sedation.24) +
	1 + (1 | hosp.id), data=wdt)
display(m)
ICC(m)

# - [ ] TODO(2016-03-11): Now plot as per coef-plot 
# - [ ] TODO(2016-03-11): include the ranef coefficients in the plot for scale


# Playing ...
# now model 1st 24 hr fluid balance
# Add in baseline vars
qplot(wdt$fb.24)
m <- lmer((fb.24) ~
	male +
	rescale(age) +
	sepsis.site +
	rescale(sofa.1.nocvs) +
	rescale(ne.1) +
	rescale(sedation.24) +
	1 + (1 | hosp.id), data=wdt)
display(m)
ICC(m)

