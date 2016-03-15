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

# Load functions
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

# function to produce coefficient plots
coef.plot <- function(model=m) {

	# Load packages
	require(data.table)
	require(ggplot2)

	# Test the model class
	if(class(m)=="lm") {
		m.coef <- summary(m)$coefficients
		m.coef <- data.frame(summary(m)$coefficients)
		colnames(m.coef)[1] <- "est"
		colnames(m.coef)[2] <- "se"
		colnames(m.coef)[3] <- "t"
		colnames(m.coef)[4] <- "p"
	} else if (class(m)=="lmerMod") {
		m.coef <- data.frame(summary(m)$coefficients)
		colnames(m.coef)[1] <- "est"
		colnames(m.coef)[2] <- "se"
		colnames(m.coef)[3] <- "t"
	} else {
		warning("Unknown model class: exiting function call")
		return(NULL)
	}

	# Convert to data.table and keep rownames
	m.coef <- data.table(m.coef, keep.rownames=TRUE)
	# Remove the intercept
	m.coef <- m.coef[rn!="(Intercept)"]
	print(m.coef)

	gg.p <- ggplot(data=m.coef, aes(x=rn, y=est)) + 
		geom_hline(yintercept=0, colour="grey", ) +
		geom_linerange(aes(ymin=est - 2 * se, ymax=est + 2 * se), colour="grey") +
		geom_pointrange(aes(ymin=est - 1 * se, ymax=est + 1 * se), lwd=0.5, fatten=2) +
		xlab("Predictor") + ylab("Effect estimate") +
		coord_flip() +
		theme_minimal()
	return(gg.p)
}




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
m <- lm(ne.24.log ~ age, data=wdt)
# How to extract model parameter estimates for coefplot
class(m)
summary(m)$coefficients
str(summary(m)$coefficients)


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


names(wdt)
describe(wdt$peep.24)
describe(wdt$mv.24)
nrow(wdt)
# Add in baseline vars
m <- lmer(ne.24.log ~
	male +
	rescale(age) +
	sepsis.site +
	rescale(sofa.1.nocvs) +
	rescale(ne.1) +
	rescale(fb.24) +
	# rescale(peep.24) +
	mv.24 +
	rescale(sedation.24) +
	1 + (1 | hosp.id), data=wdt)
display(m)
# - [ ] TODO(2016-03-11): Now plot as per coef-plot @done
coef.plot(m)
ICC(m)

# - [ ] TODO(2016-03-11): include the ranef coefficients in the plot for scale

# - [ ] TODO(2016-03-15): now model 1st 24 hr fluid balance
# should move this to its own file?
names(wdt)
m <- lmer(fb.24 ~
	male +
	rescale(age) +
	weight +
	sepsis.site +
	rescale(sofa.1.nocvs) +
	rescale(ne.1) +
	# rescale(peep.24) +
	mv.24 +
	rescale(sedation.24) +
	1 + (1 | hosp.id), data=wdt)
display(m)
# - [ ] TODO(2016-03-11): Now plot as per coef-plot @done
coef.plot(m)
ICC(m)

# - [ ] TODO(2016-03-15): now model MAP at 24h
names(wdt)
summary(wdt$map.24)
qplot(wdt$map.24)
# looks normal therefore no transformation needed of depvar

m <- lmer(map.24 ~
	male +
	rescale(age) +
	weight +
	sepsis.site +
	rescale(sofa.1.nocvs) +
	rescale(ne.1) +
	rescale(ne.24) +
	rescale(fb.24) +
	# rescale(peep.24) +
	mv.24 +
	rescale(sedation.24) +
	1 + (1 | hosp.id), data=wdt)
display(m)
# - [ ] TODO(2016-03-11): Now plot as per coef-plot @done
coef.plot(m)
ICC(m)

