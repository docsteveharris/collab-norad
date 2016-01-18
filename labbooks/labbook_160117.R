# 2016-01-17
# Data request from Merv
# Greetings from deepest Essex. 
# Can you do us a big quick favour …
# Could you look at the NADR database for the UK hospitals only and see how many received 0.1-0.3 mcg/kg/min and how many received >0.3 mcg/kg/min NADR within the first 24h .. and over which timeline? Reason I ask is that the beta blocker study we’re proposing needs to be powered and we need to ensure enough of a sample size.
# Ta++++++
# m
# Sorry – can I add mortalities (and SOFA scores for survivors and non-survivors) for max dose in first 24h of 0.1-0.3 and for >0.3 as well????
# Ta+++++
# M

rm(list=ls(all=TRUE))

# Set up and load data

require(Hmisc)
require(assertthat)
require(gmodels)
require(data.table)
require(ggplot2)
require(arm) 			# for display
require(hexbin)
require(pander)
panderOptions('table.split.table', Inf)
panderOptions('table.alignment.default', 'right')
panderOptions('table.style', 'rmarkdown')

load(file='../data/cleaned.Rdata')
load(file='../data/clean_long.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==736)
names(wdt)
ls()

# Could you look at the NADR database for the UK hospitals only and see
# how many received 0.1-0.3 mcg/kg/min and how many received >0.3
# mcg/kg/min NADR within the first 24h .. and over which timeline?
# Reason I ask is that the beta blocker study we’re proposing needs to
# be powered and we need to ensure enough of a sample size.
table(wdt$hosp)
tdt <- wdt[hosp %in% c("Belfast", "Royal London", "GSTT", "UCLH")]
nrow(tdt)
describe(tdt$ne.1)

# best way to get quick contingency table to markdown
t <- with(tdt, table(mort.itu, ne.24.cat))
prop.table(t) # table percentages
prop.table(t,1) # row percentages
prop.table(t,2) # col percentages
margin.table(t,1) # row margins
margin.table(t,2) # col margins

# Project mosaic
require(mosaic)
tally(mort.itu ~ ne.24.cat, data=tdt, margins=TRUE)
pander(tally(mort.itu ~ ne.24.cat, data=tdt, margins=TRUE))
pander(tally(~ mort.itu | ne.24.cat, data=tdt, margins=TRUE))
pander(tally(mort.itu ~ ne.24.cat, data=tdt, margins=TRUE, format="percent"))
pander(tally(mort.itu ~ ne.24.cat, data=tdt, margins=TRUE, format="percent"))
pander(tally(~ mort.itu | ne.24.cat, data=tdt, margins=TRUE, format="percent"))

# Pander and CrossTable
# - [ ] NOTE(2016-01-18): supposed to work with CrossTable class but doesn't
require(gmodels)
with(tdt, CrossTable(mort.itu, ne.24.cat, prop.t=FALSE, prop.chisq=FALSE))
t <- with(tdt, CrossTable(mort.itu, ne.24.cat, prop.t=FALSE, prop.chisq=FALSE))
class(t)
pander(with(tdt, CrossTable(mort.itu, ne.24.cat, prop.t=FALSE, prop.chisq=FALSE)))

# kable function from knitr
t <- tally(~ mort.itu | ne.24.cat, data=tdt, margins=TRUE)
class(t)
kable(t, format="pandoc", digits=1)
kable(tally(~ mort.itu | ne.24.cat, data=tdt, margins=TRUE, format="percent"),
	caption="ITU mortality by noradrenaline dose at 24 hours ",
	format="pandoc", digits=1)

print("Norad for UK for UK hospitals - categories at 1 hour")
tdt[, ne.1.cat := cut2(ne.1, cuts=c(0.1,0.3))]
t <- CrossTable(tdt$ne.1.cat)
print("Norad for UK for UK hospitals - categories at 24 hours")
tdt[, ne.24.cat := cut2(ne.24, cuts=c(0.1,0.3))]
t <- CrossTable(tdt$ne.24.cat)

print("Norad for UK for UK hospitals - categories cross-tabulated")
t <- CrossTable(tdt$ne.1.cat, tdt$ne.24.cat, prop.c=F, prop.t=F, prop.chisq=F)


# Sorry – can I add mortalities (and SOFA scores for survivors and non-survivors) for max dose in first 24h of 0.1-0.3 and for >0.3 as well????
print("ITU mortality")
t <- CrossTable(factor(tdt$mort.itu, levels=c(0,1), labels=c("alive", "dead")))
print("Hospital mortality")
t <- CrossTable(factor(tdt$mort.hosp, levels=c(0,1), labels=c("alive", "dead")))

# SOFA score
print("SOFA score - ITU survivors")
print(summary(tdt[mort.itu==0]$sofa.1))

print("SOFA score - ITU nonsurvivors")
print(summary(tdt[mort.itu==1]$sofa.1))

# - [ ] NOTE(2016-01-18): further request Thanks a lot  Steve – u iz a
#   star! Very useful. Could you please look specifically at the NADR
#   >0.3 at 24h group and provide SOFA scores for survivors and non-
#   survivors – many thanks indeed! This is the group we’re particularly
#   interested in as they’re the group likely to be enrolled into the
#   betablocker study All best M

require(mosaic)
require(data.table)
str(tdt)
with(tdt[ne.24>0.3], favstats(sofa.24))
t <- (with(tdt[ne.24>0.3], tapply(sofa.24, mort.itu, favstats)))
t <- rbind(t[[1]], t[[2]])
row.names(t) <- c("Survived", "Died")
kable(t,
	caption="SOFA scores by ITU mortality for patients with Norad>0.3 at 24h")
# Switch back to pander b/c output works better in mailmate
pander(t,
	caption="SOFA scores by ITU mortality for patients with Norad>0.3 at 24h")


pander(tally(~ factor(mort.itu, label=c("Alive", "Dead")) | ne.24.cat, data=tdt, margins=TRUE, format="count"),
	caption="ITU mortality by noradrenaline dose at 24 hours ",
	digits=1)


