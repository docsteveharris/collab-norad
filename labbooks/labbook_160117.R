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

# Could you look at the NADR database for the UK hospitals only and see
# how many received 0.1-0.3 mcg/kg/min and how many received >0.3
# mcg/kg/min NADR within the first 24h .. and over which timeline?
# Reason I ask is that the beta blocker study we’re proposing needs to
# be powered and we need to ensure enough of a sample size.
table(wdt$hosp)
tdt <- wdt[hosp %in% c("Belfast", "Royal London", "GSTT", "UCLH")]
nrow(tdt)
describe(tdt$ne.1)


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





