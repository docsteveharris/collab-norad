# Working for analysis questionsrm(list=ls(all=TRUE))

#  =============
#  = Libraries =
#  =============
require(Hmisc)
require(assertthat)
require(gmodels)
require(descr)
require(data.table)
require(XLConnect)

rm(list=ls(all=TRUE))
load(file='../data/cleaned.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==736)

# Let's just start by understanding the main outcome
attach(wdt)
describe(mort.itu)
describe(mort.hosp)
# How about trying to examine 7-day mortality to avoid issues with discharge practices
describe(los.itu)
describe(wdt[los.itu<=7]$mort.itu)
# Suggests that 327-144 patients survive and are discharge from ITU within 7d

