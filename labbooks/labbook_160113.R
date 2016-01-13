# Plot heatmap for norad and blood pressure against mortality
# - overall and by site
rm(list=ls(all=TRUE))

# Set up and load data

require(Hmisc)
require(assertthat)
require(gmodels)
require(data.table)
require(ggplot2)
require(arm) 			# for display
require(hexbin)

load(file='../data/cleaned.Rdata')
load(file='../data/clean_long.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==736)
names(wdt)

describe(wdt$ne.1)
describe(wdt$map.1)
describe(wdt$hr.1)

# Overall heatmapt
ggplot(data=wdt, aes(x=map.1, y=ne.1, z=mort.itu)) +
	stat_summary_hex(
		fun = function(z) ifelse(length(z) >= 4, mean(z), NA),
		bins=10) +
	xlim(40,100) + ylim(0,1) +
	theme_minimal()

str(wdt)
# Overall heatmap but facet by site
ggplot(data=wdt, aes(x=map.1, y=ne.1, z=mort.itu)) +
	stat_summary_hex(
		fun = function(z) ifelse(length(z) >= 3, mean(z), NA),
		bins=5) +
	facet_wrap(~mort.itu.order, nrow=2) +
	xlim(40,100) + ylim(0,1) +
	theme_minimal() 


