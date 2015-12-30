# Melt variables available by time into long form

rm(list=ls(all=TRUE))
require(data.table)
require(assertthat)
load(file='../data/cleaned.Rdata')

wdt.original <- wdt
wdt$sample_N <- 1
wdt$all <- 1
wdt[, all := factor(all, label=("All patients"))]
str(wdt)
assert_that(nrow(wdt)==736)
names(wdt)

# Prepare hospital name sorted by mean mortality
m <- wdt[, .(s = mean(mort.itu, na.rm=TRUE)), by=hosp]
setorder(m, s)
m[, mort.itu.order := .I ]
m[, mort.itu.order := factor(mort.itu.order, labels=m$hosp, ordered=TRUE)]
setkey(wdt,hosp)
wdt <- wdt[m[,.(hosp,mort.itu.order)]]

wdt.melt <- melt(wdt[,list(hosp, id.hosp, mort.itu.order,
	ne.1, ne.24, map.1, map.24, hr.1, hr.24,
	sedation.1, sedation.24,
	lac.1, lac.24,
	pf.1, pf.24,
	sofa.1, sofa.24,
	fin.24, fb.24
	)], id.vars=c('hosp', 'id.hosp', 'mort.itu.order'))

# TODO: 2014-11-30 - [ ] need to convert melt table back to data.table

wdt.melt
wdt.melt[,variable := as.character(variable)]
# Extract var and then time
wdt.melt[, var := gsub("([a-z]+)\\.([0-9]+)", "\\1", variable, perl=TRUE)]
wdt.melt[, time := gsub("(\\w+)\\.(\\d+)", "\\2", variable, perl=TRUE)]
table(wdt.melt$var)
table(wdt.melt$time)

# Drop old variable
wdt.melt[, variable := NULL]
wdt.melt
# Convert back to 
wdt.long <- dcast.data.table(wdt.melt, hosp + id.hosp + mort.itu.order + value + time ~ var)
setorder(wdt.long, mort.itu.order)
wdt.long

save(wdt.long, file='../data/clean_long.RData')

