# author: Steve Harris
# date: 2014-10-09
# subject: Prepare data for analysis

# Readme
# ======

# Assuming ..data/_data_in/6 centres-15-09-14.xls is the most current file
# Converted this to CSV by hand

# Todo
# ====


# Log
# ===
# 2014-10-09
# - file created
# - data dictionary converted to plain text
# - data sheet in ..data/_data_in/6 centres-15-09-14.xls saved as CSV file
# 2014-10-13
# - cloned from labbook_141013.R
# 2014-11-12
# - added the following fields
# 	- betablockade in 1st 24h
# 	- steroids in 1st 24h
# 	- fluids in in 1st 24h
# 	- fb in 1st 24h
# 	- cumulative fluids in
# 	- cumulative fb

library(Hmisc)
library(assertthat)
library(gmodels)
library(data.table)

# Assuming ..data/_data_in/6 centres-15-09-14.xls is the most current file
warning("Check:\nAssuming ..data/_data_in/6 centres-15-09-14.xls is the most current file")
raw_csv_file = '../data/working_140915.csv'
rdf <- read.table(raw_csv_file,
	sep = ",", quote="\"", header=TRUE,
	strip.white=TRUE, stringsAsFactors=FALSE )
str(rdf)

# Drop if missing id (there's a blank row at the end of the sheet)
tail(data.frame(rdf$id, rdf$age))
nrow(rdf)
rdf <- rdf[rdf$id != "",]
nrow(rdf)
assert_that(nrow(rdf) == 596)
rdt <- data.table(rdf)

# Sanity checks
df.names <- data.frame(names(rdf))
length(df.names[,])
df.names

with(rdf, head(data.frame(num, id, age, gender)))

# Prepare working data frame
# Only add variables as you need them

# Check that ID is unique
assert_that(sum(duplicated(rdf$id)) == 0)

# Prepare empty data frame
rm(wdt)
wdt <- data.table(id.original = rdf$id)
setkey(rdt, id)
setkey(wdt, id.original)

# Use date for ordering and then create a unique sequence
describe(rdf$icu_adm) # can't use missing 100 vals
describe(rdf$ne_start)
wdt <- wdt[rdt[,.(ne_start),keyby=id]]
wdt[,ne.start.dt := as.Date(ne_start, '%d/%m/%y')]
wdt[,ne_start    := NULL]

# TODO: 2014-10-13 - [ ] check for ne_start fr 2008? @roberta-sara
describe(wdt$ne.start.dt)

# Now start adding variables
wdt[,hosp := factor(tolower(gsub("([a-z]?)-[0-9]+","\\1", id.original)))]
str(wdt)
describe(wdt$hosp)
assert_that(
	nrow(wdt)
	- sum(wdt$hosp %in% c('an', 'rome', 'blf', 'lee', 'rlh', 'uclh'))
	== 0
	)

# Now make your own ordered unique ID (overall)
setorder(wdt, hosp, ne.start.dt)
wdt$id <- c(1:nrow(wdt))
head(wdt)
describe(wdt$id)

# Now make your own ordered unique ID (by hosp)
wdt[, id.hosp := c(1:nrow(.SD)), by=hosp]
describe(wdt$id.hosp)

# Patient characteristics
str(wdt)
setkey(wdt, id.original)
wdt <- wdt[rdt[,.(gender),keyby=id]]
wdt[,male 	:= ifelse(rdf$gender   == 1, 1, 0)]
wdt[,gender := NULL]
describe(wdt$male)

wdt <- wdt[rdt[,.(age),keyby=id]]
describe(wdt$age)
stem(wdt$age)

describe(rdf$height)
wdt <- wdt[rdt[,.(height),keyby=id]]
stem(wdt$height)

describe(rdf$weight)
wdt <- wdt[rdt[,.(weight),keyby=id]]
stem(wdt$weight)

describe(rdf$bmi)
wdt <- wdt[rdt[,.(bmi),keyby=id]]
stem(wdt$bmi)

# Sepsis
describe(rdf$source_infection)
wdt <- wdt[rdt[,.(source_infection),keyby=id]]
setnames(wdt,'source_infection','sepsis.site')

# TODO: 2014-10-13 - [ ] decide which bugs you want reported (will need cleaning) @roberta-sara
describe(rdf$bug1)
describe(rdf$bug2)

# Medications and interventions
describe(rdt$b_block_history)
wdt <- wdt[rdt[,.(b_block_history),keyby=id]]
wdt[,pmh.betablock := factor(b_block_history, labels=c(FALSE, TRUE))]
wdt[,b_block_history  := NULL]
str(wdt)

wdt <- wdt[rdt[,.(b_block_1to24),keyby=id]]
wdt[,rx.betablock := factor(b_block_1to24, labels=c(FALSE, TRUE))]
wdt[,b_block_1to24  := NULL]

wdt <- wdt[rdt[,.(steroids_1to24),keyby=id]]
wdt[,rx.roids := factor(steroids_1to24, labels=c(FALSE, TRUE))]
wdt[,steroids_1to24  := NULL]

# TODO: 2014-11-12 - [ ] correct fin.24 outliers at 30, 44, 9 @roberta-sara
wdt <- wdt[rdt[,.(tot_in_1),keyby=id]]
setnames(wdt,'tot_in_1','fin.24')
describe(wdt$fin.24)
stem(wdt$fin.24)

# TODO: 2014-11-12 - [ ] correct fin.cum outliers at 12.2 -- 180 ? or correct for LOS
wdt <- wdt[rdt[,.(tot_in_cumul),keyby=id]]
setnames(wdt,'tot_in_cumul','fin.cum')
describe(wdt$fin.cum)
stem(wdt$fin.cum)

# NOTE: 2014-11-12 - [ ] looks OK
wdt <- wdt[rdt[,.(fb_1),keyby=id]]
setnames(wdt,'fb_1','fb.24')
describe(wdt$fb.24)
stem(wdt$fb.24)

# NOTE: 2014-11-12 - [ ] looks OK
wdt <- wdt[rdt[,.(fb_cumul),keyby=id]]
setnames(wdt,'fb_cumul','fb.cum')
describe(wdt$fb.cum)
stem(wdt$fb.cum)


# Severity
describe(rdf$adm_sofa)
wdt <- wdt[rdt[,.(adm_sofa),keyby=id]]
setnames(wdt,'adm_sofa','sofa.0')
describe(rdf$sofa_1)
wdt <- wdt[rdt[,.(sofa_1),keyby=id]]
setnames(wdt,'sofa_1','sofa.1')


# NOTE: 2014-10-13 - error in naming sofa_24 column name
try(setnames(rdt,'sofa._24','sofa_24'), silent=FALSE)
wdt <- wdt[rdt[,.(sofa_24),keyby=id]]
setnames(wdt,'sofa_24','sofa.24')

# Norad, heart rate etc
obs <- list(
	c('ne', 'ne'),
	c('hr', 'hr'),
	c('map', 'map'),
	c('syst_bp', 'bps'),
	c('sed_score', 'sedation')
	)

for (i in 1:length(obs)) {

	suffixes <- c(1, 24)
	for (j in 1:length(suffixes)) {
		suffix <- suffixes[j]
		ob_orig <- paste(obs[[i]][1], suffix, sep='_')
		ob_new <- paste(obs[[i]][2], suffix, sep='.')

		wdt <- wdt[rdt[,.(get(ob_orig)),keyby=id]]
		setnames(wdt,'V1',ob_new)
	}

}
str(wdt)

# Outcomes
wdt <- wdt[rdt[,.(itu_mortality),keyby=id]]
setnames(wdt,'itu_mortality','mort.itu')

# NOTE: 2014-10-09 - missing hospital mortality for 63 patients
wdt <- wdt[rdt[,.(hosp_mortality),keyby=id]]
setnames(wdt,'hosp_mortality','mort.hosp')
wdt[,list(.N,mort.hosp.miss = sum(is.na(mort.hosp)) ),hosp]

head(wdt)
str(wdt)

# Length of stay
str(rdt)
wdt <- wdt[rdt[,.(itu_los),keyby=id]]
setnames(wdt,'itu_los','los.itu')
# TODO: 2014-11-23 - [ ] converting to numeric; check this is OK
table(wdt$los.itu)
wdt[,los.itu := ifelse(los.itu=="<1", 0, as.numeric(los.itu))]
describe(wdt$los.itu)

# Save
save(rdf, wdt, file='../data/working.RData')

