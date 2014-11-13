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

# Use date for ordering and then create a unique sequence
describe(rdf$icu_adm) # can't use missing 100 vals
describe(rdf$ne_start)
wdt$ne.start.dt <- as.Date(rdf$ne_start, '%d/%m/%y')
describe(wdt$ne.start.dt) # TODO: 2014-10-13 - [ ] odd dates fr 2008?

# Now start adding variables
wdt$hosp <- tolower(gsub("([a-z]?)-[0-9]+","\\1", wdt$id.original))
factor(wdt$hosp)
table(wdt$hosp)
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
wdt$male 	<- ifelse(rdf$gender   == 1, 1, 0)
describe(wdt$male)

wdt$age <- rdf$age
describe(wdt$age)

describe(rdf$height)
wdt$height <- rdf$height

describe(rdf$weight)
wdt$weight <- rdf$weight

describe(rdf$bmi)
wdt$bmi <- rdf$bmi

# Sepsis
describe(rdf$source_infection)
wdt$sepsis.site <- rdf$source_infection
factor(wdt$sepsis.site)
wdt[,list(.N, pct=.N/nrow(wdt)*100),sepsis.site]

# TODO: 2014-10-13 - [ ] decide which bugs you want reported (will need cleaning)
describe(rdf$bug1)
describe(rdf$bug2)

# Medications and interventions
describe(rdf$b_block_hist)
wdt$pmh.betablock <- factor(rdf$b_block_hist, labels=c(FALSE, TRUE))
describe(wdt$pmh.betablock)

str(rdf$b_block_1to24)
wdt$rx.betablock <- factor(rdf$b_block_1to24, labels=c(FALSE, TRUE))
describe(wdt$rx.betablock)

wdt$rx.roids <- factor(rdf$steroids_1to24, labels=c(FALSE, TRUE))
describe(wdt$rx.roids)

# TODO: 2014-11-12 - [ ] correct fin.24 outliers at 30, 44, 9
wdt$fin.24 <- rdf$tot_in_1
describe(wdt$fin.24)
stem(wdt$fin.24)

# TODO: 2014-11-12 - [ ] correct fin.cum outliers at 12.2 -- 180 ? or correct for LOS
wdt$fin.cum <- rdf$tot_in_cumul
describe(wdt$fin.cum)
stem(wdt$fin.cum)

# NOTE: 2014-11-12 - [ ] looks OK
wdt$fb.24 <- rdf$fb_1
describe(wdt$fb.24)
stem(wdt$fb.24)

# NOTE: 2014-11-12 - [ ] looks OK
wdt$fb.cum <- rdf$fb_cumul
describe(wdt$fb.cum)
stem(wdt$fb.cum)


# Severity
describe(rdf$adm_sofa)
wdt$sofa.0 <- rdf$adm_sofa
describe(rdf$sofa_1)
wdt$sofa.1 <- rdf$sofa_1

# NOTE: 2014-10-13 - error in naming sofa_24 column name
try(names(rdf)[names(rdf)=='sofa._24'] <- 'sofa_24', silent=FALSE)
describe(rdf$sofa_24)
wdt$sofa.24 <- rdf$sofa_24

# Norad, heart rate etc
obs <- list(
	c('ne', 'ne'),
	c('hr', 'hr'),
	c('map', 'map'),
	c('syst_bp', 'bps'),
	c('sed_score', 'sedation')
	)

# names(rdf)
# str(wdt)

# TODO: 2014-10-13 - [ ] nested loop: fix
for (i in 1:length(obs)) {

	suffixes <- c(1, 24)
	for (j in 1:length(suffixes)) {
		suffix <- suffixes[j]
		# print(describe(rdf[paste(obs[[i]][1], '1', sep='_')]))
		print(paste(obs[[i]][1], j, sep='_'))
		ob_orig <- paste(obs[[i]][1], suffix, sep='_')
		print(str(rdf[ob_orig]))

		ob_new <- paste(obs[[i]][2], suffix, sep='.')
		wdt[,ob_new] <- rdf[ob_orig]
		print(str(wdt[,ob_new]))
	}

}
# str(wdt)
describe(wdt$sedation.1)
describe(wdt$sedation.24)


# Outcomes
describe(rdf$itu_mortality)
wdt$mort.itu <- rdf$itu_mortality

# NOTE: 2014-10-09 - missing hospital mortality for 63 patients
describe(rdf$hosp_mortality)
wdt$mort.hosp <- rdf$hosp_mortality
wdt[,list(.N,mort.hosp.miss = sum(is.na(mort.hosp)) ),hosp]

head(wdt)
str(wdt)
str(rdf)

# Save
save(rdf, wdt, file='../data/working.RData')


