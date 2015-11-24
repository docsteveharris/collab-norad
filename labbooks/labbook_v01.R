# author: Steve Harris
# date: 2014-10-09
# subject: Have a look and start working with the data

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

library(Hmisc)
library(assertthat)
library(gmodels)
library(data.table)

raw_csv_file = '../data/working_140915.csv'
rdf <- read.csv(raw_csv_file, strip.white=TRUE)
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
wdt <- data.table(id.original = rdf$id)
wdt$id <- c(1:nrow(wdt))
head(wdt)


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

describe(rdf$source_infection)
wdt$sepsis.site <- rdf$source_infection
factor(wdt$sepsis.site)
wdt[,list(.N, pct=.N/nrow(wdt)*100),sepsis.site]

# Outcomes
describe(rdf$itu_mortality)
wdt$mort.itu <- rdf$itu_mortality

# NOTE: 2014-10-09 - missing hospital mortality for 63 patients
describe(rdf$hosp_mortality)
wdt$mort.hosp <- rdf$hosp_mortality
wdt[,list(.N,mort.hosp.miss = sum(is.na(mort.hosp)) ),hosp]

head(wdt)


