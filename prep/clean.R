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
# 2015-04-08
# - switch to using updated file with 8 ICU's

#  ====================================================
#  = Load raw data and all dependencies and functions =
#  ====================================================
source(file="../prep/load.R")

# Drop if missing id (there's a blank row at the end of the sheet)
tail(data.frame(rdf$id, rdf$age))
nrow(rdf)
rdf <- rdf[rdf$id != "",]
nrow(rdf)
assert_that(nrow(rdf) == 736)
rdt <- data.table(rdf)
rdt

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
describe(rdf$ne_start)
wdt <- wdt[rdt[,.(ne_start),keyby=id]]
wdt[,ne.start.dt := as.Date(ne_start, '%d/%m/%Y')]
wdt[,ne_start    := NULL]

# ICU admission
describe(rdf$icu_adm) # can't use missing 100 vals
wdt <- wdt[rdt[,.(icu_adm),keyby=id]]
wdt[,icu.adm.dt := as.Date(icu_adm, '%d/%m/%Y')]
wdt[,icu_adm    := NULL]

# Work out length of stay pre-shock and post-shock
describe(rdt$post_itu_los)
wdt <- wdt[rdt[,.(los.post.itu=post_itu_los),keyby=id]]
wdt[, los.post.itu:=as.integer(los.post.itu)]

describe(rdt$pre_itu_los)
wdt <- wdt[rdt[,.(los.pre.itu=pre_itu_los),keyby=id]]
wdt[, los.pre.itu:=as.integer(los.pre.itu)]

# TODO: 2014-10-13 - [ ] check for ne_start fr 2008? @roberta-sara
describe(wdt$ne.start.dt)

# Now start adding variables
wdt[,hosp := factor(tolower(gsub("([a-z]?)-[0-9]+","\\1", id.original)))]
str(wdt)
describe(wdt$hosp)
assert_that(
	nrow(wdt)
	- sum(wdt$hosp %in% c('an', 'rome', 'blf', 'lee', 'rlh', 'uclh', 'sth', 'paris'))
	== 0
	)

# Name the hospitals
table(wdt$hosp)
wdt[, hosp := factor(hosp,
	levels=c("an", "blf", "lee", "paris", "rlh", "rome", "sth", "uclh"),
	labels=c("Ancona", "Belfast", "Leuven", "Paris", "Royal London", "Rome", "GSTT", "UCLH"))]


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
# NOTE: 2015-11-24 - [ ] manually correct heights for RLH 
wdt[, height := ifelse(hosp=="rlh",height*100,height)]
stem(wdt$height)

describe(rdf$weight)
wdt <- wdt[rdt[,.(weight),keyby=id]]
stem(wdt$weight)

# NOTE: 2015-04-08 - [ ] missing from 8 ICU data?
# describe(rdf$bmi)
# wdt <- wdt[rdt[,.(bmi),keyby=id]]
# stem(wdt$bmi)

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
describe(wdt$b_block_history)
wdt[, b_block_history := as.numeric(ifelse(b_block_history=="yes",1,b_block_history))]
wdt[,pmh.betablock := factor(b_block_history, labels=c(FALSE, TRUE))]
wdt[,b_block_history  := NULL]
describe(wdt$pmh.betablock)
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

# NOTE: 2015-04-08 - [ ] missing from 8 ICU data
# Total cumulative fluid balance
# 1--4 days monitored
# we have 24 patients where fluid balance is reported over zero days - should replace with old def
describe(rdt$n_days_fb)
describe(rdt$fb_days)
head(rdt[n_days_fb!=fb_days,.(id, n_days_fb, fb_days)])
head(rdt[fb_days==0,.(id, n_days_fb, fb_days)])

wdt[,n_days_fb:=NULL]
wdt[,fb_days:=NULL]
wdt[,fb.days:=NULL]
wdt <- wdt[rdt[,.(n_days_fb),keyby=id]]
wdt <- wdt[rdt[,.(fb_days),keyby=id]]
wdt[,fb.days:=as.integer(max(n_days_fb,fb_days,na.rm=TRUE)),by=id]
head(wdt[n_days_fb!=fb_days,.(id, fb.days, n_days_fb, fb_days)],100)
wdt[,fb_days:=ifelse(fb_days==0,NA,fb_days)]
describe(wdt$fb.days)


wdt[,fb.mean := fb.cum / fb.days]
stem(wdt$fb.mean)

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

# NOTE: 2014-11-29 - [ ] pao2 doesn't end in _1 or _24
setnames(rdt, "pao2_1kpa", "pao2.kpa_1")
setnames(rdt, "pao2_24kpa", "pao2.kpa_24")

# Norad, heart rate etc
obs <- list(
	c('ne', 'ne'),
	c('other_vas', 'pressor.other'),
	c('vadi', 'vadi'),
	c('hr', 'hr'),
	c('rhythm', 'non.sinus'),
	c('map', 'map'),
	c('syst_bp', 'bps'),
	c('diast_bp', 'bpd'),
	c('co', 'co'),
	c('lactate', 'lac'),
	c('pao2.kpa', 'pao2'),
	c('fio2', 'fio2'),
	c('peep', 'peep'),
	c('mv', 'mv'),
	c('sed_score', 'sedation'),
	c('rrt', 'rrt')
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

wdt[, peep.1 := round(as.numeric(peep.1))]
wdt[, peep.24 := round(as.numeric(peep.24))]

# Checks
# ------
# These seem OK
lapply(wdt[,.(lac.1, lac.24)], check.cont)
lapply(wdt[,.(fio2.1, fio2.24)], check.cont)
lapply(wdt[,.(pao2.1, pao2.24)], check.cont)

# Now generate P:F ratios
wdt[, pf.1 := pao2.1 / fio2.1]
wdt[, pf.24 := pao2.24 / fio2.24]
lapply(wdt[,.(pf.1, pf.24)], check.cont)

# RRT use
lapply(wdt[,.(rrt.1, rrt.24)], check.cat)

# Outcomes
wdt <- wdt[rdt[,.(itu_mortality),keyby=id]]
setnames(wdt,'itu_mortality','mort.itu')
# wdt[, mort.itu := factor(mort.itu, levels=c(0,1), labels=c("Alive", "Dead"))]

# NOTE: 2015-11-24 - missing hospital mortality for 2 patients (lee)
wdt <- wdt[rdt[,.(hosp_mortality),keyby=id]]
setnames(wdt,'hosp_mortality','mort.hosp')
# wdt[, mort.hosp := factor(mort.hosp, levels=c(0,1), labels=c("Alive", "Dead"))]
wdt[,list(.N,mort.hosp.miss = sum(is.na(mort.hosp)) ),hosp]

# Length of stay
wdt <- wdt[rdt[,.(itu_los),keyby=id]]
setnames(wdt,'itu_los','los.itu')
# TODO: 2014-11-23 - [ ] converting to numeric; check this is OK
table(wdt$los.itu)
wdt[,los.itu := ifelse(los.itu=="<1", 0, as.numeric(los.itu))]
describe(wdt$los.itu)
str(wdt$los.itu)

# ITU discharge
wdt[, .(icu.adm.dt,los.itu,itu.dc.dt=icu.adm.dt+los.itu)]
wdt[, itu.dc.dt := icu.adm.dt+los.itu]

# ITU los post norad
wdt[, .(icu.adm.dt,los.itu,itu.dc.dt, los.ne = itu.dc.dt-ne.start.dt)]
wdt[, los.ne := as.integer(itu.dc.dt-ne.start.dt)]
# Replace with missing if non-sensical
wdt[, los.ne := ifelse(los.ne < 0, NA, los.ne)]
describe(wdt$los.ne)

describe(wdt$ne.start.dt)
wdt[, .(icu.adm.dt,los.itu,itu.dc.dt, ne.start.dt, los.ne, los.post.itu)]


# Unit acquired shock (upgrade to be within 48h of admission)
describe(rdt$ne_less24h)
wdt[, los.ne.pre := as.integer(ne.start.dt-icu.adm.dt)]
wdt[, los.ne.pre := ifelse(los.ne.pre < 0 | los.ne.pre > 60 , NA, los.ne.pre)]
describe(wdt$los.ne.pre)

rdt[,itu.shock := ifelse(ne_less24h==0, 1, 0)]
wdt <- wdt[rdt[,.(itu.shock),keyby=id]]
describe(wdt$itu.shock)
with(wdt, table(los.ne.pre, itu.shock, useNA="always"))
wdt[, itu.shock := ifelse(!is.na(los.ne.pre) & los.ne.pre > 2, 1, itu.shock)]
describe(wdt$itu.shock)



# Generate a hospital level variable sorted by mortality
m <- wdt[, .(s = mean(mort.itu, na.rm=TRUE)), by=hosp]
setorder(m, s)
m[, mort.itu.order := .I ]
m[, mort.itu.order := factor(mort.itu.order, labels=m$hosp, ordered=TRUE)]
setkey(wdt,hosp)
wdt <- wdt[m[,.(hosp,mort.itu.order)]]

# Save
save(wdt, file='../data/cleaned.Rdata')

