# author: Steve Harris
# date: 2014-10-09
# subject: Produce Table 1 for the norad study

# Readme
# ======


# Todo
# ====

# Notes
#

# Log
# ===
# 2014-10-09
# - file created
# 2014-10-10
# - works, produces a single data table with all the vars
# 2014-10-11
# - switch to using development version of data.table
# 2014-10-11
# - seems to be working
# - handles strata
# - exports complete and formatted data to excel
# 2014-10-12
# - rewrite of t1.catvars function which was completely wrong!
# - fixed table ordering
# 2014-10-13
# - duplicated from paper-spotearly/labbook_table1.R
# 2014-11-12
# - added columns and data as per request
# 2014-11-29
# - adding further vars

rm(list=ls(all=TRUE))

"usage: 
    table1 [options]

options:
    --help              help  (print this)
    -d, --describe      describe this model
    --subgrp=SUBGRP     All patients or subgrp [default: all]" -> doc

require(docopt) # load the docopt library to parse
# opts <- docopt(doc, "--subgrp=icu_recommend --nsims=5") # for debugging
opts <- docopt(doc)
if (opts$d) {
    write("
********************************************************
Generate a standardised Table 1 with options by subgroup
********************************************************
Subgroups 
- all
- ne24
- morelli
", stdout())
    quit()
}

library(Hmisc)
library(data.table) # NB setnames and setorder only exist in >v1.9
library(reshape2)
library(XLConnect)
require(assertthat)
require(gmodels)

subgrp <- opts$subgrp         # define subgrp
# Load data and prepare vars
load("../data/strobe.Rdata")

if (subgrp=="all") {
	wdt$sample_N <- 1
    assert_that(nrow(wdt)==691)
    grp_suffix <- "_all"
} else if (subgrp=="ne24") {
	wdt$sample_N <- 1
    wdt <- wdt[get(paste0("grp.", opts$subgrp))==1]
    assert_that(nrow(wdt)==567)
    grp_suffix <- "_ne24"
} else if (subgrp=="morelli") {
	wdt$sample_N <- 1
    wdt <- wdt[get(paste0("grp.", opts$subgrp))==1]
    assert_that(nrow(wdt)==270)
    grp_suffix <- "_morelli"
} else {
    stop(paste("ERROR?:", subgrp, "not one of 'all', 'grp.ne24', or 'grp.morelli'"))
}

# Define file name
table1.data <- paste0( '../write/tables/table1', grp_suffix, '.RData')
table1.file <- paste0( '../write/tables/table1', grp_suffix, '.xlsx')

print(table1.file)
print(nrow(wdt))

# Define strata
# NOTE: 2014-10-13 - analyse all
vars.strata <-  NA

# Define the vars

vars <- c(
	'hosp.id',
	'male',
	'age',
	'weight',
	'bmi',
	# 'height',
	'itu.shock',
	'sepsis.site',
	# 'pmh.betablock', 
	'hr.1',
	'non.sinus.1',
	'map.1',
	'bps.1',
	'bpd.1',
	'co.1',
	'lac.1',
	'mv.24',
	'pf.24',
	'peep.24',
	'rrt.24',
	'ne.24',
	'pressor.other.24',
	'vadi.24',
	'rx.roids',
	'sedation.24',
	'sofa.1',
	# 'rx.betablock',
	'fin.24', 'fb.24',
	'fb.mean',
	'los.ne',
	'mort.itu'
	# 'mort.hosp' # dropped because less than unit mortality!?
	)

vars <- c('sample_N', vars) # prepend all obs for total counts

# If no strata defined then use sample_N dummy variable
if (is.na(vars.strata)) {
	vars.strata <- 'sample_N'
}

# Define the characteristics of the variables
vars.factor <- c(
	'hosp.id',
	'male',
	'sepsis.site',
	# 'pmh.betablock',
	# 'rx.betablock',
	'itu.shock',
	'non.sinus.24',
	'mv.24',
	'pressor.other.24',
	'rx.roids',
	'rrt.24',
	'mort.itu'
	# 'mort.hosp'
	)

# Define distributions
vars.norm 	<- c('age', 'weight', 'hr.24', 'map.24')

# NOTE: 2014-10-12 - you need to have the strata var in the data.table
if (!vars.strata %in% vars) {
	vars <- c(vars, vars.strata)
	vars.factor <- c(vars.factor, vars.strata)
}
vars.factor <- c('sample_N', vars.factor) # prepend all obs for total counts
vars.cont 	<- vars[!vars %in% vars.factor]
str(wdt[,vars.cont,with=FALSE])


# Now update columns to be factors if not already
wdt[, (vars.factor) :=lapply(.SD, as.factor), .SDcols=vars.factor]

# Remove the stratifying variable from the factor vars
vars.factor <- vars.factor[!vars.factor %in% vars.strata]

# Now produce the summaries for the continuous vars
# NOTE: 2014-10-11 - lapply to sapply since then returns vectors not lists

t1.contvars <- function(var, strata, this_dt) {
	this_dt[,
		list(
				varname	 = var,
				N 		 = .N,
				# NOTE: 2014-10-11 - add these later else strata means you get missing by strata not var
				miss.n	 = sapply(.SD, function(x) sum(is.na(x))),
				# miss.p	 = sapply(.SD, function(x) round(sum(is.na(x)) / length(x) * 100, 1)),
				mean	 = sapply(.SD, function(x) mean(x, na.rm=TRUE)),
			 	sd		 = sapply(.SD, function(x) sd(x, na.rm=TRUE)),
			 	min		 = sapply(.SD, function(x) min(x, na.rm=TRUE)),
			 	q05		 = sapply(.SD, function(x) quantile(x, 0.05, na.rm=TRUE)),
			 	q25		 = sapply(.SD, function(x) quantile(x, 0.25, na.rm=TRUE)),
			 	q50		 = sapply(.SD, function(x) quantile(x, 0.50, na.rm=TRUE)),
			 	q75		 = sapply(.SD, function(x) quantile(x, 0.75, na.rm=TRUE)),
			 	q95		 = sapply(.SD, function(x) quantile(x, 0.95, na.rm=TRUE)),
			 	max		 = sapply(.SD, function(x) max(x, na.rm=TRUE))
			 	),
			by=strata,
			.SDcols = var
	]
}
# t1.contvars(c('lactate', 'bpsys'), c('room_cmp', 'sex'), wdt)
# t1.contvars('sofa.1', vars.strata, wdt)

# Now do this for all the continuous vars
t1.contvars.results <- t1.contvars(vars.cont, vars.strata, wdt)

# Now total the missing by variable
t1.contvars.results[,miss.n := sum(miss.n), by=varname]
t1.contvars.results[, miss.p := lapply(.SD, function(x) round(max(miss.n)/sum(N) *100, 1)), by=varname ]

# Now convert this list of data.tables into a single data.table
t1.contvars.results$vartype <- 'continuous'
t1.contvars.results

# Categorical vars

t1.catvars <- function(var_as_string, strata_as_string, this_dt) {

	strata 		<- this_dt[[strata_as_string]]
	var 		<- this_dt[[var_as_string]]

	var <- with(this_dt, get(var_as_string))
	t2 <- this_dt[,.(strata.rows=.N),by=strata]

	t1 <- this_dt[, .N ,by=list(var, strata)]
	setkey(t1, strata)
	setkey(t2, strata)
	t1 <- t1[t2]

	t1[,`:=` (
		pct 	= round(N/strata.rows*100, 1),
		varname = var_as_string,
		vartype = 'categorical',
		strata.rows = NULL
		)]
	setnames(t1, 'var', 'level')
	setnames(t1, 'strata', strata_as_string)
	return(t1)
}
# t1.catvars('dead28','early4', wdt)

t1.catvars.results <- lapply(vars.factor, function(var) t1.catvars(var, vars.strata, wdt))
t1.catvars.results <- do.call(rbind, t1.catvars.results)
t1.catvars.results

t1.catvars.results[, miss.n:=NULL, ]
t1.catvars.results[is.na(level), miss.n := lapply(.SD, function(x) sum(N)), by=varname]
t1.catvars.results[, miss.n := ifelse(is.na(miss.n), 0, miss.n) ]
t1.catvars.results[, max(miss.n), by=varname ]
t1.catvars.results[, miss.n := lapply(.SD, function(x) max(miss.n)), by=varname ]
t1.catvars.results[, miss.p := lapply(.SD, function(x) round(max(miss.n)/sum(N) *100, 1)), by=varname ]

t1.contvars.results.orig <- t1.contvars.results
t1.contvars.results <- lapply(t1.contvars.results, function(x) if (is.list(x)) as.numeric(x) else x)
t1.contvars.results <- as.data.table(t1.contvars.results)

# Now merge these data.frames together
cols_shared <- c(c('varname', 'vartype', 'N', 'miss.n', 'miss.p'), vars.strata)
t1.results <- merge(t1.contvars.results, t1.catvars.results,
	by=cols_shared, all=TRUE)
setcolorder(t1.results, c(vars.strata, 'varname', 'level', 'N', 'pct', 'mean', 'sd', 'min', 'q05', 'q25', 'q50', 'q75', 'q95', 'max', 'miss.n', 'miss.p', 'vartype'))

# Provide the table order
t1.results[,table.order := NULL]
t1.results[, table.order := which(vars == varname), by=varname]

# Define the variables distribution
t1.results[, dist := ifelse(varname %in% vars.norm, 'normal', '')]

# setorder(t1.results, table.order, vars.strata)
cols_all_order <- c(vars.strata, c('varname', 'miss.p', 'level', 'N', 'pct', 'mean', 'sd'))
# t1.results[, .SD, .SDcols=cols_all_order]

# Drop the NA levels since these are captured in the miss.n and miss.p fields
t1.results <- t1.results[!(vartype=='categorical' & is.na(level))]
# Convert level from factor to numeric to enable sorting
# NOTE: 2014-11-13 - [ ] where TRUE and FALSE used as factors this step fails
# so now create a new variable level.order
as.numeric(t1.results$level)
t1.results[, level.order := as.numeric(level)]

# Extract variable level results
t1.results.byvarname <- t1.results[, list(
	miss.n = max(miss.n),
	miss.p = max(miss.p) )
	, by=varname ]

#  ====================
#  = Raw data to wide =
#  ====================
t1.melt <- melt(t1.results, id=c(vars.strata, c('varname', 'level')))
setnames(t1.melt, vars.strata, 'strata')

# Drop empty strata from the table
t1.wide.raw <- dcast.data.table(t1.melt, varname + level ~ strata + variable,
	subset = .(!is.na(strata)))

t1.wide.raw[, table.order := which(vars == varname), by=varname]
setorder(t1.wide.raw, table.order)
t1.wide.raw[is.na(level), level := '']

#  ==========================
#  = Formatted data to wide =
#  ==========================
# Pick your results for the final formatted table
# NOTE: 2014-10-11 - ifelse returns NA if you ask NA == 1 rather than the else clause
# str(t1.results)
t1.results.formatted <- t1.results[, list(
	strata      = get(vars.strata),
	varname     = varname,
	level       = level,
	level.order = level.order,
	v.mid       = ifelse(vartype == 'categorical', N, ifelse(dist== 'normal', mean, q50)),
	v.left      = ifelse(vartype == 'categorical', pct, ifelse(dist == 'normal', sd, q25)),
	v.right     = ifelse(vartype == 'categorical' | dist == 'normal', NA, q75),
	miss.n      = miss.n,
	miss.p      = miss.p,
	N           = N,
	vartype     = vartype,
	dist        = dist
	)]


# Now provide the formatting
t1.results.formatted <- t1.results.formatted[,
	`:=` (
	v.fmt1 	= ifelse(vartype == 'categorical', sprintf("%.0f", v.mid), sprintf("%.1f", v.mid)),
	v.fmt2 	= ifelse(vartype == 'categorical', paste('(', sprintf("%.1f", v.left), '%)', sep=''),
				ifelse(vartype == 'continuous' & dist == 'normal',
					paste('(', sprintf("%.1f", v.left), ')', sep=''),
					paste('(', sprintf("%.1f", v.left), '--', sprintf("%.1f", v.right), ')', sep='')))
		) ]
t1.results.formatted

# Now reshape to wide
t1.melt <- melt(
	t1.results.formatted[, list(strata, varname, level, level.order, v.fmt1, v.fmt2, N, miss.n, miss.p)],
	id=c('strata', 'varname', 'level', 'level.order'))
t1.melt

# Drop empty strata from the table
t1.wide.summ <- dcast.data.table(
	t1.melt[(variable == 'v.fmt1' | variable == 'v.fmt2')],
	varname + level + level.order ~ strata + variable,
	subset = .(!is.na(strata)))
t1.wide.summ
t1.wide.summ[, table.order := which(vars == varname), by=varname]

t1.wide.summ[is.na(level), level := '']

setkey(t1.wide.summ, varname)
setkey(t1.results.byvarname, varname)
t1.wide.summ <- t1.wide.summ[t1.results.byvarname]
setorder(t1.wide.summ, +table.order, -level.order)

t1.wide.summ

# write table
save(t1.wide.summ, t1.wide.raw, file=table1.data)

#  =======================
#  = Now export to excel =
#  =======================
wb <- loadWorkbook(table1.file, create = TRUE)

sheet1 <- paste('varsBy_', vars.strata, sep='')
removeSheet(wb, sheet1)
createSheet(wb, name = sheet1)
writeWorksheet(wb, t1.wide.summ, sheet1)

sheet2 <- paste('varsBy_', vars.strata, '_detail', sep='')
removeSheet(wb, sheet2)
createSheet(wb, name = sheet2)
writeWorksheet(wb, t1.wide.raw, sheet2)

saveWorkbook(wb)

