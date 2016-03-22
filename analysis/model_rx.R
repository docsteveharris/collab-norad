# author: Steve Harris
# date: 2016-03-11
# subject: model noradrenaline dose at 24 h

# Readme
# ======


# Todo
# ====
# - [ ] TODO(2016-03-18): add in VADI when you have the raw data


# Log
# ===
# 2016-03-11
# - file created
# 2016-03-18
# - adapted to become generic for each treatment variables

rm(list=ls(all=TRUE))

"usage: 
    model_rx [options]

options:
    --help              help  (print this)
    -d, --describe      describe this model
    --rx=TREATMENT      specify treatment [default: ne.24]" -> doc

require(docopt) # load the docopt library to parse
# opts <- docopt(doc, "--subgrp=icu_recommend --nsims=5") # for debugging
opts <- docopt(doc)
if (opts$d) {
    write("
*************************
Model treatment variables
*************************
Potential variables
- ne.24
- fb.24
- map.24
", stdout())
    quit()
}

rx <- opts$rx         # define rx variable of interest

library(Hmisc)
library(data.table)
library(dsbc)
library(arm)
library(ggplot2)
library(reshape2)
library(assertthat)
library(XLConnect)

# rx <- "ne.24" # for debugging
load("../data/strobe.Rdata")
source(file="../share/functions4paper.R")

assert_that(nrow(wdt)==691)


if (rx=="ne.24") {
	# 	+ noradrenaline dose (weight adjusted) at 24h
	wdt[, "rx" := get(rx), with=FALSE]
	wdt[, rx := log(rx)] # log transform for ne
	# Drop missing and zero values
	tdt <- wdt[!(rx %in% c(NA, -Inf, +Inf, NaN))]
    rx_suffix <- "_ne.24"
	vars.other <- c("rescale(map.24)", "rescale(fb.24)")
	m.labels.specific <- c(
		"MAP (at 24 hours)",
		"Fluid balance (at 24 hours)"
		)
} else if (rx=="map.24") {
	# 	+ mean arterial pressure at 24h (treatment variable)
	wdt[, "rx" := get(rx), with=FALSE]
	# Drop missing and zero values
	tdt <- wdt[!(rx %in% c(NA, -Inf, +Inf, NaN))]
    rx_suffix <- "_map.24"
	vars.other <- c("rescale(ne.24)", "rescale(fb.24)")
	m.labels.specific <- c(
		"Noradrenaline (at 24 hours)",
		"Fluid balance (at 24 hours)"
		)
} else if (rx=="fb.24") {
	# 	+ fluid balance in 1st 24h
	wdt[, "rx" := get(rx), with=FALSE]
	wdt[, rx := (rx/1000)] # transform to litres
	# Drop missing and zero values
	tdt <- wdt[!(rx %in% c(NA, -Inf, +Inf, NaN))]
    rx_suffix <- "_fb.24"
	vars.other <- c("rescale(ne.24)", "rescale(map.24)")
	m.labels.specific <- c(
		"Noradrenaline (at 24 hours)",
		"MAP (at 24 hours)"
		)
} else {
    stop(paste("ERROR?:", rx, "not one of 'ne.24' or ''"))
}

file.table <- paste0( '../write/tables/model_rx', rx_suffix, '.xlsx')
file.coefplot <- paste0("../write/figures/model_rx", rx_suffix, ".eps")

m.labels <- c(
	"Age",
	"Male",
	"Weight",
	"Abdominal sepsis",
	"Genito-urinary sepsis",
	"Respiratory sepsis",
	"SOFA (baseline)",
	"Lactate (baseline)",
	"Mechanical ventilation",
	"Renal replacement therapy",
	"Sedation score",
	"Steroid treatment", m.labels.specific)

# Describe the dependent variable
describe(tdt$rx)

# - define patient level predictors
# 	+ age
# 	+ sex
# 	+ weight
# 	+ sepsis site
# 	+ initial SOFA score (at hour 1)
vars.pt <- c("rescale(age)", "male", "rescale(weight)", "sepsis.site", "rescale(sofa.1)", "rescale(lac.1)")

# - define treatment variables for adjustment
# 	+ mechanical ventilation
# 	+ renal replacement therapy
# 	+ other vasopressors (yes/no)
# 	+ other inotropes (yes/no) ? or just combine with above and calculate by subtracting from VADI
# 	+ sedatation dose
# vars.rx <- c("mv.24", "rrt.24", "rescale(vadi.24)", "rescale(sedation.24)", "rx.roids")
warning("**MISSING VADI**")
vars.rx <- c("mv.24", "rrt.24", "rescale(sedation.24)", "rx.roids")

f <- formula(paste("rx ~ ", paste(c(vars.pt, vars.rx, vars.other), collapse="+"), "+ (1 | hosp.id)"))
print(f)

m <- lmer(f, data=tdt)
display(m)
# Prepare table to save
m.table <- data.table(summary(m)$coefficients, keep.rownames=TRUE)
colnames(m.table)[2] <- "est"
colnames(m.table)[3] <- "se"
colnames(m.table)[4] <- "t"
m.table$z <- m.table$est / m.table$se
m.table$p <- pnorm(abs(m.table$z), lower.tail=FALSE)

m.table <- cbind(label=c("Intercept", m.labels), m.table)
m.table

p <- coef.plot(m) 
p <- coef.plot(m, m.labels=m.labels)
print(p)
ggsave(p, file=file.coefplot, width=3, height=3, scale=2)

(m.icc <- ICC(m))
(model_parameters <- list(data.frame(names(m.icc), m.icc)))


# Now write to Excel
wb <- loadWorkbook(file.table, create = TRUE)

sheet1 <- paste0("model", rx_suffix)
# if (existsSheet(wb, sheet1)) {
# 	removeSheet(wb, sheet1)
# }
createSheet(wb, name = sheet1)
writeWorksheet(wb, model_parameters, sheet1)

sheet2 <- paste0("raw", rx_suffix)
# if (existsSheet(wb, sheet2)) {
# 	removeSheet(wb, sheet2)
# }
createSheet(wb, name = sheet2)
writeWorksheet(wb, m.table, sheet2)

saveWorkbook(wb)
