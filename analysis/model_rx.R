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
# 2016-04-22
# - drop 24 hour variables from model since these are hard to interpret in terms of predictors

rm(list=ls(all=TRUE))

"usage: 
    model_rx [options]

options:
    --help              help  (print this)
    -d, --describe      describe this model
    --rx=TREATMENT      specify treatment [default: ne.24]
    --coeflim=COEFLIM  specify coef plot scale[default: 1]" -> doc

require(docopt) # load the docopt library to parse
opts <- docopt(doc, "--rx=ne.24") # for debugging
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
coef.lim <- as.numeric(opts$coeflim)
coef.lim <- c(-1*coef.lim, coef.lim)

require(Hmisc)
require(data.table)
require(dsbc)
require(arm)
require(ggplot2)
require(reshape2)
require(assertthat)
require(XLConnect)

# rx <- "ne.24" # for debugging
load("../data/strobe.Rdata")
source(file="../share/functions4paper.R")

assert_that(nrow(wdt)==691)


if (rx=="ne.24") {
    #   + noradrenaline dose (weight adjusted) at 24h
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
    #   + mean arterial pressure at 24h (treatment variable)
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
    #   + fluid balance in 1st 24h
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
    "Additional vasopressors",
    "Steroid treatment"
    # , m.labels.specific
    )

# Describe the dependent variable
describe(tdt$rx)

# - define patient level predictors
#   + age
#   + sex
#   + weight
#   + sepsis site
#   + initial SOFA score (at hour 1)
vars.pt <- c("rescale(age)", "male", "rescale(weight)", "sepsis.site", "rescale(sofa.1)", "rescale(lac.1)")

# - define treatment variables for adjustment
#   + mechanical ventilation
#   + renal replacement therapy
#   + other vasopressors (yes/no)
#   + other inotropes (yes/no) ? or just combine with above and calculate by subtracting from VADI
#   + sedatation dose
# vars.rx <- c("mv.24", "rrt.24", "rescale(vadi.24)", "rescale(sedation.24)", "rx.roids")
vars.rx <- c("mv.24", "rrt.24", "rescale(sedation.24)", "vadi.other.24.logical", "rx.roids")

# Define formula
# Null formula - centre effect only
f <- formula(paste("rx ~ (1 | hosp.id)"))
# Patient level predictors
f <- formula(paste("rx ~ ", paste(c(vars.pt), collapse="+"), "+ (1 | hosp.id)"))
# Patient and treatment level predictors
# f <- formula(paste("rx ~ ", paste(c(vars.pt, vars.rx, vars.other), collapse="+"), "+ (1 | hosp.id)"))
# Drop vars other since these are 24 hour variables - not sure it makes sense to adjust
f <- formula(paste("rx ~ ", paste(c(vars.pt, vars.rx), collapse="+"), "+ (1 | hosp.id)"))

# patient and treatment level predictors reduce individual level residual variation
# hence the ICC appears to improve

print(f)
# Model
m <- lmer(f, data=tdt)
display(m)
coef.plot(m) 
ICC(m)

# Prepare table to save
m.table <- data.table(summary(m)$coefficients, keep.rownames=TRUE)
colnames(m.table)[2] <- "est"
colnames(m.table)[3] <- "se"
# - [ ] FIXME(2016-03-26): using normal as approximation of
#   t-distribution b/c don't know how to get a the degrees of freedom
#   for the t
colnames(m.table)[4] <- "t"
m.table$z <- m.table$est / m.table$se
m.table$p <- pnorm(abs(m.table$z), lower.tail=FALSE)
# Calculate confidence intervals 
m.table$ci95.l <- m.table$est - (qnorm(0.975) * m.table$se)
m.table$ci95.u <- m.table$est + (qnorm(0.975) * m.table$se)

m.table <- cbind(label=c("Intercept", m.labels), m.table)
m.table

p <- coef.plot(m) 
p <- coef.plot(m, m.labels=m.labels, coef.lim)
print(p)
ggsave(p, file=file.coefplot, width=3, height=3, scale=2)

(m.icc <- ICC(m))
(model_parameters <- list(data.frame(names(m.icc), m.icc)))


# Now write to Excel
wb <- loadWorkbook(file.table, create = TRUE)

sheet1 <- paste0("model", rx_suffix)
# if (existsSheet(wb, sheet1)) {
#   removeSheet(wb, sheet1)
# }
createSheet(wb, name = sheet1)
writeWorksheet(wb, model_parameters, sheet1)

sheet2 <- paste0("raw", rx_suffix)
# if (existsSheet(wb, sheet2)) {
#   removeSheet(wb, sheet2)
# }
createSheet(wb, name = sheet2)
writeWorksheet(wb, m.table, sheet2)

saveWorkbook(wb)
