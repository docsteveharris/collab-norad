# Prep generic vars

# Encode the hospitals
(dt.hosp <- data.table(hosp=unique(wdt$hosp), hosp.id=rnorm(8)))
setorder(dt.hosp,hosp.id)
dt.hosp[,hosp.id := .I]
dt.hosp
setkey(dt.hosp, hosp)
setkey(wdt, hosp)
wdt <- dt.hosp[wdt]
wdt[, hosp.id:=factor(hosp.id)]
setkey(wdt,id)
str(wdt)

# Clean up sepsis site
table(wdt$sepsis.site)
wdt[, sepsis.site := ifelse(sepsis.site == "Acute abdo infection", "Abdominal", sepsis.site)]
wdt[, sepsis.site := ifelse(sepsis.site == "Pneumonia", "Respiratory", sepsis.site)]
wdt[, sepsis.site := ifelse(sepsis.site == "Genito-Urinary", "Genito-urinary", sepsis.site)]
wdt[, sepsis.site := ifelse(sepsis.site == "Urinary Tract", "Genito-urinary", sepsis.site)]
wdt[, sepsis.site := ifelse(sepsis.site %in% c(
						"Abdominal",
						"Respiratory",
						"Genito-urinary"
						), sepsis.site, "Other")]

table(wdt$sepsis.site)

# Patient vars
wdt[, bmi:=weight/(height/100)^2]
describe(wdt$bmi)

