# Prep generic vars

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

