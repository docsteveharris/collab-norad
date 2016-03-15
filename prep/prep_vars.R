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

wdt[,sepsis.site:=relevel(factor(sepsis.site), "Other")]
table(wdt$sepsis.site)

# Patient vars
wdt[, bmi:=weight/(height/100)^2]
describe(wdt$bmi)

# Construct sofa.1 without norad points
require(plyr)
wdt[, sofa.1.cvs := 2 + cut(ne.1, c(0,0.1,10), labels=FALSE, right=FALSE)]
describe(wdt$sofa.1.cvs)
wdt[, sofa.1.nocvs := sofa.1 - sofa.1.cvs]


