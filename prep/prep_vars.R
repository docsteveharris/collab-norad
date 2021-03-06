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

wdt[, vadi.other.24 := 
        ifelse(ne.24 > 0.3, vadi.24 - 3,
        ifelse(ne.24 > 0.1, vadi.24 - 2,
        ifelse(ne.24 > 0.0, vadi.24 - 1, vadi.24)))]
wdt[, vadi.other.24.logical := ifelse(vadi.other.24 > 0, 1, 0)]

#  ========================================================
#  = define low/high blood pressure and heart rate groups =
#  ========================================================
wdt[,map.high := ifelse(map.24 >= 75, 1, 0 )]
wdt[,hr.high := ifelse(hr.24 >= 95, 1, 0 )]
with(wdt, table(map.high, hr.high))
