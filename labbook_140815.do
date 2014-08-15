*  ==============
*  = Rough work =
*  ==============

insheet using "../data/_data_in/6 centers.csv", clear
describe
save ../data/working_raw.dta, replace

* TEST: 2014-08-15 - check number of patients = 599
count
assert r(N) == 599

cap drop idhosp
gen idhosp = substr(id, 1, strpos(id,"-")-1)
list id idhosp  in 1/10
replace idhosp = lower(idhosp)
tempvar var xx
encode idhosp, gen(`xx')
drop idhosp
clonevar idhosp = `xx'
* TEST: 2014-08-15 - check six sites
tab idhosp
ret li
assert r(r) == 6


cap drop idpt
gen idpt = substr(id, strpos(id,"-")+1, .)
list id idpt  in 1/10
replace idpt = lower(idpt)
destring idpt, replace
* TEST: 2014-08-15 - check six sites

xtset idhosp

* NOTE: 2014-08-15 - rename to save typing
d ne*
rename ne_# ne#, renumber sort
d ne*

d hr*
rename hr_# hr#, renumber sort
d hr*

d syst_bp_*
rename syst_bp_# bps#, renumber sort
d bps*

d diast_bp_*
rename diast_bp_# bpd#, renumber sort
d bpd*

save ../data/working.dta, replace

xtsum ne1 ne2 bps* hr*

