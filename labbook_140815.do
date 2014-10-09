*  ==============
*  = Rough work =
*  ==============


/*
Todo
====
- data validation issues to return to Roberta and Sara to check
- work out how to handle deaths in 1st 24h

Log
===
*/

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
encode idhosp, gen(`xx') label(idhosp)
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

order id*, first
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

d map_*
rename map_# map#, renumber sort
d map*

gen deadicu = itu_mortality
label values deadicu truefalse

* NOTE: 2014-08-15 - labels
cap label drop truefalse
label define truefalse 0 "False" 1 "True"
rename gender male
label values male truefalse


* Validation checks and cleaing
* TODO: 2014-08-15 - review these with Roberta and Sara
replace bps1 = . if bps1 > 300
destring map1, replace force
// 94 hr_2 below 10 , and some text data in field too
destring hr2, replace force

save ../data/working.dta, replace
use ../data/working.dta, clear
set scheme shred

* xtsum ne1 ne2 bps* hr* map*
xtreg bps2
xtreg ne2
xtreg hr2


