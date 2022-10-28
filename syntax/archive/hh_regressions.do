** Liz Bageant
** October 27, 2022

clear all
set more off
version 15


/*------------------------------------------------------------------------------
	Household characteristics. Do they attenuate biodiversity filtering?
------------------------------------------------------------------------------*/


/* -------- Assemble data ----------------------------------------------------*/

* CFR level biodiversity in terms of species counts (cfr_biom in R)
use "$processed/biom_formb.dta",clear
keep cfrid scode_biom
duplicates drop 
collapse (count) scode_biom, by (cfrid)
ren scode_biom biom_species
la var biom_species "Number of species in system (biom)"
save "$component/cfr_biom", replace

* Household level catch biodiversity in terms of species counts (hh_catch in R)
use "$processed/ccm_q7.dta", clear
count if catch_iweight== 0 // check. all ok.
keep hhid cfrid scode_ccm
duplicates drop
collapse (count) scode_ccm, by(hhid)
ren scode_ccm catch_species
la var catch_species "Number of species caught by HH (ccm)"
save "$component/hh_catch", replace

* Household level consumption and sale biodiveristy in terms of species counts (hh_soldcons in R)
use "$processed/ccm_q7.dta", clear
keep hhid cfrid scode_ccm atefresh
keep if atefresh == 1
duplicates drop
count // 9032
collapse (count) scode_ccm, by(hhid)
ren scode_ccm consumption_species
la var consumption_species "Number of species consumed by HH (ccm)"
save "$component/hh_cons", replace

use "$processed/ccm_q7.dta", clear
keep hhid cfrid scode_ccm soldfresh
keep if soldfresh == 1
duplicates drop
count // 1879
collapse (count) scode_ccm, by(hhid)
ren scode_ccm sold_species
la var sold_species "Number of species sold by HH (ccm)"
save "$component/hh_sold", replace

merge 1:1 hhid using "$component/hh_cons"
recode sold_species (. = 0) // these are cases where a household did not sell anything over the period
drop _mer
save "$component/hh_soldcons", replace

* Combine above files to create the equivalent of cfr_biodiv R file (name is misleading--file is at HH level)
cd "$component"

use hh_soldcons, clear
merge 1:1 hhid using hh_catch
drop _mer
merge 1:1 hhid using "$processed/cfr_hhid_link" // bringing in cfr codes
keep if _mer == 3 // 413 hh
drop _mer
merge m:1 cfrid using cfr_biom
drop _mer
order hhid cfrid biom catch cons sold
save "$component/cfr_biodiv", replace

* Add CFR characteristics (type and distance to city). This file has no equivalent in core R data files.

use "$component/cfr_biodiv", clear
drop cfrid
do "$do/0_cfrid_string_to_num.do"
tostring cfrid, replace
merge m:1 cfrid using "$processed/city_distance"
drop _mer
destring cfrid, replace
gen year = 2012 // livelihood data has 2012 and 2015. This facilitates the merge.
merge 1:1 hhid year using "$processed/livelihood"
keep if _mer == 3
drop year _mer

save "$processed/hh_level_stata", replace



