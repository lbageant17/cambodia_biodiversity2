** Liz Bageant
** October 27, 2022

clear all
set more off
version 15


/*------------------------------------------------------------------------------
	This file creates a single household-level stata data file containing all 
	key variables that were generated in R.
------------------------------------------------------------------------------*/



/* -------- Species count data -----------------------------------------------*/


import delimited "$processed/species_hh_level.csv", clear 
drop v1
la var catch_species "Number of species caught by households"
la var consumption_species "Number of species consumed by households"
ren cfr cfr_species 
la var cfr_species "Number of species in system (CFR)"
la var sold_species "Number of species sold"
save "$component/species", replace

/* -------- Biodiversity indices data ----------------------------------------*/

* CFR and household level diversity indices were generated in R
/*import delimited "$processed/diversity_indices_cfr_level.csv", clear 
drop v1
foreach var of varlist h_* d1_* d2_* {
	rename `var' `var'_cfr_level
	}
la var h_biom "Shannon index--system CFR level"
la var d1_biom "Simpson index--system CFR level"
la var d2_biom "Inverse simpson index--system CFR level"
la var h_catch "Shannon index--catch CFR level"
la var d1_catch "Simpson index--catch CFR level"
la var d2_catch "Inverse simpson index--catch CFR level"
la var h_cons "Shannon index--consumption CFR level"
la var d1_cons "Simpson index--consumption CFR level"
la var d2_cons "Inverse simpson index--consumption CFR level"
la var h_sold "Shannon index--sold CFR level"
la var d1_sold "Simpson index--sold CFR level"
la var d2_sold "Inverse simpson index--sold CFR level"
	
save "$component/cfr_shannon_index", replace */


import delimited "$processed/diversity_indices_hh_level.csv", clear 
drop v1
la var h_catch "Shannon index--catch"
la var d1_catch "Simpson index--catch"
la var d2_catch "Inverse simpson index--catch"
la var h_cons "Shannon index--consumption"
la var d1_cons "Simpson index--consumption"
la var d2_cons "Inverse simpson index--consumption"
la var h_sold "Shannon index--sold"
la var d1_sold "Simpson index--sold"
la var d2_sold "Inverse simpson index--sold"

ren h_biom h_cfr
ren d1_biom d1_cfr
ren d2_biom d2_cfr
la var h_cfr "Shannon index--CFR"
la var d1_cfr "Simpson index--CFR"
la var d2_cfr "Inverse simpson index--CFR"

save "$component/hh_div_index", replace

/* -------- Commonness index -----------------------------------------------*/
import delimited "$processed/commonness_hh_level.csv", clear
drop v1
foreach var of varlist cfr catch cons sold {
	ren `var' com_`var'
	la var com_`var' "Commonness index: `var' level"
	}
save "$component/commonness", replace

/* -------- Body size -----------------------------------------------*/
import delimited "$processed/body_size_hh_level.csv", clear
drop v1
foreach var of varlist cfr catch cons sold {
	ren `var' body_size_`var'
	la var body_size_`var' "Body size: `var' level"
	}
save "$component/body_size", replace


/* -------- Diet quality metrics ---------------------------------------------*/

* Diet quality metrics were generated in R
import delimited "$processed/nd_score_hh_level.csv", clear
drop v1
foreach var of varlist catch cons sold cfr {
	ren `var' nd_score_`var'
	la var nd_score_`var' "Nutrient density score: `var' level"
	}
save "$component/diet_quality", replace

/* -------- CFR and household characteristics data ---------------------------*/

* Add CFR characteristics (type and distance to city) and household characteristics from livelihood data. This file has no equivalent in core R data files.

import delimited "$processed/cfrid_to_hhid.csv", clear 
drop v1
merge m:1 cfrid using "$processed/cfr_category" 
drop _mer
merge 1:m hhid using "$processed/livelihood"
keep if year == 2012
keep if _mer == 3
drop year _mer
count // 413

la var city_distance "Distance to nearest large town (km)--Battambang, Kampong Thom, Pursat, Siem Reap"
la def category 1 "Reservoir for irrigation in upland area" 2 "Community pond within agricultural land--not flood prone" 3 "Community pond within agricultural land--flood prone" 4 "Demarcated area in larger water body"
la val category category
la var category "CFR type"

save "$component/hh", replace

/* -------- Effort data ------------------------------------------------------*/
use "$processed/effort",clear
collapse (sum) effort, by(hhid)
la var effort "Person-days"
ren effort_persondays effort
gen effort2 = effort*effort
la var effort2 "Effort-squared"
save "$component/effort", replace

/* -------- Combine all HH level data files ----------------------------------*/

use "$component/hh", clear
merge 1:1 hhid using "$component/species"
drop _mer
merge 1:1 hhid using "$component/commonness"
drop _mer
merge 1:1 hhid using "$component/body_size"
drop _mer
merge 1:1 hhid using "$component/hh_div_index"
drop _mer
mer 1:1 hhid using "$component/diet_quality"
drop _mer
mer 1:1 hhid using "$component/effort"
keep if _mer == 3
drop _mer
mer 1:1 hhid using "$component/index1" // generated by asset_index.do
	/*

		Result                           # of obs.
		-----------------------------------------
		not matched                           228
			from master                         3  (_merge==1) <-- Note: hhid 566, 630 and 634 are missing asset info. 
			from using                        225  (_merge==2) <-- these are households that were not included in CCM survey, so they will be dropped from this analysis.

		matched                               410  (_merge==3)
		-----------------------------------------
	*/

keep if _mer == 3 // this reduces our N to 410. 
drop _mer
mer 1:1 hhid using "$component/index2" // generated by asset_index.do
keep if _mer == 3
drop _mer

/* -------- Address NA values ------------------------------------------------*/

desc *_sold

foreach var of varlist body_size_sold com_sold {
	replace `var' = "0" if `var' == "NA"
	destring `var', replace
	}

	
order hhid cfrid *_species h_* nd_score* body_size* com_* d1_* d2_* effort*

save "$processed/hh_level_stata", replace



